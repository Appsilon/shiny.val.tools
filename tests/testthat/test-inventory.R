test_that("resolve_function_origins handles pkg::fn explicitly", {
  refs <- tibble::tibble(
    from_file = "server.R", kind = "call", name = "filter",
    namespace = NA_character_, container = NA_character_, package = "dplyr",
    internal = FALSE,
    in_def_kind = "output", in_def_name = "z", in_def_namespace = NA_character_,
    line = 3L, col = 5L
  )
  imports <- tibble::tibble(
    from_file = character(), kind = character(), package = character(),
    alias = character(), function_set = list(),
    whole_namespace = logical(), local_path = character(),
    absolute = logical(), conditional = logical(),
    inside_function = logical(),
    line = integer(), col = integer()
  )

  resolved <- resolve_function_origins(refs, imports)

  expect_equal(nrow(resolved), 1L)
  expect_equal(resolved$package, "dplyr")
  expect_equal(resolved$resolution, "explicit")
  expect_equal(resolved$fq_name, "dplyr::filter")
  expect_equal(resolved$warnings[[1]], character())
})

test_that("resolve_function_origins emits SVT-W201 for pkg:::fn access", {
  refs <- tibble::tibble(
    from_file = "server.R", kind = "call", name = "internal_helper",
    namespace = NA_character_, container = NA_character_, package = "dplyr",
    internal = TRUE,
    in_def_kind = "output", in_def_name = "z", in_def_namespace = NA_character_,
    line = 4L, col = 5L
  )
  imports <- tibble::tibble(
    from_file = character(), kind = character(), package = character(),
    alias = character(), function_set = list(),
    whole_namespace = logical(), local_path = character(),
    absolute = logical(), conditional = logical(),
    inside_function = logical(),
    line = integer(), col = integer()
  )

  resolved <- resolve_function_origins(refs, imports)
  expect_equal(resolved$warnings[[1]], "SVT-W201")
})

test_that("resolve_function_origins resolves bare-name calls via box::use(pkg[fn])", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines(c(
    "box::use(",
    "  dplyr[filter, mutate],",
    ")",
    "function(input, output, session) {",
    "  output$z <- shiny::renderText({ filter(df, x > 1) })",
    "}"
  ), file.path(tmp, "app.R"))

  refs <- build_references_table(tmp)
  imports <- build_imports_table(tmp)
  resolved <- resolve_function_origins(refs, imports)

  filt <- resolved[resolved$name == "filter", ]
  expect_equal(nrow(filt), 1L)
  expect_equal(filt$package, "dplyr")
  expect_equal(filt$resolution, "box_declared")
})

test_that("resolve_function_origins resolves alias$fn via box::use(alias = pkg)", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines(c(
    "box::use(",
    "  log = logger,",
    ")",
    "function(input, output, session) {",
    "  output$z <- shiny::renderText({ log$info('ok') })",
    "}"
  ), file.path(tmp, "app.R"))

  refs <- build_references_table(tmp)
  imports <- build_imports_table(tmp)
  resolved <- resolve_function_origins(refs, imports)

  hit <- resolved[resolved$name == "info" & !is.na(resolved$container), ]
  expect_equal(nrow(hit), 1L)
  expect_equal(hit$package, "logger")
  expect_equal(hit$resolution, "box_declared")
})

test_that("resolve_function_origins resolves bare-name calls via library walk", {
  # `tools` is base-recommended and always available; `toRd` is exported.
  refs <- tibble::tibble(
    from_file = "app.R", kind = "call", name = "toRd",
    namespace = NA_character_, container = NA_character_,
    package = NA_character_, internal = FALSE,
    in_def_kind = "output", in_def_name = "z", in_def_namespace = NA_character_,
    line = 2L, col = 5L
  )
  imports <- tibble::tibble(
    from_file = "app.R", kind = "library", package = "tools",
    alias = NA_character_, function_set = list(character()),
    whole_namespace = FALSE, local_path = NA_character_,
    absolute = NA, conditional = FALSE,
    inside_function = FALSE,
    line = 1L, col = 1L
  )

  resolved <- resolve_function_origins(refs, imports)
  expect_equal(resolved$package, "tools")
  expect_equal(resolved$resolution, "library_walk")
})

test_that("resolve_function_origins flags unresolved bare calls as SVT-W203", {
  refs <- tibble::tibble(
    from_file = "app.R", kind = "call", name = "made_up_fn",
    namespace = NA_character_, container = NA_character_,
    package = NA_character_, internal = FALSE,
    in_def_kind = "output", in_def_name = "z", in_def_namespace = NA_character_,
    line = 2L, col = 5L
  )
  imports <- tibble::tibble(
    from_file = character(), kind = character(), package = character(),
    alias = character(), function_set = list(),
    whole_namespace = logical(), local_path = character(),
    absolute = logical(), conditional = logical(),
    inside_function = logical(),
    line = integer(), col = integer()
  )

  resolved <- resolve_function_origins(refs, imports)
  expect_equal(resolved$resolution, "unresolved")
  expect_equal(resolved$warnings[[1]], "SVT-W203")
})

test_that("direct_packages collects library, box::use, and DESCRIPTION pkgs", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines(c(
    "library(shiny)",
    "box::use(dplyr[filter])"
  ), file.path(tmp, "app.R"))
  writeLines(c(
    "Package: dummy",
    "Imports: ggplot2, tidyr (>= 1.0.0)",
    "Depends: R (>= 4.0)"
  ), file.path(tmp, "DESCRIPTION"))

  imports <- build_imports_table(tmp)
  pkgs <- direct_packages(imports, app_path = tmp)

  expect_true(all(c("shiny", "dplyr", "ggplot2", "tidyr") %in% pkgs))
  expect_false("R" %in% pkgs)
})

test_that("build_inventory produces per-feature FunctionCall + PackageUse views", {
  app_path <- system.file("extdata", "traditional_basic",
                          package = "shiny.val.tools")
  graph <- build_graph(app_path)
  features <- default_slice(graph)
  inv <- build_inventory(graph, features, app_path)

  expect_true("doubled" %in% names(inv))
  doubled <- inv$doubled
  expect_s3_class(doubled$functions, "tbl_df")
  expect_s3_class(doubled$packages, "tbl_df")
  expect_true("call_sites" %in% colnames(doubled$functions))
})

test_that("collate_function_calls bundles multiple sites under one (pkg, fn) row", {
  per_site <- tibble::tibble(
    package = c("dplyr", "dplyr"),
    fn = c("filter", "filter"),
    fq_name = c("dplyr::filter", "dplyr::filter"),
    file = c("server.R", "server.R"),
    line = c(3L, 7L), col = c(5L, 5L),
    resolution = c("explicit", "explicit"),
    direct = c(TRUE, TRUE),
    category = c("unset", "unset"),
    warnings = list(character(), character())
  )

  fc <- collate_function_calls(per_site)
  expect_equal(nrow(fc), 1L)
  expect_equal(nrow(fc$call_sites[[1]]), 2L)
  expect_equal(fc$call_sites[[1]]$line, c(3L, 7L))
})

test_that("render_inventory_json emits stable schema_version 1.0", {
  inv <- list(
    feature = "feat1",
    kind = "feature",
    functions = tibble::tibble(
      package = "dplyr", fn = "filter", fq_name = "dplyr::filter",
      call_sites = list(tibble::tibble(file = "server.R",
                                       line = 3L, col = 5L)),
      resolution = "explicit", direct = TRUE, category = "framework",
      warnings = list(character())
    ),
    packages = tibble::tibble(
      package = "dplyr", functions = list("filter"), direct = TRUE,
      category = "framework", call_count = 1L
    ),
    package_versions = c(dplyr = "1.1.4"),
    warnings = empty_warnings()
  )

  json <- render_inventory_json(inv)

  expect_match(json, "\"schema_version\":\"1.0\"", fixed = TRUE)
  expect_match(json, "\"feature\":\"feat1\"", fixed = TRUE)
  expect_match(json, "\"package\":\"dplyr\"", fixed = TRUE)
  expect_match(json, "\"function\":\"filter\"", fixed = TRUE)
  expect_match(json, "\"call_sites\":[{\"file\":\"server.R\"",
               fixed = TRUE)
  expect_match(json, "\"package_versions\":\\{\"dplyr\":\"1.1.4\"\\}")
})

test_that("write_inventory_json writes <out_dir>/<feature>/inventory.json", {
  inv <- list(
    feature = "feat1", kind = "feature",
    functions = empty_function_calls(),
    packages = tibble::tibble(
      package = character(), functions = list(), direct = logical(),
      category = character(), call_count = integer()
    ),
    package_versions = character(),
    warnings = empty_warnings()
  )
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  path <- write_inventory_json(inv, tmp)
  expect_true(file.exists(path))
  expect_equal(basename(path), "inventory.json")
  expect_equal(basename(dirname(path)), "feat1")
})

test_that("SVT-W204 fires when a resolved function's package is transitive", {
  # Set up: library(shiny). Resolve a call to `tibble` which lives in the
  # `tibble` package — that's not directly imported, so SVT-W204 fires.
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines(c(
    "library(tibble)",
    "function(input, output, session) {",
    "  output$z <- shiny::renderText({ tibble::tibble(x = 1) })",
    "}"
  ), file.path(tmp, "app.R"))

  graph <- build_graph(tmp)
  features <- default_slice(graph)
  inv <- build_inventory(graph, features, tmp)

  doubled <- inv[[features[[1]]$name]]
  shiny_warns <- doubled$warnings[doubled$warnings$code == "SVT-W204", ]
  # `shiny::renderText` resolves to shiny — direct = FALSE since shiny isn't
  # in this app's imports.
  expect_true(nrow(shiny_warns) >= 1L)
})
