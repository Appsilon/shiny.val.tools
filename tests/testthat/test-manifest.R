test_that("read_manifest returns an empty manifest when path is NULL or missing", {
  # `report` (spec 07) is carried alongside features/modules and is NULL
  # when the manifest does not declare a `report:` block.
  expect_equal(read_manifest(NULL),
               list(features = list(), modules = list(), report = NULL))
  expect_equal(read_manifest(tempfile()),
               list(features = list(), modules = list(), report = NULL))
})

test_that("read_manifest parses a valid features.yml", {
  tmp <- tempfile(fileext = ".yml")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c(
    "features:",
    "  - name: doubled",
    "    intended_use: |",
    "      Compute the doubled value.",
    "    risk_classification: low",
    "    roots:",
    "      - output: doubled",
    "modules: []"
  ), tmp)
  m <- read_manifest(tmp)
  expect_length(m$features, 1L)
  expect_equal(m$features[[1L]]$name, "doubled")
  expect_equal(m$features[[1L]]$risk_classification, "low")
  expect_equal(m$features[[1L]]$roots[[1L]]$output, "doubled")
})

test_that("validate_manifest emits SVT-W101 for an unknown root", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")
  graph <- build_graph(app_path)
  manifest <- list(
    features = list(list(
      name = "ghost",
      intended_use = "x", risk_classification = "low",
      roots = list(list(output = "no_such_output"))
    )),
    modules = list()
  )
  issues <- validate_manifest(manifest, graph)
  expect_true("SVT-W101" %in% issues$code)
})

test_that("validate_manifest emits SVT-W102 for two features claiming the same root", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")
  graph <- build_graph(app_path)
  manifest <- list(
    features = list(
      list(name = "a", intended_use = "x", risk_classification = "low",
           roots = list(list(output = "doubled"))),
      list(name = "b", intended_use = "x", risk_classification = "low",
           roots = list(list(output = "doubled")))
    ),
    modules = list()
  )
  issues <- validate_manifest(manifest, graph)
  expect_true("SVT-W102" %in% issues$code)
})

test_that("validate_manifest emits SVT-W105 for not_required without rationale", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")
  graph <- build_graph(app_path)
  manifest <- list(
    features = list(list(
      name = "doubled",
      intended_use = "x",
      risk_classification = "not_required",
      roots = list(list(output = "doubled"))
    )),
    modules = list()
  )
  issues <- validate_manifest(manifest, graph)
  expect_true("SVT-W105" %in% issues$code)
})

test_that("validate_manifest accepts not_required when rationale is present", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")
  graph <- build_graph(app_path)
  manifest <- list(
    features = list(list(
      name = "doubled",
      intended_use = "x",
      risk_classification = "not_required",
      rationale = "Display-only feature, no computation",
      roots = list(list(output = "doubled"))
    )),
    modules = list()
  )
  issues <- validate_manifest(manifest, graph)
  expect_false("SVT-W105" %in% issues$code)
})

test_that("validate_manifest rejects collisions across features and modules", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")
  graph <- build_graph(app_path)
  manifest <- list(
    features = list(list(name = "shared", intended_use = "x",
                         risk_classification = "low",
                         roots = list(list(output = "doubled")))),
    modules = list(list(name = "shared", intended_use = "y",
                        risk_classification = "low"))
  )
  issues <- validate_manifest(manifest, graph)
  expect_true("manifest_name_collision" %in% issues$code)
})

test_that("apply_manifest produces a feature with manifest metadata merged in", {
  app_path <- system.file("extdata", "traditional_basic", package = "shiny.val.tools")
  graph <- build_graph(app_path)
  manifest <- list(
    features = list(list(
      name = "primary_doubled",
      intended_use = "Display the doubled value to the user.",
      risk_classification = "low",
      roots = list(list(output = "doubled")),
      package_categories = list(shiny = "framework")
    )),
    modules = list()
  )
  features <- apply_manifest(manifest, graph)

  m <- Filter(function(f) f$name == "primary_doubled", features)[[1L]]
  expect_equal(m$intended_use, "Display the doubled value to the user.")
  expect_equal(m$risk_classification, "low")
  expect_equal(m$package_categories$shiny, "framework")
  # The default-sliced 'doubled' feature is dropped because the
  # manifest claimed its root.
  expect_false("doubled" %in% vapply(features, function(f) f$name, character(1)))
})

test_that("apply_manifest leaves unclaimed default features intact", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines("library(shiny)", file.path(tmp, "ui.R"))
  writeLines(c(
    "function(input, output, session) {",
    "  output$a <- renderText({ input$x })",
    "  output$b <- renderText({ input$y })",
    "}"
  ), file.path(tmp, "server.R"))

  graph <- build_graph(tmp)
  manifest <- list(
    features = list(list(name = "feat_a",
                         intended_use = "...", risk_classification = "low",
                         roots = list(list(output = "a")))),
    modules = list()
  )
  features <- apply_manifest(manifest, graph)
  names_v <- vapply(features, function(f) f$name, character(1))
  expect_setequal(names_v, c("feat_a", "b"))
})

test_that("manifest_template generates a starter YAML covering every default root", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines("library(shiny)", file.path(tmp, "ui.R"))
  writeLines(c(
    "function(input, output, session) {",
    "  output$a <- renderText({ input$x })",
    "  output$b <- renderText({ input$y })",
    "}"
  ), file.path(tmp, "server.R"))

  graph <- build_graph(tmp)
  yml <- manifest_template(graph)

  expect_true(grepl("name: a", yml, fixed = TRUE))
  expect_true(grepl("name: b", yml, fixed = TRUE))
  expect_true(grepl("output: a", yml, fixed = TRUE))
})
