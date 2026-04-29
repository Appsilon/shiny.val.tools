test_that("box-aliased $server calls produce module_instance nodes (rhino pattern)", {
  app_path <- materialize_fixture_with_dotfiles("rhino_multi_module")
  graph <- build_graph(app_path)

  inst <- graph$nodes[graph$nodes$type == "module_instance", , drop = FALSE]
  expect_true(nrow(inst) >= 2L)
  expect_setequal(inst$name, c("app/view/mod_a", "app/view/mod_b"))
  expect_true(all(inst$namespace == "app/main"))
  expect_match(inst$fq_name, "\\$server\\[", all = TRUE)
})

test_that("file_module_aliases maps box::use bindings to module identities", {
  imports <- tibble::tibble(
    from_file = c("app/main.R", "app/main.R", "app/main.R"),
    kind = c("box_use", "box_use", "box_use"),
    package = c(NA_character_, NA_character_, "shiny"),
    alias = c(NA_character_, "mb", NA_character_),
    function_set = list(character(), character(), character()),
    whole_namespace = c(FALSE, FALSE, FALSE),
    local_path = c("app/view/mod_a", "app/view/mod_b", NA_character_),
    absolute = c(TRUE, TRUE, NA),
    conditional = c(FALSE, FALSE, FALSE),
    inside_function = c(FALSE, FALSE, FALSE),
    line = c(1L, 2L, 3L), col = c(1L, 1L, 1L)
  )
  identities <- c("app/view/mod_a", "app/view/mod_b")
  m <- file_module_aliases(imports, "app/main.R", identities)

  expect_equal(unname(m[["mod_a"]]), "app/view/mod_a")
  expect_equal(unname(m[["mb"]]), "app/view/mod_b")
})

test_that("file_module_aliases skips imports that are not module identities", {
  imports <- tibble::tibble(
    from_file = "app/main.R", kind = "box_use",
    package = NA_character_, alias = NA_character_,
    function_set = list(character()), whole_namespace = FALSE,
    local_path = "app/logic/util", absolute = TRUE,
    conditional = FALSE, inside_function = FALSE, line = 1L, col = 1L
  )
  m <- file_module_aliases(imports, "app/main.R",
                            module_identities = "app/view/mod_a")
  expect_length(m, 0L)
})

test_that("rhino_multi_module produces parent->child architecture edges", {
  app_path <- materialize_fixture_with_dotfiles("rhino_multi_module")
  graph <- svt_build_graph(svt_parse(app_path))
  feats <- svt_slice(graph)

  edges <- compute_module_edges(feats$records, graph)
  expect_setequal(edges$parent, "app/main")
  expect_setequal(edges$child, c("app/view/mod_a", "app/view/mod_b"))
})

test_that("module_slice's per-module subgraph includes its module_instance nodes", {
  app_path <- materialize_fixture_with_dotfiles("rhino_multi_module")
  graph <- build_graph(app_path)
  modules <- module_slice(graph, app_path)
  main_rec <- Filter(function(r) r$name == "app/main", modules)[[1L]]

  inst_ids <- graph$nodes$id[graph$nodes$type == "module_instance" &
                                graph$nodes$namespace == "app/main"]
  expect_true(all(inst_ids %in% main_rec$node_ids))
})

test_that("find_module_instances emits one record per wrapper call site with a string id", {
  parsed <- parse(text = "
    function(input, output, session) {
      counter_server('c1')
      counter_server('c2')
    }
  ", keep.source = TRUE)

  records <- find_module_instances(parsed, wrapper_names = c("counter_server"))

  expect_length(records, 2)
  expect_equal(vapply(records, function(r) r$wrapper, character(1)),
               c("counter_server", "counter_server"))
  expect_equal(vapply(records, function(r) r$ns_id, character(1)),
               c("c1", "c2"))
})

test_that("find_module_instances skips calls whose id is not a string literal", {
  parsed <- parse(text = "
    function(input, output, session) {
      counter_server(input$which)
      counter_server(paste0('c', i))
    }
  ", keep.source = TRUE)

  records <- find_module_instances(parsed, wrapper_names = c("counter_server"))
  expect_length(records, 0)
})

test_that("find_module_instances ignores non-wrapper calls", {
  parsed <- parse(text = "
    function(input, output, session) {
      paste('a', 'b')
      counter_server('c1')
    }
  ", keep.source = TRUE)

  records <- find_module_instances(parsed, wrapper_names = c("counter_server"))
  expect_length(records, 1)
  expect_equal(records[[1]]$wrapper, "counter_server")
})

test_that("build_nodes_table emits module_instance nodes per wrapper call", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines("library(shiny)", file.path(tmp, "ui.R"))
  writeLines(c(
    "counter_server <- function(id) {",
    "  moduleServer(id, function(input, output, session) {",
    "    output$count <- renderText({ input$x })",
    "  })",
    "}",
    "function(input, output, session) {",
    "  counter_server('c1')",
    "  counter_server('c2')",
    "}"
  ), file.path(tmp, "server.R"))

  nodes <- build_nodes_table(tmp)

  inst <- nodes[nodes$type == "module_instance", ]
  expect_equal(nrow(inst), 2L)
  # `name` is the target module's identity (file-path-derived); the
  # wrapper-binding label lives in `fq_name`.
  expect_setequal(inst$name, c("server", "server"))
  expect_setequal(inst$fq_name,
                  c("counter_server[c1]", "counter_server[c2]"))
  # The instance node's `container` slot carries the concrete ns_id —
  # makes (type, ns, container, name) keys disambiguate call sites.
  expect_setequal(inst$container, c("c1", "c2"))
  # Each instance has a distinct id.
  expect_equal(length(unique(inst$id)), 2L)
})

test_that("module_instance nodes are namespaced by the parent (NA at top level)", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines("library(shiny)", file.path(tmp, "ui.R"))
  writeLines(c(
    "counter_server <- function(id) {",
    "  moduleServer(id, function(input, output, session) {",
    "    output$count <- renderText({ input$x })",
    "  })",
    "}",
    "function(input, output, session) {",
    "  counter_server('c1')",
    "}"
  ), file.path(tmp, "server.R"))

  nodes <- build_nodes_table(tmp)
  inst <- nodes[nodes$type == "module_instance", ]
  expect_equal(nrow(inst), 1L)
  expect_true(is.na(inst$namespace))
  expect_equal(inst$name, "server")
  expect_equal(inst$fq_name, "counter_server[c1]")
})

test_that("module_instance nodes inside another module are namespaced by the outer module", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  writeLines("library(shiny)", file.path(tmp, "ui.R"))
  writeLines(c(
    "inner_server <- function(id) {",
    "  moduleServer(id, function(input, output, session) {",
    "    output$y <- renderText({ input$x })",
    "  })",
    "}",
    "outer_server <- function(id) {",
    "  moduleServer(id, function(input, output, session) {",
    "    inner_server('inner1')",
    "  })",
    "}"
  ), file.path(tmp, "server.R"))

  nodes <- build_nodes_table(tmp)
  inst <- nodes[nodes$type == "module_instance", ]
  expect_equal(nrow(inst), 1L)
  # Two moduleServers in one file — identities are disambiguated with a
  # `<path>::<binding>` suffix.
  expect_equal(inst$name, "server::inner_server")
  expect_equal(inst$namespace, "server::outer_server")
  expect_equal(inst$container, "inner1")
  expect_equal(inst$fq_name, "server::outer_server/inner_server[inner1]")
})
