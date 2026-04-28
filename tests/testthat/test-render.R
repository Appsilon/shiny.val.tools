test_that("node_style_table covers every node type", {
  styles <- node_style_table()
  expect_setequal(styles$type,
                  c("input", "output", "reactive", "observer",
                    "value", "module_instance"))
  expect_equal(styles$shape[styles$type == "module_instance"],
               "doubleCircle")
  expect_equal(styles$color[styles$type == "output"], "#5BB85B")
})

test_that("choose_solver picks hierarchical for sparse graphs", {
  res <- choose_solver(n_nodes = 5, n_edges = 4)
  expect_equal(res$solver, "hierarchicalRepulsion")
})

test_that("choose_solver falls back to forceAtlas2Based for dense graphs", {
  res <- choose_solver(n_nodes = 4, n_edges = 12)
  expect_equal(res$solver, "forceAtlas2Based")
  expect_match(res$reason, "exceeds 1.5", fixed = TRUE)
})

test_that("build_vis_nodes joins type to shape/color and renders tooltips", {
  app_path <- system.file("extdata", "traditional_basic",
                          package = "shiny.val.tools")
  graph <- build_graph(app_path)
  feats <- default_slice(graph)
  nodes_df <- build_vis_nodes(feats[[1L]], graph)

  expect_true(all(c("id", "label", "shape", "color", "title") %in%
                    colnames(nodes_df)))
  expect_true(nrow(nodes_df) >= 1L)
  expect_true(all(nchar(nodes_df$shape) > 0L))
  expect_true(all(grepl("^#", nodes_df$color)))
  expect_true(any(grepl("type:", nodes_df$title, fixed = TRUE)))
})

test_that("write_feature_html writes a self-contained HTML file", {
  app_path <- system.file("extdata", "traditional_basic",
                          package = "shiny.val.tools")
  graph <- build_graph(app_path)
  feats <- default_slice(graph)

  out_dir <- tempfile()
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  path <- write_feature_html(feats[[1L]], graph, out_dir,
                             app_path = app_path)
  expect_true(file.exists(path))
  expect_true(grepl("\\.html$", path))

  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(txt, "Feature: doubled", fixed = TRUE)
  expect_match(txt, "Risk:", fixed = TRUE)
  expect_match(txt, "Doc:", fixed = TRUE)
  expect_match(txt, "Inventory:", fixed = TRUE)
  expect_match(txt, "Layout:", fixed = TRUE)
})

test_that("svt_render emits an HTML widget per record alongside doc + inventory", {
  app_path <- system.file("extdata", "traditional_basic",
                          package = "shiny.val.tools")
  graph <- svt_build_graph(svt_parse(app_path))
  feats <- svt_slice(graph)
  inv <- svt_inventory(graph, feats)

  out_dir <- tempfile()
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  result <- svt_render(feats, inv, out_dir = out_dir)

  expect_equal(length(result$widgets), length(result$doc_stubs))
  expect_true(all(file.exists(result$widgets)))
  expect_true(all(grepl("\\.html$", result$widgets)))
})

test_that("module records get module_<name>.html links via build_vis_nodes URL", {
  # A node of type module_instance carries a URL pointing at the module
  # artifact filename — `<name>.html`. The exact filename is checked here.
  feature <- list(
    name = "parent", kind = "feature",
    node_ids = c("module_instance::counter_server::counter_server"),
    edge_ids = integer()
  )
  graph <- list(
    nodes = tibble::tibble(
      id = "module_instance::counter_server::counter_server",
      type = "module_instance",
      name = "counter_server",
      namespace = NA_character_,
      container = NA_character_,
      fq_name = "counter_server",
      file = "server.R", line = 5L, col = 1L,
      warnings = list(character())
    ),
    edges = tibble::tibble(source_id = character(), target_id = character(),
                           file = character(), line = integer(),
                           col = integer())
  )
  nodes_df <- build_vis_nodes(feature, graph)
  expect_equal(nodes_df$shape, "doubleCircle")
  expect_equal(nodes_df$url, "counter_server.html")
})

test_that("build_vis_edges flips depends_on into data-flow direction", {
  feature <- list(
    name = "f", kind = "feature",
    node_ids = c("input:::x", "reactive:::r", "output:::y"),
    edge_ids = c(1L, 2L)
  )
  graph <- list(
    nodes = tibble::tibble(
      id = c("input:::x", "reactive:::r", "output:::y"),
      type = c("input", "reactive", "output"),
      name = c("x", "r", "y"),
      namespace = NA_character_, container = NA_character_,
      fq_name = c("x", "r", "y"),
      file = "server.R", line = 1L, col = 1L,
      warnings = list(character(), character(), character())
    ),
    edges = tibble::tibble(
      source_id = c("output:::y",   "reactive:::r"),
      target_id = c("reactive:::r", "input:::x"),
      file = c("server.R", "server.R"), line = 1L, col = 1L
    )
  )
  edges_df <- build_vis_edges(feature, graph)
  expect_equal(nrow(edges_df), 2L)
  expect_true(all(c("input:::x", "reactive:::r") %in% edges_df$from))
  expect_true(all(c("reactive:::r", "output:::y") %in% edges_df$to))
  expect_false(any(edges_df$from == "output:::y"))
})

test_that("write_feature_html sets full-chain hover highlight + legend", {
  app_path <- system.file("extdata", "traditional_basic",
                          package = "shiny.val.tools")
  graph <- build_graph(app_path)
  feats <- default_slice(graph)

  out_dir <- tempfile()
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  path <- write_feature_html(feats[[1L]], graph, out_dir,
                             app_path = app_path)
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(txt, "\"highlight\":\\{[^}]*\"enabled\":true",
               perl = TRUE)
  expect_match(txt, "\"algorithm\":\"hierarchical\"", fixed = TRUE)
  expect_match(txt, "\"hoverNearest\":true", fixed = TRUE)
  expect_match(txt, "\"useGroups\":true", fixed = TRUE)
  expect_match(txt, "cubicBezier", fixed = TRUE)
  expect_match(txt, "\"idselection\":\\{[^}]*\"enabled\":true",
               perl = TRUE)
})

test_that("warning badges show up in node labels", {
  feature <- list(
    name = "f", kind = "feature",
    node_ids = "output:::x", edge_ids = integer()
  )
  graph <- list(
    nodes = tibble::tibble(
      id = "output:::x", type = "output", name = "x",
      namespace = NA_character_, container = NA_character_,
      fq_name = "x", file = "server.R", line = 1L, col = 1L,
      warnings = list(c("SVT-W010"))
    ),
    edges = tibble::tibble(source_id = character(), target_id = character(),
                           file = character(), line = integer(),
                           col = integer())
  )
  nodes_df <- build_vis_nodes(feature, graph)
  expect_match(nodes_df$label, "⚠1", fixed = TRUE)
})
