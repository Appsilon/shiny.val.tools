#' Compute the upstream closure of a root node in a graph.
#'
#' Returns the set of node ids reachable from `root_id` by walking the
#' edges in their reverse direction — i.e. for each visited node `n`,
#' include every `target_id` of an edge whose `source_id == n`. Edges
#' encode `source depends_on target`, so following them outward yields
#' the upstream subgraph.
#'
#' Walking stops naturally at nodes with no outgoing edges. The
#' canonical subgraph terminals are: input nodes, top-level
#' reactiveValues entries, and module-instance nodes (opaque per the
#' module-contract design).
#'
#' @noRd
upstream_closure <- function(root_id, nodes, edges) {
  visited <- character()
  frontier <- root_id

  while (length(frontier)) {
    visited <- unique(c(visited, frontier))
    if (!nrow(edges)) break
    next_frontier <- edges$target_id[edges$source_id %in% frontier]
    next_frontier <- setdiff(unique(next_frontier), visited)
    frontier <- next_frontier
  }

  visited
}

#' Discover the default roots for slicing.
#'
#' One feature per **top-level** output and per **top-level**
#' side-effecting observer. `top-level` means namespace is NA — module-
#' internal outputs/observers belong to module subgraphs, not to the
#' parent app's features.
#'
#' Module-instance nodes are not feature roots. They are opaque from a
#' parent's view; if the parent reads from them, the parent feature's
#' subgraph will include the instance as a terminal.
#'
#' @noRd
default_feature_roots <- function(nodes) {
  if (!nrow(nodes)) {
    return(tibble::tibble(
      name = character(), root_id = character(),
      type = character()
    ))
  }
  is_top <- is.na(nodes$namespace) | !nzchar(nodes$namespace)
  is_root <- is_top & nodes$type %in% c("output", "observer")
  rows <- nodes[is_root, , drop = FALSE]
  if (!nrow(rows)) {
    return(tibble::tibble(
      name = character(), root_id = character(),
      type = character()
    ))
  }
  tibble::tibble(
    name = rows$name,
    root_id = rows$id,
    type = rows$type
  )
}

#' Slice a graph into one feature subgraph per default root.
#'
#' Implements the default slicing rule. For each top-level output and
#' top-level observer, builds:
#'   - `name`           — the bare output/observer name
#'   - `roots`          — character vector of root node ids
#'   - `node_ids`       — transitive upstream closure
#'   - `edge_ids`       — row indexes of edges entirely inside the closure
#'   - `intended_use`   — NA (filled by manifest merge later)
#'   - `risk_classification`, `rationale`
#'   - `package_categories`, `reviewers`
#'   - `kind`           — "feature" (vs "module" for module subgraphs)
#'
#' Returns a list of feature records. Order is the order of the underlying
#' nodes table — deterministic given the same source.
#'
#' @noRd
default_slice <- function(graph) {
  nodes <- graph$nodes
  edges <- graph$edges
  roots_tbl <- default_feature_roots(nodes)
  if (!nrow(roots_tbl)) return(list())

  features <- vector("list", nrow(roots_tbl))
  for (i in seq_len(nrow(roots_tbl))) {
    root_id <- roots_tbl$root_id[i]
    closure <- upstream_closure(root_id, nodes, edges)
    edge_idx <- if (nrow(edges)) {
      which(edges$source_id %in% closure & edges$target_id %in% closure)
    } else {
      integer()
    }
    features[[i]] <- new_feature_record(
      name = roots_tbl$name[i],
      kind = "feature",
      roots = root_id,
      node_ids = closure,
      edge_ids = edge_idx
    )
  }
  features
}

#' Construct a feature record with the canonical shape.
#'
#' Auto-filled fields are populated by the slicer; manifest-derived
#' fields default to NA / empty so a manifest merge can layer on top
#' without re-deriving the structural pieces.
#'
#' @noRd
new_feature_record <- function(name,
                               kind = "feature",
                               roots = character(),
                               node_ids = character(),
                               edge_ids = integer(),
                               intended_use = NA_character_,
                               risk_classification = NA_character_,
                               rationale = NA_character_,
                               package_categories = list(),
                               reviewers = list(),
                               warnings = character()) {
  list(
    name = name,
    kind = kind,
    roots = roots,
    node_ids = node_ids,
    edge_ids = edge_ids,
    intended_use = intended_use,
    risk_classification = risk_classification,
    rationale = rationale,
    package_categories = package_categories,
    reviewers = reviewers,
    warnings = warnings
  )
}

#' Bundle the graph tables into a single list.
#'
#' Used by the slicer and the inventory layer. v1 builds tables on
#' demand from disk — this helper just shapes them into one object
#' carrying `files`, `imports`, `sources`, `nodes`, `edges`, `warnings`.
#'
#' @noRd
build_graph <- function(app_path) {
  list(
    files = build_files_table(app_path),
    imports = build_imports_table(app_path),
    sources = build_sources_table(app_path),
    nodes = build_nodes_table(app_path),
    edges = build_edges_table(app_path),
    warnings = build_warnings_table(app_path)
  )
}

#' Minimal Files table builder.
#'
#' v1 surfaces just the enumerated file roster as the canonical column;
#' parse status and richer metadata are reserved for future work. Other
#' consumers (slicer, inventory) only need the file list today.
#'
#' @noRd
build_files_table <- function(app_path) {
  files <- enumerate_app_files(app_path)
  tibble::tibble(file = files)
}
