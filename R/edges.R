#' Build the Edges table for an app.
#'
#' Each edge represents `source depends_on target` — the source reads from
#' the target inside its body. Edges are derived by joining References
#' (the reads) to Nodes (the definitions reachable as targets):
#'   - `kind = "input"` references → edge to the input node
#'   - `kind = "value"` references → edge to the value node (named rv access)
#'   - `kind = "call"` references → edge to a reactive/observer node when
#'     the bare-name callee matches a definition's name in scope. Calls
#'     that don't match (helper functions, base R, namespaced calls) are
#'     not graph edges; they live in the per-feature inventory.
#'
#' Multiple reads of the same `(source, target)` collapse to one edge;
#' the first occurrence (in walker order) wins for `source_loc`.
#'
#' @noRd
build_edges_table <- function(app_path) {
 svt_memoize(paste0("edges\x1f", app_path), function() {
  refs <- build_references_table(app_path)
  nodes <- build_nodes_table(app_path)

  empty <- tibble::tibble(
    source_id = character(),
    target_id = character(),
    file = character(),
    line = integer(),
    col = integer()
  )

  if (!nrow(refs) || !nrow(nodes)) return(empty)

  # Drop references that aren't inside a definition — they have no source
  # node to attach the edge to.
  in_def <- !is.na(refs$in_def_kind) & !is.na(refs$in_def_name)
  refs <- refs[in_def, , drop = FALSE]
  if (!nrow(refs)) return(empty)

  source_id_for <- function(kind, name, namespace) {
    node_id(kind, namespace, NA_character_, name)
  }

  ref_source <- vapply(seq_len(nrow(refs)), function(i) {
    source_id_for(refs$in_def_kind[i], refs$in_def_name[i],
                  refs$in_def_namespace[i])
  }, character(1))

  ref_target <- vapply(seq_len(nrow(refs)), function(i) {
    kind <- refs$kind[i]
    if (kind == "input") {
      return(node_id("input", refs$namespace[i], NA_character_, refs$name[i]))
    }
    if (kind == "value") {
      return(node_id("value", refs$namespace[i], refs$container[i], refs$name[i]))
    }
    if (kind == "call" && is.na(refs$package[i])) {
      # A bare-name call resolves against either reactive or observer
      # definitions in the same namespace. We try both; whichever exists
      # in the Nodes table is the target.
      ns <- refs$namespace[i]
      nm <- refs$name[i]
      cand <- c(
        node_id("reactive", ns, NA_character_, nm),
        node_id("observer", ns, NA_character_, nm)
      )
      hit <- cand[cand %in% nodes$id]
      if (length(hit)) return(hit[1L])
    }
    NA_character_
  }, character(1))

  keep <- !is.na(ref_target) & ref_source %in% nodes$id & ref_target %in% nodes$id
  if (!any(keep)) return(empty)

  refs <- refs[keep, , drop = FALSE]
  ref_source <- ref_source[keep]
  ref_target <- ref_target[keep]

  edge_key <- paste(ref_source, ref_target, sep = "\x1f")
  first_idx <- !duplicated(edge_key)

  tibble::tibble(
    source_id = ref_source[first_idx],
    target_id = ref_target[first_idx],
    file = refs$from_file[first_idx],
    line = as.integer(refs$line[first_idx]),
    col = as.integer(refs$col[first_idx])
  )
 })
}
