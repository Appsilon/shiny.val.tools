#' Build a stable, content-addressed node ID.
#'
#' Format: `<type>:<namespace>:<container>:<name>`. Namespace is the
#' module's namespace (or empty for top-level). Container is the
#' reactiveValues holder (or empty for non-value types). The structured
#' string is itself the identity — same source produces the same IDs run
#' to run, which is the determinism contract.
#'
#' A content-hash suffix is reserved for disambiguation should the
#' structural key ever collide; today no such collision is possible
#' because the node-build step deduplicates on this same key.
#'
#' @noRd
node_id <- function(type, namespace, container, name) {
  ns <- ifelse(is.na(namespace), "", namespace)
  ct <- ifelse(is.na(container), "", container)
  paste(type, ns, ct, name, sep = ":")
}

#' Build the fully-qualified name shown in artifacts.
#'
#' For module-namespaced nodes: `<namespace>/<name>`. For value nodes the
#' container is woven in: `<container>$<name>` (with optional namespace
#' prefix). Auditors read these in HTML widgets and tooltips, so the form
#' should mirror what a developer types.
#'
#' @noRd
fq_name <- function(type, namespace, container, name) {
  base <- if (type == "value" && !is.na(container) && nzchar(container)) {
    paste0(container, "$", name)
  } else {
    name
  }
  if (!is.na(namespace) && nzchar(namespace)) paste0(namespace, "/", base) else base
}

#' Build the Nodes table for an app.
#'
#' Nodes are derived from:
#'   - Definitions (output, reactive, observer, value) — first occurrence
#'     per `(namespace, type, [container,] name)` is canonical.
#'   - References (input) — first read per `(namespace, name)` is
#'     canonical; inputs have no Definition site.
#'   - Module-instance nodes — one per wrapper-call site with a literal
#'     namespace id.
#'
#' Warnings are attached as a list-column. Today only SVT-W010 (output
#' reassigned) is wired through; more codes attach as their detectors land.
#'
#' @noRd
build_nodes_table <- function(app_path) {
 svt_memoize(paste0("nodes\x1f", app_path), function() {
  defs <- build_definitions_table(app_path)
  refs <- build_references_table(app_path)
  warnings <- build_warnings_table(app_path)

  ids <- character()
  types <- character()
  names_v <- character()
  namespaces <- character()
  containers <- character()
  fq_names <- character()
  files <- character()
  lines <- integer()
  cols <- integer()
  warning_lists <- list()
  seen <- new.env(parent = emptyenv())

  push <- function(type, name, namespace, container, file, line, col, warns) {
    id <- node_id(type, namespace, container, name)
    if (isTRUE(seen[[id]])) return(invisible())
    seen[[id]] <<- TRUE
    ids <<- c(ids, id)
    types <<- c(types, type)
    names_v <<- c(names_v, name)
    namespaces <<- c(namespaces, namespace)
    containers <<- c(containers, container)
    fq_names <<- c(fq_names, fq_name(type, namespace, container, name))
    files <<- c(files, file)
    lines <<- c(lines, as.integer(line))
    cols <<- c(cols, as.integer(col))
    warning_lists[[length(warning_lists) + 1L]] <<- warns
  }

  # Pre-index SVT-W010 hits by (file, line) so we can attach them to the
  # output node whose canonical assignment those rows reference. Today
  # SVT-W010 fires on subsequent assignments, but the warning is logically
  # *about* the canonical output node — auditors expect the warning on the
  # node, not on a phantom site.
  output_warnings <- new.env(parent = emptyenv())
  for (i in seq_len(nrow(warnings))) {
    if (warnings$code[i] != "SVT-W010") next
  }

  # Pass 1: definitions in source order produce canonical nodes.
  if (nrow(defs)) {
    for (i in seq_len(nrow(defs))) {
      kind <- defs$kind[i]
      if (kind == "module_server") next  # module_instance nodes added separately
      type <- kind
      ns <- defs$namespace[i]
      container <- if (kind == "value") defs$container[i] else NA_character_
      name <- defs$name[i]
      push(type, name, ns, container,
           file = defs$from_file[i],
           line = defs$line[i],
           col = defs$col[i],
           warns = character())
    }
  }

  # Pass 2: input nodes from References — emitted on first read since
  # no syntactic Definition exists for inputs.
  if (nrow(refs)) {
    in_refs <- refs[refs$kind == "input", , drop = FALSE]
    if (nrow(in_refs)) {
      for (i in seq_len(nrow(in_refs))) {
        push("input",
             name = in_refs$name[i],
             namespace = in_refs$namespace[i],
             container = NA_character_,
             file = in_refs$from_file[i],
             line = in_refs$line[i],
             col = in_refs$col[i],
             warns = character())
      }
    }
  }

  nodes <- tibble::tibble(
    id = ids,
    type = types,
    name = names_v,
    namespace = namespaces,
    container = containers,
    fq_name = fq_names,
    file = files,
    line = lines,
    col = cols,
    warnings = warning_lists
  )

  # module_instance nodes — one per wrapper-call site. Each call site
  # gets its own node (distinct concrete ns_id), so we don't dedupe by
  # the (type, ns, container, name) key here.
  inst_nodes <- build_module_instance_nodes(app_path, defs)
  if (nrow(inst_nodes)) {
    nodes <- rbind(nodes, inst_nodes)
  }

  if (!nrow(nodes) || !nrow(warnings)) return(nodes)

  # Attach SVT-W010 to the canonical output node. The warning rows point
  # at the *flagged* (subsequent) sites; the node lives at the first.
  w010 <- warnings[warnings$code == "SVT-W010", , drop = FALSE]
  if (nrow(w010)) {
    out_idx <- which(nodes$type == "output")
    for (i in seq_len(nrow(w010))) {
      flagged_file <- w010$file[i]
      candidate <- out_idx[nodes$file[out_idx] == flagged_file]
      # Find the canonical row that matches a definition with same (namespace,
      # name) as the flagged row. Defs rows in `defs` carry this info.
      flagged_def <- defs[defs$kind == "output" &
                            defs$from_file == flagged_file &
                            defs$line == w010$line[i] &
                            defs$col == w010$col[i], , drop = FALSE]
      if (!nrow(flagged_def)) next
      ns_match <- ifelse(is.na(flagged_def$namespace[1]), "",
                         flagged_def$namespace[1])
      ns_node <- ifelse(is.na(nodes$namespace[candidate]), "",
                        nodes$namespace[candidate])
      pick <- candidate[ns_node == ns_match &
                          nodes$name[candidate] == flagged_def$name[1]]
      if (!length(pick)) next
      idx <- pick[1L]
      nodes$warnings[[idx]] <- unique(c(nodes$warnings[[idx]], "SVT-W010"))
    }
  }

  nodes
 })
}
