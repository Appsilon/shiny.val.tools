#' Render the master index doc stub.
#'
#' Returns the same `list(text, auto_keys)` shape as `render_doc_stub()`
#' so the merge-on-regen mechanism reuses the per-feature path.
#'
#' Section order (per spec 04 "Index artifact"):
#'   1. Title (`# <app> Validation`)
#'   2. Summary             — counts table (auto)
#'   3. App overview        — link to `index.html` (auto)
#'   4. Features            — table (auto)
#'   5. Modules             — table (auto)
#'   6. Module relationships — bullet list (auto)
#'   7. Aggregate warnings  — table (auto)
#'   8. Reviewers           — placeholder (preserved on regen)
#'
#' @noRd
render_index_md <- function(features, inventory, graph, app_path) {
  app_name <- if (is.null(app_path) || !nzchar(app_path)) {
    "App"
  } else {
    basename(normalizePath(app_path, winslash = "/", mustWork = FALSE))
  }

  records <- features$records %||% list()
  feats <- Filter(function(r) identical(r$kind, "feature"), records)
  mods <- Filter(function(r) identical(r$kind, "module"), records)

  out <- character()
  out <- c(out, paste0("# ", app_name, " Validation"), "")

  out <- c(out, "## Summary",
           render_summary_table(features, inventory, graph, app_path),
           "")

  out <- c(out, "## App overview",
           "[Architecture diagram](index.html)",
           "")

  out <- c(out, "## Features",
           render_features_table(feats, inventory, graph),
           "")

  out <- c(out, "## Modules",
           render_modules_table(mods, inventory, graph),
           "")

  out <- c(out, "## Module relationships",
           render_module_relationships(records, graph),
           "")

  out <- c(out, "## Aggregate warnings",
           render_aggregate_warnings(features, inventory, graph),
           "")

  out <- c(out, "## Reviewers",
           "- Developer: __________________ Date: __________",
           "- Validation Engineer: __________________ Date: __________")

  list(
    text = paste(out, collapse = "\n"),
    auto_keys = c("Summary", "App overview", "Features", "Modules",
                  "Module relationships", "Aggregate warnings")
  )
}

#' Render the Summary section as a tight 2-column table.
#'
#' Two package metrics, because they answer different questions and one
#' cannot be recovered from the other:
#'
#'   - **Packages used** — how many distinct packages the validated surface
#'     depends on. This is the app-level number, and the one an auditor
#'     scanning the index is asking for.
#'   - **Package use rows** — the sum over features, so a package used by
#'     five features counts five times. It is the size of the evidence
#'     underneath, and it is what the per-feature inventories add up to.
#'
#' @noRd
render_summary_table <- function(features, inventory, graph, app_path) {
  records <- features$records %||% list()
  feats <- Filter(function(r) identical(r$kind, "feature"), records)
  mods <- Filter(function(r) identical(r$kind, "module"), records)

  files <- if (!is.null(graph$files)) nrow(graph$files) else 0L
  use_rows <- 0L
  distinct <- character()
  for (nm in names(inventory$features %||% list())) {
    pv <- inventory$features[[nm]]$packages
    if (is.null(pv) || !nrow(pv)) next
    use_rows <- use_rows + nrow(pv)
    distinct <- c(distinct, pv$package[!is.na(pv$package)])
  }
  distinct <- length(unique(distinct))

  warns <- aggregate_warning_counts(features, inventory, graph)
  total_warns <- if (nrow(warns)) sum(warns$count) else 0L

  commit <- git_commit_short(app_path)
  rows <- c(
    paste0("| Features | ", length(feats), " |"),
    paste0("| Modules | ", length(mods), " |"),
    paste0("| Files parsed | ", files, " |"),
    paste0("| Packages used | ", distinct, " |"),
    paste0("| Package use rows | ", use_rows, " |"),
    paste0("| Total warnings | ", total_warns, " |")
  )
  if (!is.na(commit)) {
    rows <- c(rows, paste0("| Commit | ", commit, " |"))
  }
  c("| Metric | Value |",
    "|--------|-------|",
    rows)
}

#' Render the Features table.
#'
#' @noRd
render_features_table <- function(feats, inventory, graph) {
  if (!length(feats)) return("(none)")
  hdr <- "| Name | Intended use | Risk | Nodes | Warnings | Doc | Widget |"
  sep <- "|------|--------------|------|-------|----------|-----|--------|"
  feats <- feats[order(vapply(feats, function(f) f$name, character(1)))]
  rows <- vapply(feats, function(f) {
    slug <- slugify_artifact_name(f$name)
    iu <- truncate_text(f$intended_use, 60L)
    iu_text <- if (is.na(iu) || !nzchar(iu)) "(undeclared)" else iu
    risk <- f$risk_classification %|NA|% "(undeclared)"
    nodes <- length(f$node_ids)
    warns <- count_record_warnings(f, inventory, graph)
    paste0("| ", f$name,
           " | ", iu_text,
           " | ", risk,
           " | ", nodes,
           " | ", warns,
           " | [", slug, ".md](", slug, ".md)",
           " | [", slug, ".html](", slug, ".html) |")
  }, character(1))
  c(hdr, sep, rows)
}

#' Render the Modules table.
#'
#' @noRd
render_modules_table <- function(mods, inventory, graph) {
  if (!length(mods)) return("(none)")
  hdr <- "| Name | Inputs | Outputs | Returned | Warnings | Doc | Widget |"
  sep <- "|------|--------|---------|----------|----------|-----|--------|"
  mods <- mods[order(vapply(mods, function(m) m$name, character(1)))]
  rows <- vapply(mods, function(m) {
    slug <- slugify_artifact_name(m$name)
    contract <- m$contract %||% list(inputs = character(),
                                     outputs = character(),
                                     returned = character())
    warns <- count_record_warnings(m, inventory, graph)
    paste0("| ", m$name,
           " | ", length(contract$inputs),
           " | ", length(contract$outputs),
           " | ", length(contract$returned),
           " | ", warns,
           " | [", slug, ".md](", slug, ".md)",
           " | [", slug, ".html](", slug, ".html) |")
  }, character(1))
  c(hdr, sep, rows)
}

#' Render the Module relationships section.
#'
#' One bullet per `<parent>` → `<child>` instantiation. Derived from
#' `module_instance` nodes whose `name` field resolves to a known module
#' identity. The parent is identified by the record (feature or module)
#' whose subgraph contains the instance.
#'
#' @noRd
render_module_relationships <- function(records, graph) {
  edges <- compute_module_edges(records, graph)
  if (!nrow(edges)) return("(none)")
  edges <- edges[order(edges$parent, edges$child), , drop = FALSE]
  rels <- vapply(seq_len(nrow(edges)), function(i) {
    paste0("- `", edges$parent[i], "` instantiates `", edges$child[i], "`")
  }, character(1))
  rels
}

#' Render the Aggregate warnings table.
#'
#' @noRd
render_aggregate_warnings <- function(features, inventory, graph) {
  agg <- aggregate_warning_counts(features, inventory, graph)
  if (!nrow(agg)) return("(none)")
  hdr <- "| Code | Count | Description |"
  sep <- "|------|-------|-------------|"
  rows <- vapply(seq_len(nrow(agg)), function(i) {
    paste0("| ", agg$code[i],
           " | ", agg$count[i],
           " | ", warning_message(agg$code[i]), " |")
  }, character(1))
  c(hdr, sep, rows)
}

#' Aggregate warning counts across the graph + every per-feature inventory.
#'
#' @noRd
aggregate_warning_counts <- function(features, inventory, graph) {
  buckets <- list()
  bump <- function(code, n = 1L) {
    if (is.null(code) || is.na(code) || !nzchar(code)) return(invisible())
    buckets[[code]] <<- (buckets[[code]] %||% 0L) + as.integer(n)
  }

  if (!is.null(graph$warnings) && nrow(graph$warnings)) {
    for (c in graph$warnings$code) bump(c)
  }
  if (!is.null(graph$nodes) && nrow(graph$nodes) &&
      "warnings" %in% colnames(graph$nodes)) {
    for (w in graph$nodes$warnings) {
      if (length(w)) for (c in w) bump(c)
    }
  }
  for (nm in names(inventory$features %||% list())) {
    inv_w <- inventory$features[[nm]]$warnings
    if (!is.null(inv_w) && nrow(inv_w)) {
      for (c in inv_w$code) bump(c)
    }
  }
  issues <- features$manifest_issues
  if (!is.null(issues) && nrow(issues)) {
    for (c in issues$code) bump(c)
  }

  if (!length(buckets)) {
    return(tibble::tibble(code = character(), count = integer()))
  }
  codes <- sort(names(buckets))
  tibble::tibble(
    code = codes,
    count = vapply(codes, function(c) buckets[[c]], integer(1))
  )
}

#' Count warnings owned by one feature/module record.
#'
#' @noRd
count_record_warnings <- function(rec, inventory, graph) {
  n <- 0L
  if (!is.null(graph$nodes) && length(rec$node_ids)) {
    nodes <- graph$nodes[graph$nodes$id %in% rec$node_ids, , drop = FALSE]
    if (nrow(nodes) && "warnings" %in% colnames(nodes)) {
      n <- n + sum(vapply(nodes$warnings, length, integer(1)))
    }
  }
  inv_rec <- inventory$features[[rec$name]]
  if (!is.null(inv_rec) && !is.null(inv_rec$warnings) &&
      nrow(inv_rec$warnings)) {
    n <- n + nrow(inv_rec$warnings)
  }
  as.integer(n)
}

#' Compute parent->child module edges from records + graph.
#'
#' A parent is any record whose subgraph contains a `module_instance`
#' node whose `name` (the target module identity) matches a known
#' module record. The output is a tibble with `parent` and `child` cols.
#'
#' @noRd
compute_module_edges <- function(records, graph) {
  if (!length(records) || is.null(graph$nodes) || !nrow(graph$nodes)) {
    return(tibble::tibble(parent = character(), child = character()))
  }
  module_names <- vapply(
    Filter(function(r) identical(r$kind, "module"), records),
    function(r) r$name, character(1)
  )

  parents <- character()
  children <- character()
  for (rec in records) {
    if (!length(rec$node_ids)) next
    in_set <- graph$nodes[graph$nodes$id %in% rec$node_ids, , drop = FALSE]
    mi <- in_set[in_set$type == "module_instance", , drop = FALSE]
    if (!nrow(mi)) next
    for (child_name in unique(mi$name)) {
      if (!child_name %in% module_names) next
      if (identical(child_name, rec$name)) next  # don't self-loop
      parents <- c(parents, rec$name)
      children <- c(children, child_name)
    }
  }
  tibble::tibble(parent = parents, child = children) |> unique_rows()
}

#' Drop duplicate rows from a 2-column tibble (parent/child).
#'
#' @noRd
unique_rows <- function(tbl) {
  if (!nrow(tbl)) return(tbl)
  key <- paste(tbl$parent, tbl$child, sep = "\x1f")
  tbl[!duplicated(key), , drop = FALSE]
}

#' Write `index.md` to `out_dir`, merging with any existing reviewers section.
#'
#' @noRd
write_index_md <- function(features, inventory, graph, out_dir, app_path) {
  rendered <- render_index_md(features, inventory, graph, app_path)
  path <- file.path(out_dir, "index.md")
  existing <- if (file.exists(path)) {
    paste(readLines(path, warn = FALSE), collapse = "\n")
  } else {
    NULL
  }
  text <- merge_doc_stub(rendered, existing)
  writeLines(text, path)
  path
}

#' Render the architecture overview as a visNetwork HTML widget.
#'
#' Modules and top-level features are nodes; edges are parent-module ->
#' child-module instantiations and feature -> module references. Click a
#' node to open its `<slug>.html` in a new tab. See spec 04 "Index
#' artifact / index.html".
#'
#' @noRd
write_index_html <- function(features, inventory, graph, out_dir, app_path) {
  records <- features$records %||% list()
  feats <- Filter(function(r) identical(r$kind, "feature"), records)
  mods <- Filter(function(r) identical(r$kind, "module"), records)

  styles <- node_style_table()
  shape_for <- function(kind) {
    s <- styles$shape[styles$type == if (kind == "module")
                                       "module_instance" else "output"]
    if (length(s)) s[1L] else "dot"
  }
  color_for <- function(kind) {
    c <- styles$color[styles$type == if (kind == "module")
                                       "module_instance" else "output"]
    if (length(c)) c[1L] else "#999999"
  }

  node_rows <- list()
  for (m in mods) {
    slug <- slugify_artifact_name(m$name)
    title <- paste0("<b>", htmltools::htmlEscape(m$name), "</b><br>",
                    "type: module")
    node_rows[[length(node_rows) + 1L]] <- data.frame(
      id = paste0("module:", m$name),
      label = m$name,
      group = "module",
      shape = shape_for("module"),
      color = color_for("module"),
      title = title,
      url = paste0(slug, ".html"),
      stringsAsFactors = FALSE
    )
  }
  for (f in feats) {
    slug <- slugify_artifact_name(f$name)
    title <- paste0("<b>", htmltools::htmlEscape(f$name), "</b><br>",
                    "type: feature")
    node_rows[[length(node_rows) + 1L]] <- data.frame(
      id = paste0("feature:", f$name),
      label = f$name,
      group = "feature",
      shape = shape_for("feature"),
      color = color_for("feature"),
      title = title,
      url = paste0(slug, ".html"),
      stringsAsFactors = FALSE
    )
  }
  nodes_df <- if (length(node_rows)) {
    do.call(rbind, node_rows)
  } else {
    data.frame(id = character(), label = character(), group = character(),
               shape = character(), color = character(),
               title = character(), url = character(),
               stringsAsFactors = FALSE)
  }
  nodes_df <- nodes_df[order(nodes_df$id), , drop = FALSE]

  edges <- compute_module_edges(records, graph)
  module_nodes <- vapply(mods, function(m) m$name, character(1))

  edge_from <- character()
  edge_to <- character()
  if (nrow(edges)) {
    for (i in seq_len(nrow(edges))) {
      parent_id <- if (edges$parent[i] %in% module_nodes) {
        paste0("module:", edges$parent[i])
      } else {
        paste0("feature:", edges$parent[i])
      }
      child_id <- paste0("module:", edges$child[i])
      edge_from <- c(edge_from, parent_id)
      edge_to <- c(edge_to, child_id)
    }
  }
  edges_df <- if (length(edge_from)) {
    df <- data.frame(from = edge_from, to = edge_to, stringsAsFactors = FALSE)
    df[order(df$from, df$to), , drop = FALSE]
  } else {
    data.frame(from = character(), to = character(),
               stringsAsFactors = FALSE)
  }

  solver <- choose_solver(nrow(nodes_df), nrow(edges_df))
  commit <- git_commit_short(app_path)

  app_name <- if (is.null(app_path) || !nzchar(app_path)) {
    "App"
  } else {
    basename(normalizePath(app_path, winslash = "/", mustWork = FALSE))
  }
  agg_warnings <- aggregate_warning_counts(features, inventory, graph)
  total_warns <- if (nrow(agg_warnings)) sum(agg_warnings$count) else 0L

  header_main <- paste0("Architecture: ", app_name)
  header_sub <- paste0(length(mods), " module(s), ",
                       length(feats), " feature(s)")
  footer_parts <- c(
    paste0("Layout: ", solver$solver, " (", solver$reason, ")"),
    paste0("Warnings: ", total_warns),
    "Doc: <a href=\"index.md\">index.md</a>"
  )
  if (!is.na(commit)) {
    footer_parts <- c(footer_parts, paste0("Commit: ", commit))
  }
  footer <- paste(footer_parts, collapse = " | ")

  widget <- visNetwork::visNetwork(
    nodes_df, edges_df,
    main = list(text = header_main, style = "font-weight: 600;"),
    submain = list(text = header_sub,
                   style = "color: #555; font-size: 14px;"),
    footer = list(text = footer, style = "font-size: 12px; color: #555;"),
    height = "640px", width = "100%"
  )
  widget$elementId <- "svt-index"
  widget <- visNetwork::visEdges(
    widget,
    arrows = "to",
    smooth = list(type = "cubicBezier",
                  roundness = 0.4,
                  forceDirection = "horizontal")
  )
  if (identical(solver$solver, "hierarchicalRepulsion")) {
    widget <- visNetwork::visHierarchicalLayout(
      widget, direction = "LR", sortMethod = "directed"
    )
    widget <- visNetwork::visPhysics(
      widget, solver = "hierarchicalRepulsion", stabilization = TRUE
    )
  } else {
    widget <- visNetwork::visPhysics(
      widget, solver = "forceAtlas2Based", stabilization = TRUE
    )
  }
  widget <- visNetwork::visGroups(
    widget, groupname = "module",
    shape = shape_for("module"), color = color_for("module")
  )
  widget <- visNetwork::visGroups(
    widget, groupname = "feature",
    shape = shape_for("feature"), color = color_for("feature")
  )
  widget <- visNetwork::visLegend(widget, useGroups = TRUE,
                                  position = "right", main = "Node type")
  widget <- visNetwork::visOptions(
    widget,
    highlightNearest = list(
      enabled   = TRUE,
      algorithm = "hierarchical",
      degree    = list(from = 50, to = 50),
      hover     = TRUE
    ),
    nodesIdSelection = TRUE
  )
  widget <- visNetwork::visEvents(widget, selectNode = paste0(
    "function(properties) {",
    "  if (!properties.nodes.length) return;",
    "  var n = this.body.data.nodes.get(properties.nodes[0]);",
    "  if (n && n.url) window.open(n.url, '_blank');",
    "}"
  ))

  path <- file.path(out_dir, "index.html")
  visNetwork::visSave(widget, file = path, selfcontained = TRUE,
                      background = "white")
  cleanup_widget_libdir(path)
  path
}
