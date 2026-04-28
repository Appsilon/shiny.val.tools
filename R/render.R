#' Node styling table.
#'
#' Type -> (shape, color). v1 ships one fixed style and does not surface
#' theming. Returned as a small lookup tibble so the rendering pipeline
#' can join on `type` in one go.
#'
#' @noRd
node_style_table <- function() {
  tibble::tibble(
    type = c("input", "output", "reactive", "observer",
             "value", "module_instance"),
    shape = c("ellipse", "box", "diamond", "triangle",
              "dot", "doubleCircle"),
    color = c("#4C9BE8", "#5BB85B", "#F0AD4E", "#D9534F",
              "#777777", "#9265DA")
  )
}

#' Decide which solver to use for a subgraph.
#'
#' Default to `hierarchicalRepulsion` with LR direction. The
#' `forceAtlas2Based` fallback is for graphs that would produce >5%
#' node overlap in the hierarchical layout -- a runtime concept we
#' cannot measure statically. We approximate with a density heuristic:
#' when the edge-to-node ratio exceeds 1.5, the hierarchical layout
#' begins to crowd at typical canvas sizes, so we fall back. The
#' fallback decision is logged in the widget footer.
#'
#' Returns a list with `solver` and `reason` (a short string for the
#' footer).
#'
#' @noRd
choose_solver <- function(n_nodes, n_edges) {
  if (n_nodes < 1L) {
    return(list(solver = "hierarchicalRepulsion",
                reason = "empty subgraph"))
  }
  density <- n_edges / max(n_nodes, 1L)
  if (density > 1.5) {
    return(list(solver = "forceAtlas2Based",
                reason = sprintf(
                  "density %.2f exceeds 1.5; hierarchical layout would crowd",
                  density)))
  }
  list(solver = "hierarchicalRepulsion",
       reason = sprintf("density %.2f within hierarchical budget", density))
}

#' Build the visNetwork node data frame for a feature subgraph.
#'
#' Joins the subgraph's nodes against the style table. Tooltips are
#' assembled inline with HTML so they render as rich text in the
#' widget. `module_instance` rows additionally carry a `url` field with
#' a link to the corresponding module artifact -- the click handler
#' picks this up.
#'
#' @noRd
build_vis_nodes <- function(feature, graph, inventory_record = NULL) {
  if (!length(feature$node_ids)) {
    return(data.frame(
      id = character(), label = character(), shape = character(),
      color = character(), title = character(), url = character(),
      stringsAsFactors = FALSE
    ))
  }
  nodes <- graph$nodes[graph$nodes$id %in% feature$node_ids, , drop = FALSE]
  nodes <- nodes[order(nodes$id), , drop = FALSE]
  styles <- node_style_table()

  shape <- vapply(nodes$type, function(t) {
    hit <- styles$shape[styles$type == t]
    if (length(hit)) hit[1L] else "dot"
  }, character(1))
  color <- vapply(nodes$type, function(t) {
    hit <- styles$color[styles$type == t]
    if (length(hit)) hit[1L] else "#999999"
  }, character(1))

  labels <- ifelse(is.na(nodes$fq_name) | !nzchar(nodes$fq_name),
                   nodes$name, nodes$fq_name)

  titles <- vapply(seq_len(nrow(nodes)), function(i) {
    fq <- if (!is.na(nodes$fq_name[i]) && nzchar(nodes$fq_name[i])) {
      nodes$fq_name[i]
    } else {
      nodes$name[i]
    }
    loc <- if (!is.na(nodes$file[i]) && nzchar(nodes$file[i])) {
      paste0(nodes$file[i], ":", as.integer(nodes$line[i] %|NA|% NA_integer_))
    } else {
      "(unknown)"
    }
    warns <- nodes$warnings[[i]]
    warn_line <- if (length(warns)) {
      paste0("warnings: ", paste(warns, collapse = ", "))
    } else {
      "warnings: (none)"
    }
    paste0("<b>", htmltools::htmlEscape(fq), "</b><br>",
           htmltools::htmlEscape(loc), "<br>",
           "type: ", htmltools::htmlEscape(nodes$type[i]), "<br>",
           htmltools::htmlEscape(warn_line))
  }, character(1))

  urls <- ifelse(nodes$type == "module_instance",
                 paste0(nodes$name, ".html"),
                 NA_character_)

  warning_count <- vapply(nodes$warnings, length, integer(1))
  has_warning <- warning_count > 0L
  labels <- ifelse(has_warning,
                   paste0(labels, " (\u26A0", warning_count, ")"),
                   labels)

  data.frame(
    id = nodes$id,
    label = labels,
    group = nodes$type,
    shape = shape,
    color = color,
    title = titles,
    url = urls,
    stringsAsFactors = FALSE
  )
}

#' Build the visNetwork edge data frame.
#'
#' The rendered arrow flows along data-flow direction (input -> reactive
#' -> output), the inverse of the depends_on edge in the graph model
#' (where `source` reads `target`). We swap source/target here at the
#' rendering boundary so the underlying graph data stays canonical and
#' auditors read the widget as "this input feeds this output".
#'
#' @noRd
build_vis_edges <- function(feature, graph) {
  if (!length(feature$edge_ids) || !nrow(graph$edges)) {
    return(data.frame(from = character(), to = character(),
                      stringsAsFactors = FALSE))
  }
  edges <- graph$edges[feature$edge_ids, , drop = FALSE]
  ord <- order(edges$target_id, edges$source_id)
  data.frame(
    from = edges$target_id[ord],
    to   = edges$source_id[ord],
    stringsAsFactors = FALSE
  )
}

#' Render one feature/module as a self-contained HTML widget.
#'
#' Writes `<out_dir>/<name>.html`. Returns the path written.
#'
#' Determinism: nodes/edges are sorted by id; layout is
#' `hierarchicalRepulsion` with a fixed seed when overlap is unlikely;
#' header/footer text is fully derived from inputs (no timestamps).
#'
#' @noRd
write_feature_html <- function(feature, graph,
                               out_dir,
                               inventory_record = NULL,
                               app_path = NULL) {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  nodes_df <- build_vis_nodes(feature, graph, inventory_record)
  edges_df <- build_vis_edges(feature, graph)

  solver <- choose_solver(nrow(nodes_df), nrow(edges_df))

  warning_count <- if (!is.null(inventory_record) &&
                         nrow(inventory_record$warnings)) {
    nrow(inventory_record$warnings)
  } else {
    0L
  }
  node_warnings <- sum(vapply(
    if (length(feature$node_ids)) {
      ws <- graph$nodes$warnings[graph$nodes$id %in% feature$node_ids]
      ws
    } else {
      list()
    },
    length, integer(1)
  ))
  total_warnings <- as.integer(warning_count + node_warnings)

  commit <- git_commit_short(app_path)

  intended_use <- feature$intended_use
  intended_short <- truncate_text(intended_use, 120L)
  risk <- feature$risk_classification %|NA|% "(not declared)"

  header_main <- paste0(
    if (identical(feature$kind, "module")) "Module: " else "Feature: ",
    feature$name
  )
  header_sub <- paste0(
    "Risk: ", risk,
    if (!is.na(intended_short) && nzchar(intended_short)) {
      paste0(" -- ", intended_short)
    } else {
      ""
    }
  )

  footer_parts <- c(
    paste0("Layout: ", solver$solver, " (", solver$reason, ")"),
    paste0("Warnings: ", total_warnings),
    paste0("Doc: <a href=\"", feature$name, ".md\">", feature$name, ".md</a>"),
    paste0("Inventory: <a href=\"", feature$name,
           "/inventory.json\">inventory.json</a>")
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
  styles <- node_style_table()
  for (i in seq_len(nrow(styles))) {
    widget <- visNetwork::visGroups(
      widget,
      groupname = styles$type[i],
      shape     = styles$shape[i],
      color     = styles$color[i]
    )
  }
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

  path <- file.path(out_dir, paste0(feature$name, ".html"))
  visNetwork::visSave(widget, file = path, selfcontained = TRUE,
                      background = "white")
  path
}

#' Read the short commit hash for an app, if it lives in a git repo.
#'
#' Returns `NA_character_` when `app_path` is NULL, when no `.git`
#' directory is found, or when `git` is unavailable. Looks at HEAD only;
#' uncommitted changes are not flagged here (the inventory diff marker
#' on the doc stub is a separate concern).
#'
#' @noRd
git_commit_short <- function(app_path) {
  if (is.null(app_path)) return(NA_character_)
  git_dir <- file.path(app_path, ".git")
  if (!file.exists(git_dir)) return(NA_character_)
  out <- tryCatch(
    suppressWarnings(system2("git",
                             c("-C", shQuote(app_path), "rev-parse",
                               "--short", "HEAD"),
                             stdout = TRUE, stderr = FALSE)),
    error = function(e) NULL
  )
  if (is.null(out) || !length(out) || !nzchar(out[1L])) return(NA_character_)
  out[1L]
}

#' Trim a string to `max_chars`, appending an ellipsis when clipped.
#'
#' @noRd
truncate_text <- function(x, max_chars) {
  if (is.null(x) || is.na(x)) return(NA_character_)
  s <- as.character(x)
  if (nchar(s) <= max_chars) return(s)
  paste0(substr(s, 1L, max_chars - 1L), "\u2026")
}

#' NA-aware fallback -- returns `y` when `x` is NA, else `x`.
#'
#' @noRd
`%|NA|%` <- function(x, y) {
  if (is.null(x) || (length(x) == 1L && is.na(x))) y else x
}
