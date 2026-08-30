#' Render functions whose value is not a useful comparison target.
#'
#' Under `testServer()` these are readable but opaque: a display list, a
#' tag structure, a writer function. Spec 06 annotates them rather than
#' switching harness — the honest assertion is structural, and the
#' semantic one belongs in the helper that computed the data.
#'
#' @noRd
opaque_def_calls <- function() {
  c("renderPlot", "renderCachedPlot", "renderImage", "renderUI",
    "downloadHandler", "renderPlotly", "renderLeaflet", "renderDataTable",
    "renderD3", "renderEcharts4r", "renderGirafe")
}

#' The static-analysis limits that make a derived test surface partial.
#'
#' @noRd
blocker_codes <- function() {
  c("SVT-W001", "SVT-W002", "SVT-W003", "SVT-W009")
}

#' Node ids reachable *downstream* of a node, within a restricted set.
#'
#' Edges encode `source depends_on target`, so `upstream_closure()`
#' follows source → target. Reachability the other way — "which roots
#' does this input drive" — follows target → source.
#'
#' @noRd
downstream_closure <- function(node_id, edges, restrict) {
  visited <- character()
  frontier <- node_id
  while (length(frontier)) {
    visited <- unique(c(visited, frontier))
    if (!nrow(edges)) break
    nxt <- edges$source_id[edges$target_id %in% frontier]
    nxt <- setdiff(unique(nxt[nxt %in% restrict]), visited)
    frontier <- nxt
  }
  setdiff(visited, node_id)
}

#' The `def_call` recorded for a node, or NA.
#'
#' Nodes are content-addressed by `(type, namespace, container, name)`;
#' the Definitions table is keyed the same way, so the join is exact.
#' Inputs have no definition site and so never carry a `def_call`.
#'
#' @noRd
def_call_for <- function(node, defs) {
  if (!nrow(defs) || !"def_call" %in% names(defs)) return(NA_character_)
  ns_def <- ifelse(is.na(defs$namespace), "", defs$namespace)
  ns_node <- ifelse(is.na(node$namespace), "", node$namespace)
  hit <- defs$kind == node$type & defs$name == node$name & ns_def == ns_node
  if (!any(hit)) return(NA_character_)
  defs$def_call[which(hit)[1L]]
}

#' Order helper stubs: method, framework, utility, unset; then by name.
#'
#' The package's only opinion about what to test first, and it follows
#' the category model in spec 00 — a `method` helper carries the
#' analytical risk and is the cheapest thing in the app to unit test.
#'
#' @noRd
order_helpers <- function(helpers) {
  if (!nrow(helpers)) return(helpers)
  rank <- match(helpers$category, c("method", "framework", "utility"))
  rank[is.na(rank)] <- 4L
  helpers[order(rank, helpers$fn), , drop = FALSE]
}

#' Canonical `surface_hash` — md5 over the testable surface only.
#'
#' Every component is sorted before hashing, so the hash changes exactly
#' when the surface changes and never when a line moves or a table's row
#' order shifts.
#'
#' @noRd
surface_hash_of <- function(stimuli, observables, internals, helpers, blockers) {
  parts <- c(
    paste0("stimuli:", paste(sort(stimuli), collapse = ",")),
    paste0("observables:", paste(sort(observables), collapse = ",")),
    paste0("internals:", paste(sort(internals), collapse = ",")),
    paste0("helpers:", paste(sort(helpers), collapse = ",")),
    paste0("blockers:", paste(sort(blockers), collapse = ","))
  )
  md5_string(paste(parts, collapse = "\n"))
}

#' md5 of a string, without touching the filesystem twice.
#' @noRd
md5_string <- function(x) {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  writeLines(x, tmp, useBytes = TRUE)
  unname(tools::md5sum(tmp))
}

#' Empty per-component tables — the canonical shapes.
#' @noRd
empty_stimuli <- function() {
  tibble::tibble(name = character(), node_id = character(), drives = list(),
                 file = character(), line = integer(), col = integer())
}

#' @noRd
empty_observables <- function() {
  tibble::tibble(name = character(), kind = character(), def_call = character(),
                 observable_via = character(), file = character(),
                 line = integer(), col = integer())
}

#' @noRd
empty_internals <- function() {
  tibble::tibble(name = character(), kind = character(),
                 observable_via = character(), file = character(),
                 line = integer(), col = integer())
}

#' @noRd
empty_terminals <- function() {
  tibble::tibble(name = character(), kind = character(), artifact = character())
}

#' @noRd
empty_helpers <- function() {
  tibble::tibble(fn = character(), file = character(), line = integer(),
                 category = character())
}

#' Derive one feature's or module's test surface.
#'
#' Everything comes from the subgraph node set plus the Definitions table
#' — no new parsing, no app execution. See spec 06 "Test surface model".
#'
#' @noRd
build_feature_surface <- function(feature, graph, inventory_record, defs,
                                  helper_defs) {
  nodes <- graph$nodes
  edges <- graph$edges
  closure <- feature$node_ids
  sub <- nodes[nodes$id %in% closure, , drop = FALSE]
  is_module <- identical(feature$kind, "module")

  root_names <- nodes$name[nodes$id %in% feature$roots]

  # -- stimuli -------------------------------------------------------
  stim_rows <- sub[sub$type == "input", , drop = FALSE]
  stim_rows <- stim_rows[order(stim_rows$name), , drop = FALSE]
  stimuli <- if (!nrow(stim_rows)) empty_stimuli() else tibble::tibble(
    name = stim_rows$name,
    node_id = stim_rows$id,
    drives = lapply(stim_rows$id, function(id) {
      reached <- downstream_closure(id, edges, closure)
      sort(nodes$name[nodes$id %in% intersect(reached, feature$roots)])
    }),
    file = stim_rows$file,
    line = stim_rows$line,
    col = stim_rows$col
  )

  # -- observables ---------------------------------------------------
  obs_rows <- sub[sub$id %in% feature$roots, , drop = FALSE]
  obs_rows <- obs_rows[order(obs_rows$name), , drop = FALSE]
  observables <- if (!nrow(obs_rows)) empty_observables() else {
    calls <- vapply(seq_len(nrow(obs_rows)),
                    function(i) def_call_for(obs_rows[i, ], defs),
                    character(1))
    tibble::tibble(
      name = obs_rows$name,
      kind = obs_rows$type,
      def_call = calls,
      observable_via = ifelse(!is.na(calls) & calls %in% opaque_def_calls(),
                              "opaque", "direct"),
      file = obs_rows$file,
      line = obs_rows$line,
      col = obs_rows$col
    )
  }

  # -- terminals -----------------------------------------------------
  # Module instances are trusted boundaries (validated by their own
  # artifact); a top-level reactiveValues entry must be seeded by the test.
  term_rows <- sub[sub$type == "module_instance" |
                     (sub$type == "value" & is.na(sub$namespace)), , drop = FALSE]
  term_rows <- term_rows[order(term_rows$name), , drop = FALSE]
  terminals <- if (!nrow(term_rows)) empty_terminals() else tibble::tibble(
    name = term_rows$name,
    kind = term_rows$type,
    artifact = ifelse(term_rows$type == "module_instance",
                      paste0(slugify_artifact_name(term_rows$name), ".md"),
                      NA_character_)
  )

  # -- internals -----------------------------------------------------
  int_rows <- sub[sub$type %in% c("reactive", "value") &
                    !sub$id %in% feature$roots &
                    !sub$id %in% term_rows$id, , drop = FALSE]
  int_rows <- int_rows[order(int_rows$name), , drop = FALSE]
  internals <- if (!nrow(int_rows)) empty_internals() else tibble::tibble(
    name = int_rows$name,
    kind = int_rows$type,
    # Under testServer() a named reactive is callable by name, so every
    # internal is directly readable. This is why no export plumbing is
    # generated (spec 06 "Why not shinytest2").
    observable_via = rep("direct", nrow(int_rows)),
    file = int_rows$file,
    line = int_rows$line,
    col = int_rows$col
  )

  # -- helpers -------------------------------------------------------
  cats <- as.list(feature$package_categories %||% list())
  app_cat <- cats[["app"]] %||% "unset"
  called <- if (!is.null(inventory_record) && nrow(inventory_record$functions)) {
    inventory_record$functions$fn
  } else {
    character()
  }
  helper_hits <- helper_defs[helper_defs$name %in% called, , drop = FALSE]
  helpers <- if (!nrow(helper_hits)) empty_helpers() else order_helpers(
    tibble::tibble(
      fn = helper_hits$name,
      file = helper_hits$from_file,
      line = helper_hits$line,
      category = rep(as.character(app_cat), nrow(helper_hits))
    )
  )

  # -- blockers ------------------------------------------------------
  w <- graph$warnings
  blockers <- if (nrow(w) && "node_id" %in% names(w)) {
    sort(unique(w$code[!is.na(w$node_id) & w$node_id %in% closure &
                         w$code %in% blocker_codes()]))
  } else {
    character()
  }

  surface_warnings <- character()
  if (length(blockers)) surface_warnings <- c(surface_warnings, "SVT-W308")
  n_opaque <- sum(observables$observable_via == "opaque")
  if (n_opaque > 0L) surface_warnings <- c(surface_warnings, "SVT-W312")

  reason <- if (is_module) {
    "module server function driven directly by shiny::testServer()"
  } else {
    "feature surface driven through shiny::testServer()"
  }
  if (n_opaque > 0L) {
    reason <- paste0(reason, "; ", n_opaque, " observable",
                     if (n_opaque > 1L) "s are" else " is", " opaque")
  }

  list(
    name = feature$name,
    kind = feature$kind %||% "feature",
    surface_hash = surface_hash_of(
      stimuli$name,
      paste0(observables$name, "=", observables$def_call),
      internals$name,
      paste0(helpers$file, ":", helpers$fn),
      blockers
    ),
    harness = "testserver",
    harness_reason = reason,
    stimuli = stimuli,
    observables = observables,
    internals = internals,
    terminals = terminals,
    helpers = helpers,
    blockers = blockers,
    warnings = surface_warnings
  )
}

#' Build the test surface for every record in a slice.
#'
#' @noRd
build_test_surface <- function(graph, records, inventory_features, app_path) {
  defs <- build_definitions_table(app_path)
  helper_defs <- defs[defs$kind == "function", , drop = FALSE]
  # First definition wins, mirroring the node-canonicalization rule.
  helper_defs <- helper_defs[!duplicated(helper_defs$name), , drop = FALSE]

  out <- list()
  for (rec in records) {
    out[[rec$name]] <- build_feature_surface(
      rec, graph,
      inventory_record = if (is.null(inventory_features)) NULL else
        inventory_features[[rec$name]],
      defs = defs,
      helper_defs = helper_defs
    )
  }
  out[order(names(out))]
}

#' Render one surface as a JSON document.
#'
#' Emits the canonical `test_surface.json` shape with `schema_version`
#' 1.0. Stability-contracted like `inventory.json`: additive changes only
#' within v1, no removals or renames. Every list is already sorted by the
#' derivation, so the output is byte-deterministic.
#'
#' @noRd
render_test_surface_json <- function(surface) {
  loc <- function(file, line, col) {
    json_object(list(file = json_str(file), line = json_int(line),
                     col = json_int(col)))
  }

  stim <- surface$stimuli
  stim_objs <- if (!nrow(stim)) character() else vapply(seq_len(nrow(stim)),
    function(i) json_object(list(
      name = json_str(stim$name[i]),
      node_id = json_str(stim$node_id[i]),
      drives = json_str_array(stim$drives[[i]]),
      source_loc = loc(stim$file[i], stim$line[i], stim$col[i])
    )), character(1))

  obs <- surface$observables
  obs_objs <- if (!nrow(obs)) character() else vapply(seq_len(nrow(obs)),
    function(i) json_object(list(
      name = json_str(obs$name[i]),
      kind = json_str(obs$kind[i]),
      def_call = json_str_or_null(obs$def_call[i]),
      observable_via = json_str(obs$observable_via[i]),
      source_loc = loc(obs$file[i], obs$line[i], obs$col[i])
    )), character(1))

  int <- surface$internals
  int_objs <- if (!nrow(int)) character() else vapply(seq_len(nrow(int)),
    function(i) json_object(list(
      name = json_str(int$name[i]),
      kind = json_str(int$kind[i]),
      observable_via = json_str(int$observable_via[i]),
      source_loc = loc(int$file[i], int$line[i], int$col[i])
    )), character(1))

  term <- surface$terminals
  term_objs <- if (!nrow(term)) character() else vapply(seq_len(nrow(term)),
    function(i) json_object(list(
      name = json_str(term$name[i]),
      kind = json_str(term$kind[i]),
      artifact = json_str_or_null(term$artifact[i])
    )), character(1))

  hlp <- surface$helpers
  hlp_objs <- if (!nrow(hlp)) character() else vapply(seq_len(nrow(hlp)),
    function(i) json_object(list(
      `function` = json_str(hlp$fn[i]),
      file = json_str(hlp$file[i]),
      line = json_int(hlp$line[i]),
      category = json_str(hlp$category[i])
    )), character(1))

  json_object(list(
    feature = json_str(surface$name),
    kind = json_str(surface$kind),
    schema_version = json_str("1.0"),
    surface_hash = json_str(surface$surface_hash),
    harness = json_str(surface$harness),
    harness_reason = json_str(surface$harness_reason),
    stimuli = json_array(stim_objs),
    observables = json_array(obs_objs),
    internals = json_array(int_objs),
    terminals = json_array(term_objs),
    helpers = json_array(hlp_objs),
    blockers = json_str_array(surface$blockers),
    warnings = json_str_array(surface$warnings)
  ))
}

#' Write the test_surface.json artifact for one surface.
#'
#' Layout: `<out_dir>/<slug>/test_surface.json` — sibling of the
#' feature's `inventory.json`. Returns the path written.
#'
#' @noRd
write_test_surface_json <- function(surface, out_dir) {
  feat_dir <- file.path(out_dir, slugify_artifact_name(surface$name))
  if (!dir.exists(feat_dir)) dir.create(feat_dir, recursive = TRUE)
  path <- file.path(feat_dir, "test_surface.json")
  writeLines(render_test_surface_json(surface), path)
  path
}

#' Render the `## Test surface` doc-stub section.
#'
#' One row per surface member, in the spec 06 role order. The section is
#' auto-filled and refreshes on every run.
#'
#' @noRd
render_test_surface_section <- function(surface) {
  out <- c(paste0("Harness: ", surface$harness,
                  " (", surface$harness_reason, ")"), "")

  hdr <- "| Role       | Name | Detail |"
  sep <- "|------------|------|--------|"
  rows <- character()

  for (i in seq_len(nrow(surface$stimuli))) {
    drives <- surface$stimuli$drives[[i]]
    detail <- if (length(drives)) {
      paste0("drives ", paste(drives, collapse = ", "))
    } else {
      "drives nothing in this subgraph"
    }
    rows <- c(rows, paste0("| stimulus | ", surface$stimuli$name[i],
                           " | ", detail, " |"))
  }
  for (i in seq_len(nrow(surface$observables))) {
    call <- surface$observables$def_call[i]
    call_text <- if (is.na(call)) "(unknown call)" else call
    rows <- c(rows, paste0("| observable | ", surface$observables$name[i],
                           " | ", call_text, " - ",
                           surface$observables$observable_via[i], " |"))
  }
  for (i in seq_len(nrow(surface$internals))) {
    rows <- c(rows, paste0("| internal | ", surface$internals$name[i],
                           " | readable by name under testServer |"))
  }
  for (i in seq_len(nrow(surface$terminals))) {
    art <- surface$terminals$artifact[i]
    detail <- if (is.na(art)) "seed in the test" else
      paste0("validated separately - ", art)
    rows <- c(rows, paste0("| terminal | ", surface$terminals$name[i],
                           " | ", detail, " |"))
  }
  for (i in seq_len(nrow(surface$helpers))) {
    rows <- c(rows, paste0("| helper | ", surface$helpers$fn[i], " | ",
                           surface$helpers$category[i], " - ",
                           surface$helpers$file[i], ":",
                           surface$helpers$line[i], " |"))
  }

  if (!length(rows)) {
    out <- c(out, "(no derivable surface)")
  } else {
    out <- c(out, hdr, sep, rows)
  }

  if (length(surface$warnings)) {
    out <- c(out, "",
             vapply(surface$warnings,
                    function(c) paste0("- ", c, " - ", warning_message(c)),
                    character(1)))
  }
  out
}
