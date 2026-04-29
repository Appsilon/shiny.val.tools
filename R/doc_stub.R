#' Render a feature or module as a markdown doc stub.
#'
#' Section order:
#'   1. Title (`Feature:` or `Module:`)
#'   2. Intended use            -- manifest, refreshed on regen
#'   3. Risk classification     -- manifest, refreshed on regen
#'   4. Rationale               -- manifest, refreshed on regen (optional)
#'   5. Reactive subgraph       -- link to <name>.html
#'   6. Module contract         -- module-only; inputs / outputs / returned
#'   7. Functions called        -- auto-filled from the inventory
#'   8. Packages used           -- auto-filled from the inventory
#'   9. Warnings                -- auto-filled (subgraph + inventory codes)
#'  10. Reviewers               -- human-authored (preserved on regen)
#'
#' `inventory` is the per-feature record produced by
#' `build_feature_inventory()`. When NULL the Functions/Packages sections
#' fall back to "(none)".
#'
#' The output is a list with two components:
#'   - `text`        -- the full markdown document
#'   - `auto_keys`   -- character vector of `## H2` headings the renderer
#'                      considers auto-filled (for merge semantics)
#'
#' @noRd
render_doc_stub <- function(feature, graph, warnings_in_subgraph = character(),
                            inventory = NULL) {
  is_module <- identical(feature$kind, "module")
  title <- if (is_module) "Module" else "Feature"
  out <- character()

  out <- c(out, paste0("# ", title, ": ", feature$name), "")

  out <- c(out, "## Intended use",
           render_manifest_text(feature$intended_use,
                                placeholder = "(not declared - fill before validation)"),
           "")

  out <- c(out, "## Risk classification",
           render_manifest_text(feature$risk_classification,
                                placeholder = "(not declared)"),
           "")

  rationale_text <- render_manifest_text(feature$rationale, placeholder = "")
  out <- c(out, "## Rationale", rationale_text, "")

  slug <- slugify_artifact_name(feature$name)
  out <- c(out, "## Reactive subgraph",
           paste0("[", feature$name, "](", slug, ".html)"),
           "")

  if (is_module && !is.null(feature$contract)) {
    out <- c(out,
             "## Module contract",
             render_module_contract(feature$contract),
             "")
  }

  out <- c(out, "## Functions called",
           render_functions_section(inventory),
           "")

  out <- c(out, "## Packages used",
           render_packages_section(inventory),
           "")

  inv_codes <- if (!is.null(inventory) && nrow(inventory$warnings)) {
    unique(inventory$warnings$code)
  } else {
    character()
  }
  combined_warnings <- unique(c(warnings_in_subgraph, inv_codes))

  out <- c(out, "## Warnings",
           render_warnings_section(combined_warnings),
           "")

  out <- c(out, "## Reviewers",
           "- Developer: __________________ Date: __________",
           "- Validation Engineer: __________________ Date: __________",
           "")

  list(
    text = paste(out, collapse = "\n"),
    auto_keys = c("Intended use", "Risk classification", "Rationale",
                  "Reactive subgraph",
                  if (is_module) "Module contract",
                  "Functions called", "Packages used", "Warnings")
  )
}

#' Render the Functions called section as a markdown table.
#'
#' One row per `(package, fn)`. `Calls` is the number of call sites for
#' that pair, mirroring the inventory collation. NA packages render as
#' `<unknown>` so unresolved rows still surface to the auditor.
#'
#' @noRd
render_functions_section <- function(inventory) {
  if (is.null(inventory) || !nrow(inventory$functions)) return("(none)")
  fns <- inventory$functions
  pkg_key <- ifelse(is.na(fns$package), "", fns$package)
  ord <- order(pkg_key, fns$fn)
  fns <- fns[ord, , drop = FALSE]

  hdr <- "| Package | Function | Resolution | Direct | Category | Calls | Warnings |"
  sep <- "|---------|----------|------------|--------|----------|-------|----------|"
  rows <- vapply(seq_len(nrow(fns)), function(i) {
    pkg <- fns$package[i]
    pkg_text <- if (is.na(pkg)) "<unknown>" else pkg
    n_calls <- nrow(fns$call_sites[[i]])
    warns <- fns$warnings[[i]]
    warn_text <- if (!length(warns)) "" else paste(warns, collapse = ", ")
    direct_text <- if (isTRUE(fns$direct[i])) "yes" else "no"
    paste0("| ", pkg_text, " | ", fns$fn[i], " | ", fns$resolution[i],
           " | ", direct_text, " | ", fns$category[i],
           " | ", n_calls, " | ", warn_text, " |")
  }, character(1))
  c(hdr, sep, rows)
}

#' Render the Packages used section as a markdown table.
#'
#' One row per package. The function list is collapsed inline; long lists
#' are kept readable enough for diff review.
#'
#' @noRd
render_packages_section <- function(inventory) {
  if (is.null(inventory) || !nrow(inventory$packages)) return("(none)")
  pkgs <- inventory$packages
  pkgs <- pkgs[order(pkgs$package), , drop = FALSE]

  hdr <- "| Package | Functions | Direct | Category | Call count |"
  sep <- "|---------|-----------|--------|----------|------------|"
  rows <- vapply(seq_len(nrow(pkgs)), function(i) {
    fns_text <- paste(pkgs$functions[[i]], collapse = ", ")
    direct_text <- if (isTRUE(pkgs$direct[i])) "yes" else "no"
    paste0("| ", pkgs$package[i], " | ", fns_text,
           " | ", direct_text, " | ", pkgs$category[i],
           " | ", pkgs$call_count[i], " |")
  }, character(1))
  c(hdr, sep, rows)
}

#' Format an optional manifest field, falling back to a placeholder.
#'
#' Manifest text fields are character or NA. Strip trailing whitespace
#' (yaml block scalars often add a final newline) so successive
#' regenerations don't drift.
#'
#' @noRd
render_manifest_text <- function(value, placeholder) {
  if (is.null(value) || (is.character(value) && length(value) == 1L &&
                         (is.na(value) || !nzchar(trimws(value))))) {
    return(placeholder)
  }
  trimws(as.character(value), which = "right")
}

#' Format the module contract block for the doc stub.
#' @noRd
render_module_contract <- function(contract) {
  fmt <- function(label, vec) {
    if (!length(vec)) return(paste0("- ", label, ": (none)"))
    paste0("- ", label, ": ", paste(vec, collapse = ", "))
  }
  c(
    fmt("Inputs", contract$inputs),
    fmt("Outputs", contract$outputs),
    fmt("Returned reactives", contract$returned)
  )
}

#' Format the Warnings section.
#' @noRd
render_warnings_section <- function(codes) {
  codes <- unique(as.character(codes))
  codes <- codes[!is.na(codes) & nzchar(codes)]
  if (!length(codes)) return("(none)")
  vapply(codes, function(c) paste0("- ", c, " - ", warning_message(c)),
         character(1))
}

#' Find warnings whose `(file, line)` falls inside a feature subgraph.
#'
#' The Warnings table carries file/line pointers to call sites. A
#' warning is "in the subgraph" when there is a node in the subgraph
#' whose location matches the warning row. Matching by file alone
#' would over-attribute (a single file owning many features); using
#' the per-node `(file, line)` keeps attribution precise.
#'
#' @noRd
warnings_in_subgraph <- function(feature, graph) {
  if (!nrow(graph$warnings)) return(character())
  subgraph_nodes <- graph$nodes[graph$nodes$id %in% feature$node_ids, , drop = FALSE]
  if (!nrow(subgraph_nodes)) return(character())

  hits <- character()
  for (i in seq_len(nrow(graph$warnings))) {
    w <- graph$warnings[i, , drop = FALSE]
    matches <- subgraph_nodes$file == w$file &
      !is.na(subgraph_nodes$line) & !is.na(w$line) &
      subgraph_nodes$line == w$line
    if (any(matches)) hits <- c(hits, w$code)
  }

  # Also include warnings attached to subgraph nodes via the list-column.
  for (j in seq_len(nrow(subgraph_nodes))) {
    w <- subgraph_nodes$warnings[[j]]
    if (length(w)) hits <- c(hits, w)
  }

  unique(hits)
}

#' Split a markdown document into preamble + top-level (`## `) sections.
#'
#' Returns a list with two fields:
#'   - `preamble` — character vector of the lines before the first H2
#'                  (typically the H1 title and any blank line after it).
#'                  Empty character vector when the document has no
#'                  pre-heading content.
#'   - `sections` — named list keyed by H2 heading text (leading `## `
#'                  stripped). Each value is the lines of that section,
#'                  including its heading line.
#'
#' The preamble is returned separately because R's named-list semantics
#' do not let us use `""` as a retrievable key (assignment via `[[""]]`
#' creates an entry but retrieval via `[[""]]` returns NULL), which
#' previously caused the title to be silently dropped during merge.
#'
#' @noRd
split_md_sections <- function(text) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  is_h2 <- grepl("^## ", lines)
  break_idx <- which(is_h2)
  if (!length(break_idx)) {
    return(list(preamble = lines, sections = list()))
  }
  preamble <- if (break_idx[1L] > 1L) {
    lines[seq_len(break_idx[1L] - 1L)]
  } else {
    character()
  }
  sections <- list()
  starts <- break_idx
  ends <- c(starts[-1L] - 1L, length(lines))
  for (k in seq_along(starts)) {
    chunk <- lines[starts[k]:ends[k]]
    heading <- sub("^## ", "", lines[starts[k]])
    sections[[heading]] <- chunk
  }
  list(preamble = preamble, sections = sections)
}

#' Merge a freshly rendered doc stub with an existing on-disk version.
#'
#' Auto-filled sections (`auto_keys`) are taken from `rendered`. Every
#' other section in the existing file is preserved verbatim. New
#' sections introduced by `rendered` (none today, but possible as the
#' schema evolves) are appended after the matching position.
#'
#' Returns the merged markdown text.
#'
#' @noRd
merge_doc_stub <- function(rendered, existing_text) {
  if (is.null(existing_text) || !nzchar(existing_text)) return(rendered$text)

  new <- split_md_sections(rendered$text)
  old <- split_md_sections(existing_text)

  auto <- rendered$auto_keys

  # Preamble (title) always comes from the freshly rendered document.
  # Then walk the new document's section order, taking auto sections
  # from new and non-auto sections (e.g. Reviewers) from old when the
  # old document has them.
  merged <- new$preamble
  for (key in names(new$sections)) {
    chunk <- if (key %in% auto || is.null(old$sections[[key]])) {
      new$sections[[key]]
    } else {
      old$sections[[key]]
    }
    merged <- c(merged, chunk)
  }

  result <- paste(merged, collapse = "\n")
  # `strsplit("X\n", "\n")` drops the trailing empty string, so the
  # split/join round-trip would shave the trailing newline that
  # `rendered$text` ends with by convention. Restore it so the merge is
  # byte-idempotent on pristine inputs (the manifest's hash invariant
  # relies on this).
  if (endsWith(rendered$text, "\n") && !endsWith(result, "\n")) {
    result <- paste0(result, "\n")
  }
  result
}

#' Write a feature/module's doc stub to disk, merging with any existing copy.
#'
#' `inventory` is optional; when supplied, the Functions called and
#' Packages used sections render the per-feature inventory tables.
#'
#' Returns the path that was written.
#'
#' @noRd
write_doc_stub <- function(feature, graph, out_dir, inventory = NULL) {
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  rendered <- render_doc_stub(feature, graph,
                              warnings_in_subgraph(feature, graph),
                              inventory = inventory)
  path <- doc_stub_path(out_dir, feature$name)
  existing <- if (file.exists(path)) {
    paste(readLines(path, warn = FALSE), collapse = "\n")
  } else {
    NULL
  }
  text <- merge_doc_stub(rendered, existing)
  writeLines(text, path)
  path
}

#' Path to a feature/module's doc stub artifact (slugified).
#'
#' @noRd
doc_stub_path <- function(out_dir, name) {
  file.path(out_dir, paste0(slugify_artifact_name(name), ".md"))
}
