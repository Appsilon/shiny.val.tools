#' Stable text for a warning code.
#'
#' Warning codes are the SVT-W*** enumeration. Messages here are
#' intentionally short — auditors read by code; the message exists to
#' aid humans skimming a report. New codes append; existing codes never
#' change wording in a way that would break downstream tooling.
#'
#' @noRd
warning_message <- function(code) {
  switch(
    code,
    "SVT-W001" = "Dynamic input ID",
    "SVT-W002" = "renderUI / insertUI source for input definition",
    "SVT-W003" = "Named access into reactiveValues() resolved by name only",
    "SVT-W004" = "Conditional source() or box::use() inclusion",
    "SVT-W005" = "Whole-namespace box import (pkg[...])",
    "SVT-W006" = "library / box::use overlap with non-imported reference",
    "SVT-W007" = "Duplicate file inclusion via source() and box::use()",
    "SVT-W008" = "library() inside a function body",
    "SVT-W009" = "Metaprogramming construct blocks resolution",
    "SVT-W010" = "Output reassigned",
    "SVT-W101" = "Manifest references unknown node",
    "SVT-W102" = "Root claimed by multiple features",
    "SVT-W103" = "Unclaimed output",
    "SVT-W104" = "Orphan module instance",
    "SVT-W105" = "risk_classification = not_required without rationale",
    "SVT-W201" = "Internal function access (pkg:::fn)",
    "SVT-W202" = "Ambiguous function origin",
    "SVT-W203" = "Unresolved function call",
    "SVT-W204" = "Transitive function call",
    "SVT-W205" = "Category unset for non-trivially used package",
    "SVT-W206" = "Package version unresolved",
    NA_character_
  )
}

#' Detect SVT-W010 — repeated assignment to the same `output$x`.
#'
#' One node per `(namespace, name)` is canonical; the first assignment is
#' the canonical site, and every later one in the same namespace is flagged.
#' Detection is per-file so a name reused across files (e.g. unrelated
#' modules defined in separate sources) does not collide.
#'
#' @noRd
detect_output_reassignment <- function(definitions) {
  outs <- definitions[definitions$kind == "output", , drop = FALSE]
  if (nrow(outs) == 0L) {
    return(tibble::tibble(
      code = character(), file = character(),
      line = integer(), col = integer(), message = character()
    ))
  }

  ns_key <- ifelse(is.na(outs$namespace), "", outs$namespace)
  group_key <- paste(outs$from_file, ns_key, outs$name, sep = "")

  flagged <- logical(nrow(outs))
  seen <- new.env(parent = emptyenv())
  # Stable order: rely on the row order in `definitions` (insertion order
  # from the AST walker — which is line-order per file).
  for (i in seq_len(nrow(outs))) {
    key <- group_key[i]
    if (isTRUE(seen[[key]])) {
      flagged[i] <- TRUE
    } else {
      seen[[key]] <- TRUE
    }
  }

  hits <- outs[flagged, , drop = FALSE]
  if (nrow(hits) == 0L) {
    return(tibble::tibble(
      code = character(), file = character(),
      line = integer(), col = integer(), message = character()
    ))
  }

  tibble::tibble(
    code = rep("SVT-W010", nrow(hits)),
    file = hits$from_file,
    line = as.integer(hits$line),
    col = as.integer(hits$col),
    message = rep(warning_message("SVT-W010"), nrow(hits))
  )
}

#' Empty warnings tibble — the canonical shape for the table.
#'
#' @noRd
empty_warnings <- function() {
  tibble::tibble(
    code = character(), file = character(),
    line = integer(), col = integer(), message = character()
  )
}

#' Detect SVT-W008 — `library()` (or `require()`) inside a function body.
#'
#' One row per offending call. Imports rows already carry the
#' `inside_function` flag from the parser, so we filter and re-shape.
#'
#' @noRd
detect_library_inside_function <- function(imports) {
  if (!nrow(imports)) return(empty_warnings())
  hits <- imports[imports$kind %in% c("library", "require") &
                    isTRUE_vec(imports$inside_function), , drop = FALSE]
  if (!nrow(hits)) return(empty_warnings())
  tibble::tibble(
    code = rep("SVT-W008", nrow(hits)),
    file = hits$from_file,
    line = as.integer(hits$line),
    col = as.integer(hits$col),
    message = rep(warning_message("SVT-W008"), nrow(hits))
  )
}

#' Detect SVT-W005 — whole-namespace `box::use(pkg[...])` imports.
#'
#' One row per such clause. The Imports row's `whole_namespace` column
#' is the foundation; we just surface it.
#'
#' @noRd
detect_whole_namespace_box <- function(imports) {
  if (!nrow(imports)) return(empty_warnings())
  hits <- imports[imports$kind == "box_use" &
                    isTRUE_vec(imports$whole_namespace), , drop = FALSE]
  if (!nrow(hits)) return(empty_warnings())
  tibble::tibble(
    code = rep("SVT-W005", nrow(hits)),
    file = hits$from_file,
    line = as.integer(hits$line),
    col = as.integer(hits$col),
    message = rep(warning_message("SVT-W005"), nrow(hits))
  )
}

#' Detect SVT-W004 — conditional `source()` or `box::use()` inclusion.
#'
#' Both Imports (box_use clauses) and Sources rows carry `conditional`.
#' Each conditional clause gets one warning row.
#'
#' @noRd
detect_conditional_inclusion <- function(imports, sources) {
  hits <- list()
  if (nrow(imports)) {
    box_hits <- imports[imports$kind == "box_use" &
                          isTRUE_vec(imports$conditional), , drop = FALSE]
    if (nrow(box_hits)) {
      hits[[length(hits) + 1L]] <- tibble::tibble(
        code = rep("SVT-W004", nrow(box_hits)),
        file = box_hits$from_file,
        line = as.integer(box_hits$line),
        col = as.integer(box_hits$col),
        message = rep(warning_message("SVT-W004"), nrow(box_hits))
      )
    }
  }
  if (nrow(sources)) {
    src_hits <- sources[isTRUE_vec(sources$conditional), , drop = FALSE]
    if (nrow(src_hits)) {
      hits[[length(hits) + 1L]] <- tibble::tibble(
        code = rep("SVT-W004", nrow(src_hits)),
        file = src_hits$from_file,
        line = as.integer(src_hits$line),
        col = as.integer(src_hits$col),
        message = rep(warning_message("SVT-W004"), nrow(src_hits))
      )
    }
  }
  if (!length(hits)) return(empty_warnings())
  do.call(rbind, hits)
}

#' Detect SVT-W007 — same file reached via `source()` AND `box::use()`.
#'
#' Cross-references resolved targets in Sources (`to_file`) with resolved
#' local-module targets in Imports (`box_use` rows whose `local_path` is
#' present). Files reached by both pathways trigger a warning at every
#' participating call site.
#'
#' @noRd
detect_duplicate_inclusion <- function(imports, sources, app_path) {
  if (!nrow(sources) || !nrow(imports)) return(empty_warnings())
  box_local <- imports[imports$kind == "box_use" &
                         !is.na(imports$local_path), , drop = FALSE]
  if (!nrow(box_local)) return(empty_warnings())

  box_path_base <- discover_box_path(app_path) %||% "."
  resolve_one <- function(file_dir, local_path, absolute) {
    base <- if (isTRUE(absolute)) box_path_base else file_dir
    resolve_box_local(base, local_path)
  }

  box_targets <- vapply(
    seq_len(nrow(box_local)),
    function(i) {
      file_dir <- dirname(box_local$from_file[i])
      target <- resolve_one(file_dir, box_local$local_path[i],
                            box_local$absolute[i])
      target %||% NA_character_
    },
    character(1)
  )
  src_targets <- sources$to_file
  shared <- intersect(box_targets[!is.na(box_targets)],
                      src_targets[!is.na(src_targets) & nzchar(src_targets)])
  if (!length(shared)) return(empty_warnings())

  out <- list()
  for (target in shared) {
    src_rows <- sources[!is.na(sources$to_file) & sources$to_file == target,
                        , drop = FALSE]
    if (nrow(src_rows)) {
      out[[length(out) + 1L]] <- tibble::tibble(
        code = rep("SVT-W007", nrow(src_rows)),
        file = src_rows$from_file,
        line = as.integer(src_rows$line),
        col = as.integer(src_rows$col),
        message = rep(warning_message("SVT-W007"), nrow(src_rows))
      )
    }
    box_rows <- box_local[box_targets == target, , drop = FALSE]
    if (nrow(box_rows)) {
      out[[length(out) + 1L]] <- tibble::tibble(
        code = rep("SVT-W007", nrow(box_rows)),
        file = box_rows$from_file,
        line = as.integer(box_rows$line),
        col = as.integer(box_rows$col),
        message = rep(warning_message("SVT-W007"), nrow(box_rows))
      )
    }
  }
  if (!length(out)) return(empty_warnings())
  do.call(rbind, out)
}

#' Detect SVT-W001 — dynamic input ID (`input[[expr]]` with non-literal key).
#'
#' One row per offending read. The references walker emits these as
#' `kind = "dynamic_input"` so we just surface them.
#'
#' @noRd
detect_dynamic_input_id <- function(references) {
  if (!nrow(references)) return(empty_warnings())
  hits <- references[references$kind == "dynamic_input", , drop = FALSE]
  if (!nrow(hits)) return(empty_warnings())
  tibble::tibble(
    code = rep("SVT-W001", nrow(hits)),
    file = hits$from_file,
    line = as.integer(hits$line),
    col = as.integer(hits$col),
    message = rep(warning_message("SVT-W001"), nrow(hits))
  )
}

#' Detect SVT-W009 — metaprogramming construct.
#'
#' Calls to `do.call`, `eval`, `evalq`, `parse`, `Recall`, `match.call`,
#' `sys.call` block static resolution. The references walker tags these
#' as `kind = "metaprogramming"`.
#'
#' @noRd
detect_metaprogramming <- function(references) {
  if (!nrow(references)) return(empty_warnings())
  hits <- references[references$kind == "metaprogramming", , drop = FALSE]
  if (!nrow(hits)) return(empty_warnings())
  tibble::tibble(
    code = rep("SVT-W009", nrow(hits)),
    file = hits$from_file,
    line = as.integer(hits$line),
    col = as.integer(hits$col),
    message = rep(warning_message("SVT-W009"), nrow(hits))
  )
}

#' Detect SVT-W003 — named access into `reactiveValues()`.
#'
#' Every `rv$name` read carries the inherent imprecision of name-only
#' resolution; surfaced as a per-read warning so auditors know the
#' dependency was inferred, not proven.
#'
#' @noRd
detect_rv_named_access <- function(references) {
  if (!nrow(references)) return(empty_warnings())
  hits <- references[references$kind == "value", , drop = FALSE]
  if (!nrow(hits)) return(empty_warnings())
  tibble::tibble(
    code = rep("SVT-W003", nrow(hits)),
    file = hits$from_file,
    line = as.integer(hits$line),
    col = as.integer(hits$col),
    message = rep(warning_message("SVT-W003"), nrow(hits))
  )
}

#' Helper — coerce possibly-NA logical column to plain logical (NA → FALSE).
#'
#' Filters via `inside_function`, `whole_namespace`, etc. need to drop
#' NAs (which mean "not applicable to this row" in the imports table).
#'
#' @noRd
isTRUE_vec <- function(x) {
  if (is.null(x)) return(logical())
  out <- as.logical(x)
  out[is.na(out)] <- FALSE
  out
}

#' Build the Warnings table for an app.
#'
#' Currently surfaces SVT-W003, W004, W005, W007, W008, W010. Codes still
#' pending: W001 (dynamic input ID), W002 (renderUI/insertUI source for
#' input), W006 (library/box::use overlap with non-imported reference),
#' W009 (metaprogramming).
#'
#' @noRd
build_warnings_table <- function(app_path) {
  imports <- build_imports_table(app_path)
  sources <- build_sources_table(app_path)
  defs <- build_definitions_table(app_path)
  refs <- build_references_table(app_path)

  parts <- list(
    detect_dynamic_input_id(refs),
    detect_output_reassignment(defs),
    detect_library_inside_function(imports),
    detect_whole_namespace_box(imports),
    detect_conditional_inclusion(imports, sources),
    detect_duplicate_inclusion(imports, sources, app_path),
    detect_rv_named_access(refs),
    detect_metaprogramming(refs)
  )
  do.call(rbind, parts)
}
