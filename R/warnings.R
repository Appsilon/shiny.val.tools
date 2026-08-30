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
    "SVT-W301" = "Feature or module has no mapped test",
    "SVT-W302" = "Partial coverage: some observables or stimuli unexercised",
    "SVT-W303" = "Scaffold present but not filled in",
    "SVT-W304" = "Orphan test: maps to no feature, module, or helper",
    "SVT-W305" = "@covers annotation references an unknown target",
    # SVT-W306 is retired: it described the exportTestValues plumbing the
    # design needed while it still generated shinytest2 scaffolds. Per the
    # stability contract a retired code is never reused; SVT-W312 replaces it.
    "SVT-W307" = "Test surface changed since the covering test was last modified",
    "SVT-W308" = "Test surface incomplete: static-analysis blockers in the subgraph",
    "SVT-W309" = "Scaffold target already exists in the app test tree; skipped",
    "SVT-W310" = "Mapped test failed or is absent from the ingested result report",
    "SVT-W311" = "verification = not_required without rationale_verification",
    "SVT-W312" = "Observable is opaque under testServer(); assert its structure and test the helper that computed it",
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
    line = integer(), col = integer(), message = character(),
    node_id = character()
  )
}

#' Normalize a detector's output to the canonical warnings shape.
#'
#' Detectors that cannot attribute a warning to a graph node omit the
#' `node_id` column; it is filled with NA here so every part `rbind()`s.
#' `node_id` is what lets spec 06 decide whether a static-analysis limit
#' falls inside a given feature's closure.
#'
#' @noRd
with_node_id <- function(tbl) {
  if (!"node_id" %in% names(tbl)) {
    tbl$node_id <- rep(NA_character_, nrow(tbl))
  }
  tbl
}

#' The node id owning a reference row, or NA when the read sits outside
#' any definition (e.g. directly in a server body).
#'
#' @noRd
ref_owner_node_id <- function(refs) {
  ifelse(
    is.na(refs$in_def_kind) | refs$in_def_kind == "module_server",
    NA_character_,
    node_id(refs$in_def_kind, refs$in_def_namespace, NA_character_,
            refs$in_def_name)
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
    message = rep(warning_message("SVT-W001"), nrow(hits)),
    node_id = ref_owner_node_id(hits)
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
    message = rep(warning_message("SVT-W009"), nrow(hits)),
    node_id = ref_owner_node_id(hits)
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
    message = rep(warning_message("SVT-W003"), nrow(hits)),
    node_id = ref_owner_node_id(hits)
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
#' Currently surfaces SVT-W001..W005 and W007..W010. W006
#' (library/box::use overlap with non-imported reference) is still pending.
#'
#' Columns: `code`, `file`, `line`, `col`, `message`, `node_id`. `node_id`
#' is the graph node the warning is about, or NA when the site belongs to
#' no node; spec 06 reads it to decide which surfaces a static-analysis
#' limit makes incomplete.
#'
#' @noRd
build_warnings_table <- function(app_path) {
 svt_memoize(paste0("warnings\x1f", app_path), function() {
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
    detect_metaprogramming(refs),
    detect_render_ui_input(app_path)
  )
  do.call(rbind, lapply(parts, with_node_id))
 })
}

#' Walk a parsed expression for input controls created inside a render context.
#'
#' The SVT-W002 case: `output$x <- renderUI({ numericInput("y", ...) })`
#' creates the input `y` only once that UI renders. No static definition
#' site describes it, so the graph cannot see it and a test built from the
#' derived stimulus list would be missing a control.
#'
#' Detection is deliberately narrow. A **literal** id (a string, or
#' `ns("literal")`) is required: a computed id inside `renderUI()` is
#' already SVT-W001 territory, and an input control outside a render
#' context is an ordinary static input.
#'
#' Returns a list of records:
#' `list(name, in_def_kind, in_def_name, in_def_namespace, line, col)`.
#'
#' @noRd
find_render_ui_inputs <- function(parsed_expr, from_file = NA_character_) {
  acc <- list()

  render_callees <- c("renderUI", "insertUI", "appendTab", "prependTab",
                      "insertTab")

  pick_srcref <- function(expr) {
    sr <- attr(expr, "srcref")
    if (inherits(sr, "srcref")) return(sr)
    NULL
  }

  child_srcref <- function(parent_expr, i) {
    sr_list <- attr(parent_expr, "srcref")
    if (is.list(sr_list) && i >= 1L && i <= length(sr_list)) {
      candidate <- sr_list[[i]]
      if (inherits(candidate, "srcref")) return(candidate)
    }
    NULL
  }

  loc_from <- function(srcref) {
    if (is.null(srcref)) return(list(line = NA_integer_, col = NA_integer_))
    s <- as.integer(srcref)
    list(line = s[1L], col = s[2L])
  }

  callee_name <- function(head) {
    if (is.name(head)) return(as.character(head))
    if (is.call(head) && length(head) == 3L &&
        as.character(head[[1L]]) %in% c("::", ":::")) {
      return(as.character(head[[3L]]))
    }
    NA_character_
  }

  # Shiny's input constructors are `*Input()` by convention, plus a short
  # list of controls that do not follow it.
  is_input_control <- function(nm) {
    if (is.na(nm)) return(FALSE)
    grepl("Input$", nm) ||
      nm %in% c("actionButton", "actionLink", "radioButtons",
                "checkboxGroupInput", "submitButton")
  }

  # A literal id: `"threshold"` or `ns("threshold")`.
  literal_id <- function(arg) {
    if (is.character(arg) && length(arg) == 1L && !is.na(arg) && nzchar(arg)) {
      return(arg)
    }
    if (is.call(arg) && length(arg) == 2L &&
        identical(callee_name(arg[[1L]]), "ns")) {
      inner <- arg[[2L]]
      if (is.character(inner) && length(inner) == 1L && nzchar(inner)) {
        return(inner)
      }
    }
    NULL
  }

  walk <- function(expr, own_srcref, in_def, in_render) {
    if (!tryCatch({ force(expr); TRUE }, error = function(e) FALSE)) {
      return(invisible())
    }
    if (!is.call(expr)) return(invisible())

    head <- expr[[1L]]
    nm <- callee_name(head)

    if (in_render && is_input_control(nm) && length(expr) >= 2L) {
      id <- literal_id(expr[[2L]])
      if (!is.null(id)) {
        loc <- loc_from(own_srcref)
        acc[[length(acc) + 1L]] <<- list(
          name = id,
          in_def_kind = in_def$kind %||% NA_character_,
          in_def_name = in_def$name %||% NA_character_,
          in_def_namespace = in_def$namespace %||% NA_character_,
          line = loc$line,
          col = loc$col
        )
      }
    }

    # Entering a render context. Everything below it is dynamic UI.
    child_render <- in_render || (!is.na(nm) && nm %in% render_callees)

    if (is.name(head) && as.character(head) %in% c("<-", "=", "<<-") &&
        length(expr) == 3L) {
      def <- enclosing_def_for(expr[[2L]], in_def)
      for (i in seq_along(expr)) {
        child <- expr[[i]]
        if (!walkable(child)) next
        walk(child, child_srcref(expr, i) %||% pick_srcref(child) %||% own_srcref,
             if (i == 3L) def else in_def, child_render)
      }
      return(invisible())
    }

    for (i in seq_along(expr)) {
      child <- expr[[i]]
      if (!walkable(child)) next
      walk(child, child_srcref(expr, i) %||% pick_srcref(child) %||% own_srcref,
           in_def, child_render)
    }
  }

  empty_def <- list(kind = NA_character_, name = NA_character_,
                    namespace = NA_character_)
  for (i in seq_along(parsed_expr)) {
    walk(parsed_expr[[i]],
         pick_srcref(parsed_expr[[i]]) %||% child_srcref(parsed_expr, i),
         in_def = empty_def, in_render = FALSE)
  }
  acc
}

#' @noRd
walkable <- function(child) {
  # The missing-arg sentinel must be tested before anything forces
  # `child`; `is.null()` would error on it.
  if (is_missing_arg(child)) return(FALSE)
  if (is.null(child)) return(FALSE)
  if (is.symbol(child) && !nzchar(as.character(child))) return(FALSE)
  TRUE
}

#' The definition an assignment's RHS belongs to, for W002 attribution.
#'
#' `output$x <- renderUI(...)` owns its render body; any other assignment
#' leaves the enclosing definition unchanged.
#'
#' @noRd
enclosing_def_for <- function(lhs, in_def) {
  if (is.call(lhs) && length(lhs) == 3L &&
      as.character(lhs[[1L]]) %in% c("$", "[[") &&
      is.name(lhs[[2L]]) && identical(as.character(lhs[[2L]]), "output")) {
    key <- lhs[[3L]]
    nm <- if (is.name(key)) as.character(key) else if (is.character(key)) key[1L] else NA_character_
    if (!is.na(nm) && nzchar(nm)) {
      return(list(kind = "output", name = nm,
                  namespace = in_def$namespace %||% NA_character_))
    }
  }
  in_def
}

#' Detect SVT-W002 — an input created inside a render context.
#'
#' `node_id` points at the definition whose body creates the input — the
#' node a feature closure will contain — so spec 06 can surface the code
#' as a blocker on exactly the surfaces it makes incomplete.
#'
#' @noRd
detect_render_ui_input <- function(app_path) {
  files <- enumerate_app_files(app_path)

  codes <- character(); out_files <- character()
  lines <- integer(); cols <- integer(); ids <- character()

  for (f in files) {
    parsed <- parse_file(app_path, f)
    if (is.null(parsed)) next
    for (hit in find_render_ui_inputs(parsed, from_file = f)) {
      owner <- if (is.na(hit$in_def_kind)) {
        NA_character_
      } else {
        node_id(hit$in_def_kind, hit$in_def_namespace, NA_character_,
                hit$in_def_name)
      }
      codes <- c(codes, "SVT-W002")
      out_files <- c(out_files, f)
      lines <- c(lines, as.integer(hit$line))
      cols <- c(cols, as.integer(hit$col))
      ids <- c(ids, owner)
    }
  }

  if (!length(codes)) return(empty_warnings())
  tibble::tibble(
    code = codes,
    file = out_files,
    line = lines,
    col = cols,
    message = rep(warning_message("SVT-W002"), length(codes)),
    node_id = ids
  )
}
