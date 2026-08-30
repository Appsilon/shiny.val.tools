# Shared AST-walking primitives
# ------------------------------
#
# Every table builder in spec 01 walks the same base-R parse tree looking
# for a different set of patterns, and each one needs the same handful of
# primitives to do it: where am I in the source, is this call `box::use`,
# is this a function definition, is this a `moduleServer()` call.
#
# These lived as local closures inside each walker, re-declared per file —
# seven byte-identical copies of the srcref trio alone, and two copies of
# `classify_rhs()` that had to be kept in step by hand. One definition per
# primitive removes that hazard; the walkers keep only what is genuinely
# specific to the pattern they recognize.
#
# All of these operate on parse-tree objects, never on evaluated values.
# Nothing here evaluates user code.

# -- source locations ----------------------------------------------------

#' The `srcref` attached directly to an expression, or NULL.
#'
#' Top-level expressions carry a single `srcref` attribute. Children of a
#' brace block or control-flow construct do not — see `child_srcref()`.
#'
#' @noRd
pick_srcref <- function(expr) {
  sr <- attr(expr, "srcref")
  if (inherits(sr, "srcref")) return(sr)
  NULL
}

#' The `srcref` for `parent_expr[[i]]`, or NULL.
#'
#' Children of a brace block / control-flow construct carry their srcrefs
#' as a list on the parent, indexed by child position; reading
#' `attr(child, "srcref")` on them is usually NULL, so a descent has to
#' look the right element up here.
#'
#' @noRd
child_srcref <- function(parent_expr, i) {
  sr_list <- attr(parent_expr, "srcref")
  if (is.list(sr_list) && i >= 1L && i <= length(sr_list)) {
    candidate <- sr_list[[i]]
    if (inherits(candidate, "srcref")) return(candidate)
  }
  NULL
}

#' `list(line, col)` from a `srcref`, or NAs when there is none.
#'
#' @noRd
loc_from <- function(srcref) {
  if (is.null(srcref)) return(list(line = NA_integer_, col = NA_integer_))
  s <- as.integer(srcref)
  list(line = s[1L], col = s[2L])
}

# -- call-head predicates ------------------------------------------------

#' Is this call head `box::use`?
#'
#' Matched syntactically. `box::use()` is never evaluated — its argument
#' list is a DSL, not R semantics, so walkers descend into it deliberately
#' or (more often) skip it outright.
#'
#' @noRd
is_box_use <- function(head) {
  is.call(head) && length(head) == 3L &&
    identical(as.character(head[[1L]]), "::") &&
    identical(as.character(head[[2L]]), "box") &&
    identical(as.character(head[[3L]]), "use")
}

#' Is this call head a construct that may suppress its body at runtime?
#'
#' What SVT-W004 ("conditional inclusion") keys on: a `source()` or
#' `box::use()` beneath one of these may or may not run.
#'
#' @noRd
is_control_flow <- function(head) {
  if (!is.name(head)) return(FALSE)
  as.character(head) %in% c("if", "for", "while", "repeat", "switch",
                            "tryCatch")
}

#' Is this call head one of R's assignment operators?
#'
#' @noRd
is_assignment <- function(head) {
  is.name(head) && as.character(head) %in% c("<-", "=", "<<-")
}

#' Is this call head the `function` keyword?
#'
#' Operates on the **head**, unlike `is_function_def()`, which takes the
#' whole expression. Both forms are needed and the distinction is easy to
#' lose, so they carry different names.
#'
#' @noRd
is_function_head <- function(head) {
  is.name(head) && identical(as.character(head), "function")
}

#' Is this expression a function definition — `function(args) body`?
#'
#' Takes the **whole expression**, not its head; see `is_function_head()`.
#'
#' @noRd
is_function_def <- function(expr) {
  is.call(expr) && length(expr) >= 3L && is.name(expr[[1L]]) &&
    as.character(expr[[1L]]) == "function"
}

#' Is this call head `moduleServer` or `shiny::moduleServer`?
#'
#' The qualified form matters: a package that imports rather than attaches
#' Shiny writes `shiny::moduleServer(...)`, and a walker that only matched
#' the bare name would walk the module body without establishing its
#' namespace — producing a module with no edges (spec 02, "Qualified
#' `moduleServer()`").
#'
#' @noRd
is_module_server_call <- function(head) {
  if (is.name(head)) return(identical(as.character(head), "moduleServer"))
  if (is.call(head) && length(head) == 3L &&
      as.character(head[[1L]]) %in% c("::", ":::") &&
      identical(as.character(head[[2L]]), "shiny") &&
      identical(as.character(head[[3L]]), "moduleServer")) {
    return(TRUE)
  }
  FALSE
}

#' The bare name of a call's head — `fn` for both `fn()` and `pkg::fn()`.
#'
#' NA when the head is neither a symbol nor a `::` / `:::` access.
#'
#' @noRd
callee_name <- function(head) {
  if (is.name(head)) return(as.character(head))
  if (is.call(head) && length(head) == 3L &&
      as.character(head[[1L]]) %in% c("::", ":::")) {
    return(as.character(head[[3L]]))
  }
  NA_character_
}

# -- shape predicates ----------------------------------------------------

#' Does this function definition have a Shiny module server's signature?
#'
#' `function(input, output, session, ...)` — the legacy `callModule()`
#' form. The top-level app server has the same shape, so a caller must
#' additionally require that the function is assigned to a name before
#' treating it as a module.
#'
#' @noRd
has_module_signature <- function(fn_expr) {
  if (!is_function_def(fn_expr)) return(FALSE)
  arg_names <- names(as.list(fn_expr[[2L]]))
  if (length(arg_names) < 3L) return(FALSE)
  identical(arg_names[1L:3L], c("input", "output", "session"))
}

#' Formal names of a function definition, minus the `...` sentinel.
#'
#' A module's wrapper takes `id` plus whatever the module needs passed in;
#' a scaffold that omits those arguments does not run, which is exactly
#' the harness boilerplate spec 06 set out to remove.
#'
#' @noRd
formal_names <- function(fn_expr) {
  if (!is_function_def(fn_expr)) return(character())
  nms <- names(as.list(fn_expr[[2L]]))
  nms <- nms[nzchar(nms) & nms != "..."]
  as.character(nms)
}

#' The bound name on the LHS of a named binding, or NULL.
#'
#' `name <- ...` and `name = ...` both put a symbol on the LHS. Non-symbol
#' LHSs (element assignments such as `x[[1]] <- reactive(...)`) are not
#' modelled as named bindings.
#'
#' @noRd
binding_name <- function(lhs) {
  if (is.name(lhs)) {
    nm <- as.character(lhs)
    if (nzchar(nm)) return(nm)
  }
  NULL
}

#' The output name on the LHS of an `output$x <- ...` assignment, or NULL.
#'
#' `output$x` parses as `$(output, x)`; `output[["x"]]` as
#' `[[(output, "x")]]`. Dynamic keys (`output[[expr]]`) return NULL —
#' static analysis cannot resolve them.
#'
#' @noRd
output_name <- function(lhs) {
  if (!is.call(lhs) || length(lhs) != 3L) return(NULL)
  op <- as.character(lhs[[1L]])
  base <- lhs[[2L]]
  key <- lhs[[3L]]
  if (!(is.name(base) && identical(as.character(base), "output"))) return(NULL)
  if (op == "$" && is.name(key) && nzchar(as.character(key))) {
    return(as.character(key))
  }
  if (op == "[[" && is.character(key) && length(key) == 1L &&
      !is.na(key) && nzchar(key)) {
    return(key)
  }
  NULL
}

# -- RHS classification --------------------------------------------------

svt_reactive_callees <- c("reactive", "eventReactive")
svt_observer_callees <- c("observe", "observeEvent", "downloadHandler")

#' The node kind a call produces as the RHS of a named binding.
#'
#' Returns `"reactive"`, `"observer"`, or NULL. `bindEvent(x, ...)`
#' delegates to its first argument: `bindEvent(reactive(...), ...)` is
#' still a reactive, `bindEvent(observe(...), ...)` still an observer.
#'
#' @noRd
classify_rhs <- function(rhs) {
  nm <- if (is.call(rhs)) callee_name(rhs[[1L]]) else NA_character_
  if (is.na(nm)) return(NULL)
  if (nm %in% svt_reactive_callees) return("reactive")
  if (nm %in% svt_observer_callees) return("observer")
  if (nm == "bindEvent" && length(rhs) >= 2L) return(classify_rhs(rhs[[2L]]))
  NULL
}

#' The bare call name on the RHS of a definition — `renderPlot`,
#' `eventReactive`, `downloadHandler`, ...
#'
#' `pkg::fn` yields `fn`, and `bindEvent(x, ...)` delegates to `x` exactly
#' as `classify_rhs()` does, so the recorded call is the one that
#' determines the node's behaviour rather than the wrapper. This is what
#' tells spec 06 whether an observable is opaque under `testServer()`.
#' NA when the RHS is not a call.
#'
#' @noRd
rhs_call_name <- function(rhs) {
  nm <- if (is.call(rhs)) callee_name(rhs[[1L]]) else NA_character_
  if (is.na(nm)) return(NA_character_)
  if (nm == "bindEvent" && length(rhs) >= 2L) return(rhs_call_name(rhs[[2L]]))
  nm
}

# -- descent guard -------------------------------------------------------

#' Should a walker descend into this child expression?
#'
#' Filters the slots that are not real subexpressions: R's missing-arg
#' sentinel (from trailing commas and empty formals), NULL, and the empty
#' symbol. The missing-arg test must run first — `is.null()` forces its
#' argument and would error on the sentinel.
#'
#' @noRd
walkable <- function(child) {
  if (is_missing_arg(child)) return(FALSE)
  if (is.null(child)) return(FALSE)
  if (is.symbol(child) && !nzchar(as.character(child))) return(FALSE)
  TRUE
}
