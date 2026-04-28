#' Walk a parsed expression and yield every clause inside `box::use(...)`.
#'
#' One record per clause. Each record carries:
#'   - `package`: the package name (NA for local modules)
#'   - `alias`: the alias name (NA when no alias was given)
#'   - `function_set`: character vector of explicitly imported functions
#'     (length 0 when not narrowed)
#'   - `whole_namespace`: TRUE for `pkg[...]` (foundation for SVT-W005)
#'   - `local_path`: the as-written path for `./mod` / `../mod` /
#'     `seg/seg/...` clauses (NA for package clauses)
#'   - `absolute`: TRUE for absolute box paths (no leading `.` / `..`),
#'     FALSE for relative ones; NA for package clauses
#'   - `conditional`: TRUE when the enclosing `box::use(...)` sits inside
#'     a conditional construct (foundation for SVT-W004)
#'   - `line`, `col`: the location of the enclosing `box::use(...)` call
#'
#' Malformed clauses are silently skipped — the typical cause is a
#' trailing comma (missing arg).
#'
#' @noRd
find_box_use_calls <- function(parsed_expr) {
  acc <- list()

  is_box_use <- function(head) {
    is.call(head) && length(head) == 3L &&
      identical(as.character(head[[1L]]), "::") &&
      identical(as.character(head[[2L]]), "box") &&
      identical(as.character(head[[3L]]), "use")
  }

  is_control_flow <- function(head) {
    is.name(head) && as.character(head) %in%
      c("if", "for", "while", "repeat", "switch", "tryCatch")
  }

  pick_srcref <- function(expr) {
    sr <- attr(expr, "srcref")
    if (inherits(sr, "srcref")) sr else NULL
  }

  # Children of a brace block / control-flow construct carry their
  # srcrefs as a list on the parent indexed by position. Walk descents
  # need to look up the right element here.
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

  flatten_path <- function(expr) {
    if (is.name(expr)) {
      nm <- as.character(expr)
      if (!nzchar(nm)) return(NULL)
      return(nm)
    }
    if (is.call(expr) && length(expr) == 3L &&
        identical(as.character(expr[[1L]]), "/")) {
      lhs <- flatten_path(expr[[2L]])
      rhs <- flatten_path(expr[[3L]])
      if (is.null(lhs) || is.null(rhs)) return(NULL)
      return(c(lhs, rhs))
    }
    NULL
  }

  # `./mod[fn]` parses as `/( ., [(mod, fn) )` because `[` binds tighter
  # than `/`. So we peel a `[` off the **rightmost** leaf of a `/` chain,
  # not just off the top-level expression.
  peel_bracket <- function(expr) {
    if (is.call(expr) && length(expr) >= 2L &&
        identical(as.character(expr[[1L]]), "[")) {
      return(list(target = expr[[2L]],
                  bracket_args = as.list(expr)[-c(1L, 2L)]))
    }
    if (is.call(expr) && length(expr) == 3L &&
        identical(as.character(expr[[1L]]), "/")) {
      rhs_peel <- peel_bracket(expr[[3L]])
      if (is.null(rhs_peel$bracket_args)) {
        return(list(target = expr, bracket_args = NULL))
      }
      new_target <- as.call(list(as.name("/"), expr[[2L]], rhs_peel$target))
      return(list(target = new_target, bracket_args = rhs_peel$bracket_args))
    }
    list(target = expr, bracket_args = NULL)
  }

  parse_clause <- function(value, alias) {
    # Trailing comma → missing-arg sentinel; force() errors, we skip.
    if (!tryCatch({ force(value); TRUE }, error = function(e) FALSE)) {
      return(NULL)
    }
    if (is.name(value) && !nzchar(as.character(value))) return(NULL)

    function_set <- character()
    whole_namespace <- FALSE

    peeled <- peel_bracket(value)
    target <- peeled$target
    bracket_args <- peeled$bracket_args

    if (!is.null(bracket_args)) {
      if (length(bracket_args) == 1L && is.name(bracket_args[[1L]]) &&
          identical(as.character(bracket_args[[1L]]), "...")) {
        whole_namespace <- TRUE
      } else {
        names_of_fns <- vapply(
          bracket_args,
          function(a) if (is.name(a)) as.character(a) else NA_character_,
          character(1)
        )
        function_set <- names_of_fns[!is.na(names_of_fns) & nzchar(names_of_fns)]
      }
    }

    if (is.name(target)) {
      pkg <- as.character(target)
      if (!nzchar(pkg)) return(NULL)
      return(list(
        package = pkg,
        alias = alias,
        function_set = function_set,
        whole_namespace = whole_namespace,
        local_path = NA_character_,
        absolute = NA
      ))
    }

    path_parts <- flatten_path(target)
    if (!is.null(path_parts) && length(path_parts) >= 2L) {
      is_relative <- path_parts[1L] %in% c(".", "..")
      return(list(
        package = NA_character_,
        alias = alias,
        function_set = function_set,
        whole_namespace = whole_namespace,
        local_path = paste(path_parts, collapse = "/"),
        absolute = !is_relative
      ))
    }

    NULL
  }

  walk <- function(expr, own_srcref, conditional) {
    if (!tryCatch({ force(expr); TRUE }, error = function(e) FALSE)) {
      return(invisible())
    }
    if (!is.call(expr)) return(invisible())

    head <- expr[[1L]]
    if (is_box_use(head)) {
      args <- as.list(expr)[-1L]
      arg_names <- names(args)
      if (is.null(arg_names)) arg_names <- rep("", length(args))
      loc <- loc_from(own_srcref)

      for (i in seq_along(args)) {
        alias <- if (nzchar(arg_names[i])) arg_names[i] else NA_character_
        clause <- parse_clause(args[[i]], alias)
        if (is.null(clause)) next
        clause$conditional <- conditional
        clause$line <- loc$line
        clause$col <- loc$col
        acc[[length(acc) + 1L]] <<- clause
      }
      return(invisible())
    }

    child_cond <- conditional || is_control_flow(head)
    for (i in seq_along(expr)) {
      child <- expr[[i]]
      if (is_missing_arg(child)) next
      if (is.null(child)) next
      if (is.symbol(child) && !nzchar(as.character(child))) next
      walk(child,
           child_srcref(expr, i) %||% pick_srcref(child) %||% own_srcref,
           child_cond)
    }
  }

  for (i in seq_along(parsed_expr)) {
    walk(parsed_expr[[i]],
         own_srcref = pick_srcref(parsed_expr[[i]]) %||%
                       child_srcref(parsed_expr, i),
         conditional = FALSE)
  }
  acc
}

#' Find every `options(box.path = ...)` call in a parsed expression.
#'
#' Returns a list of records: `list(value_expr, line, col)`. Used to detect
#' rhino-style absolute box imports — rhino's `.Rprofile` typically sets
#' `options(box.path = getwd())` so `box::use(view/home)` resolves relative
#' to the project root. The `value_expr` is the unevaluated RHS of the
#' option (commonly `quote(getwd())` or a string literal).
#'
#' Path resolution that honors `box.path` is handled in a follow-up slice;
#' this helper just records the fact.
#'
#' @noRd
find_box_path_options <- function(parsed_expr) {
  acc <- list()

  is_options_call <- function(head) {
    if (is.name(head)) return(identical(as.character(head), "options"))
    if (is.call(head) && length(head) == 3L) {
      op <- as.character(head[[1L]])
      if (op %in% c("::", ":::")) {
        return(identical(as.character(head[[2L]]), "base") &&
                 identical(as.character(head[[3L]]), "options"))
      }
    }
    FALSE
  }

  pick_srcref <- function(expr) {
    sr <- attr(expr, "srcref")
    if (inherits(sr, "srcref")) sr else NULL
  }

  walk <- function(expr, parent_srcref) {
    if (!tryCatch({ force(expr); TRUE }, error = function(e) FALSE)) {
      return(invisible())
    }
    own_sr <- pick_srcref(expr) %||% parent_srcref
    if (!is.call(expr)) return(invisible())

    head <- expr[[1L]]
    if (is_options_call(head)) {
      args <- as.list(expr)[-1L]
      argnames <- names(args)
      if (is.null(argnames)) argnames <- rep("", length(args))
      box_idx <- which(argnames == "box.path")
      if (length(box_idx)) {
        val_expr <- args[[box_idx[1L]]]
        s <- if (!is.null(own_sr)) as.integer(own_sr) else c(NA_integer_, NA_integer_)
        acc[[length(acc) + 1L]] <<- list(
          value_expr = val_expr,
          line = s[1L],
          col = s[2L]
        )
      }
      return(invisible())
    }

    children <- as.list(expr)
    for (i in seq_along(children)) {
      walk(children[[i]], own_sr)
    }
  }

  for (i in seq_along(parsed_expr)) {
    walk(parsed_expr[[i]], parent_srcref = NULL)
  }
  acc
}

#' Discover the app's `box.path` setting from `.Rprofile`.
#'
#' Parses `.Rprofile` at the app root and looks for
#' `options(box.path = ...)`. Returns:
#'   - `"."` when the RHS is `getwd()` — the rhino convention; absolute box
#'     paths resolve against the app root.
#'   - `NULL` otherwise (no `.Rprofile`, no `box.path` setting, or an
#'     expression we don't honor — e.g. string literals, which we can't
#'     reliably relate to our app-relative model in v1).
#'
#' Callers default to app-root resolution when this returns `NULL`; rhino's
#' convention is dominant enough that allowing absolute paths by default
#' is the pragmatic choice.
#'
#' @noRd
discover_box_path <- function(app_path) {
  rprofile <- file.path(app_path, ".Rprofile")
  if (!file.exists(rprofile)) return(NULL)

  parsed <- tryCatch(
    parse(file = rprofile, keep.source = TRUE),
    error = function(e) NULL
  )
  if (is.null(parsed)) return(NULL)

  hits <- find_box_path_options(parsed)
  if (!length(hits)) return(NULL)

  # First hit wins. We have no reliable way to disambiguate competing
  # box.path settings, so we honor the first one encountered.
  expr <- hits[[1L]]$value_expr
  if (is.call(expr) && length(expr) >= 1L &&
      identical(as.character(expr[[1L]]), "getwd")) {
    return(".")
  }
  NULL
}

#' Resolve a box-local clause path (`./mod`, `../mod`) to a relpath into the app.
#'
#' `local_path` is the as-written path (e.g. `"./modules/foo"`). We strip a
#' leading `.` segment, append `.R` to the final segment, then lex-normalize
#' against the calling file's directory. Returns `NULL` when resolution
#' produces an empty path.
#'
#' @noRd
resolve_box_local <- function(file_dir, local_path) {
  parts <- strsplit(local_path, "/", fixed = TRUE)[[1L]]
  if (length(parts) > 0L && parts[1L] == ".") parts <- parts[-1L]
  if (!length(parts)) return(NULL)
  parts[length(parts)] <- paste0(parts[length(parts)], ".R")
  resolved <- normalize_relpath(file_dir, paste(parts, collapse = "/"))
  if (!nzchar(resolved)) NULL else resolved
}
