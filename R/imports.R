#' Walk a parsed expression and yield every static `library()` / `require()` call.
#'
#' Returns a list of records: `list(kind, package, inside_function, line, col)`.
#' `inside_function` is TRUE if the call sits inside any function body —
#' the foundation for SVT-W008. Such calls still count as global imports
#' for our purposes (Shiny apps load on startup; a function body may be
#' the server function itself).
#'
#' Dynamic forms — `library(pkg, character.only = TRUE)` where `pkg` is a
#' variable — are skipped: static analysis cannot resolve them.
#'
#' @noRd
find_library_calls <- function(parsed_expr) {
  acc <- list()

  is_library_or_require <- function(head) {
    is.name(head) && as.character(head) %in% c("library", "require")
  }

  is_box_use <- function(head) {
    is.call(head) && length(head) == 3L &&
      identical(as.character(head[[1L]]), "::") &&
      identical(as.character(head[[2L]]), "box") &&
      identical(as.character(head[[3L]]), "use")
  }

  is_function_def <- function(head) {
    is.name(head) && identical(as.character(head), "function")
  }

  # Top-level expressions carry a single `srcref` attribute. Children of
  # a brace block / control-flow construct have a list-valued `srcref`
  # indexed by child position; walking into them needs to look up the
  # right element rather than reading `attr(child, "srcref")` (which is
  # often NULL).
  pick_srcref <- function(expr) {
    sr <- attr(expr, "srcref")
    if (inherits(sr, "srcref")) sr else NULL
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

  extract_pkg <- function(call) {
    args <- as.list(call)[-1L]
    if (length(args) == 0L) return(NULL)
    argnames <- names(args)
    if (is.null(argnames)) argnames <- rep("", length(args))

    pkg_idx <- which(argnames == "package")
    if (length(pkg_idx)) {
      val <- args[[pkg_idx[1L]]]
    } else {
      positional <- which(argnames == "")
      if (!length(positional)) return(NULL)
      val <- args[[positional[1L]]]
    }

    co_idx <- which(argnames == "character.only")
    char_only <- length(co_idx) > 0L && isTRUE(args[[co_idx[1L]]])
    if (char_only) {
      if (is.character(val) && length(val) == 1L && !is.na(val)) return(val)
      return(NULL)
    }

    if (is.name(val)) return(as.character(val))
    if (is.character(val) && length(val) == 1L && !is.na(val)) return(val)
    NULL
  }

  walk <- function(expr, inside_function, own_srcref) {
    if (!tryCatch({ force(expr); TRUE }, error = function(e) FALSE)) {
      return(invisible())
    }
    if (!is.call(expr)) return(invisible())

    head <- expr[[1L]]
    # box::use(...) args are clause specifiers, not code — and may
    # contain missing-arg sentinels (trailing commas) that error on
    # inspection.
    if (is_box_use(head)) return(invisible())

    if (is_library_or_require(head)) {
      pkg <- extract_pkg(expr)
      if (!is.null(pkg)) {
        loc <- loc_from(own_srcref)
        acc[[length(acc) + 1L]] <<- list(
          kind = as.character(head),
          package = pkg,
          inside_function = inside_function,
          line = loc$line,
          col = loc$col
        )
      }
      return(invisible())
    }

    child_in_fn <- inside_function || is_function_def(head)
    for (i in seq_along(expr)) {
      child <- expr[[i]]
      if (is_missing_arg(child)) next
      if (is.null(child)) next
      if (is.symbol(child) && !nzchar(as.character(child))) next
      walk(child, child_in_fn,
           child_srcref(expr, i) %||% pick_srcref(child) %||% own_srcref)
    }
  }

  for (i in seq_along(parsed_expr)) {
    walk(parsed_expr[[i]],
         inside_function = FALSE,
         own_srcref = pick_srcref(parsed_expr[[i]]) %||%
                       child_srcref(parsed_expr, i))
  }
  acc
}

#' Build the Imports table for an app.
#'
#' One row per static `library()`, `require()`, or `box::use()` clause
#' across every enumerated file. Columns:
#'   - `from_file`, `kind` ("library" | "require" | "box_use")
#'   - `package`, `alias` (NA when absent), `function_set` (list-col),
#'     `whole_namespace` (lgl, TRUE for `box::use(pkg[...])`),
#'     `local_path` (NA when not a local-module clause),
#'     `absolute` (lgl, TRUE for absolute box paths like `app/view/home`,
#'     FALSE for `./mod` / `../mod`, NA for everything else)
#'   - `conditional` (lgl, foundation for SVT-W004 — only meaningful for
#'     box::use rows; library/require rows are always FALSE here)
#'   - `inside_function` (lgl, foundation for SVT-W008 — only meaningful
#'     for library/require rows)
#'   - `line`, `col`
#'
#' @noRd
build_imports_table <- function(app_path) {
 svt_memoize(paste0("imports\x1f", app_path), function() {
  files <- enumerate_app_files(app_path)

  from_files <- character()
  kinds <- character()
  packages <- character()
  aliases <- character()
  function_sets <- list()
  whole_namespaces <- logical()
  local_paths <- character()
  absolutes <- logical()
  conditionals <- logical()
  inside_fns <- logical()
  lines <- integer()
  cols <- integer()

  for (f in files) {
    parsed <- parse_file(app_path, f)
    if (is.null(parsed)) next

    for (call in find_library_calls(parsed)) {
      from_files <- c(from_files, f)
      kinds <- c(kinds, call$kind)
      packages <- c(packages, call$package)
      aliases <- c(aliases, NA_character_)
      function_sets <- c(function_sets, list(character()))
      whole_namespaces <- c(whole_namespaces, FALSE)
      local_paths <- c(local_paths, NA_character_)
      absolutes <- c(absolutes, NA)
      conditionals <- c(conditionals, FALSE)
      inside_fns <- c(inside_fns, call$inside_function)
      lines <- c(lines, as.integer(call$line))
      cols <- c(cols, as.integer(call$col))
    }

    for (clause in find_box_use_calls(parsed)) {
      from_files <- c(from_files, f)
      kinds <- c(kinds, "box_use")
      packages <- c(packages, clause$package %||% NA_character_)
      aliases <- c(aliases, clause$alias %||% NA_character_)
      function_sets <- c(function_sets, list(clause$function_set))
      whole_namespaces <- c(whole_namespaces, clause$whole_namespace)
      local_paths <- c(local_paths, clause$local_path %||% NA_character_)
      absolutes <- c(absolutes, clause$absolute)
      conditionals <- c(conditionals, clause$conditional)
      inside_fns <- c(inside_fns, FALSE)
      lines <- c(lines, as.integer(clause$line))
      cols <- c(cols, as.integer(clause$col))
    }
  }

  tibble::tibble(
    from_file = from_files,
    kind = kinds,
    package = packages,
    alias = aliases,
    function_set = function_sets,
    whole_namespace = whole_namespaces,
    local_path = local_paths,
    absolute = absolutes,
    conditional = conditionals,
    inside_function = inside_fns,
    line = lines,
    col = cols
  )
 })
}
