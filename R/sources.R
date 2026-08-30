#' Walk a parsed expression and yield every static `source()` call.
#'
#' Returns a list of records: `list(path, conditional, line, col)`.
#' A call's `conditional` flag is TRUE if it sits inside any
#' `if`/`for`/`while`/`repeat`/`switch`/`tryCatch` body — those are the
#' constructs that may suppress the call at runtime, which is what
#' SVT-W004 cares about.
#'
#' Dynamic paths (`source(my_var)`, `source(paste0(...))`) are skipped:
#' static analysis cannot resolve them, so they don't enter the table.
#'
#' @noRd
find_source_calls <- function(parsed_expr) {
  acc <- list()

  is_source_fn <- function(head) {
    if (is.name(head)) return(identical(as.character(head), "source"))
    if (is.call(head) && length(head) == 3L) {
      op <- as.character(head[[1L]])
      if (op %in% c("::", ":::")) {
        return(identical(as.character(head[[2L]]), "base") &&
                 identical(as.character(head[[3L]]), "source"))
      }
    }
    FALSE
  }

  extract_path <- function(call) {
    args <- as.list(call)[-1L]
    if (length(args) == 0L) return(NULL)
    argnames <- names(args)
    if (is.null(argnames)) argnames <- rep("", length(args))

    file_idx <- which(argnames == "file")
    if (length(file_idx)) {
      val <- args[[file_idx[1L]]]
    } else {
      positional <- which(argnames == "")
      if (!length(positional)) return(NULL)
      val <- args[[positional[1L]]]
    }
    if (is.character(val) && length(val) == 1L && !is.na(val)) val else NULL
  }

  # `chdir` defaults to FALSE per R semantics. We honor only literal
  # `TRUE` / `FALSE` — any expression we can't statically evaluate (a
  # variable, a function call) falls back to FALSE since "default
  # behavior" is the safer assumption.
  extract_chdir <- function(call) {
    args <- as.list(call)[-1L]
    if (!length(args)) return(FALSE)
    argnames <- names(args)
    if (is.null(argnames)) argnames <- rep("", length(args))
    idx <- which(argnames == "chdir")
    if (!length(idx)) return(FALSE)
    val <- args[[idx[1L]]]
    isTRUE(val)
  }

  walk <- function(expr, conditional, own_srcref) {
    # Guard against missing-arg sentinels (trailing commas in calls):
    # `force()` errors on R_MissingArg but is a no-op for real values.
    if (!tryCatch({ force(expr); TRUE }, error = function(e) FALSE)) {
      return(invisible())
    }
    if (!is.call(expr)) return(invisible())

    head <- expr[[1L]]
    # Don't descend into box::use(...) args — those are declarative clause
    # specifiers, not executable code, and trailing commas there create
    # missing-arg sentinels we can't safely inspect.
    if (is_box_use(head)) return(invisible())

    if (is_source_fn(head)) {
      path_val <- extract_path(expr)
      if (!is.null(path_val)) {
        loc <- loc_from(own_srcref)
        acc[[length(acc) + 1L]] <<- list(
          path = path_val,
          chdir = extract_chdir(expr),
          conditional = conditional,
          line = loc$line,
          col = loc$col
        )
      }
      return(invisible())
    }

    child_cond <- conditional || is_control_flow(head)
    for (i in seq_along(expr)) {
      child <- expr[[i]]
      if (!walkable(child)) next
      walk(child, child_cond,
           child_srcref(expr, i) %||% pick_srcref(child) %||% own_srcref)
    }
  }

  for (i in seq_along(parsed_expr)) {
    walk(parsed_expr[[i]],
         conditional = FALSE,
         own_srcref = pick_srcref(parsed_expr[[i]]) %||%
                       child_srcref(parsed_expr, i))
  }
  acc
}

#' Build the Sources table for an app.
#'
#' One row per static `source()` call across every enumerated file.
#' Paths are resolved relative to the calling file's directory.
#'
#' @noRd
build_sources_table <- function(app_path) {
 svt_memoize(paste0("sources\x1f", app_path), function() {
  files <- enumerate_app_files(app_path)

  from_files <- character()
  paths <- character()
  to_files <- character()
  chdirs <- logical()
  conditionals <- logical()
  lines <- integer()
  cols <- integer()

  for (f in files) {
    parsed <- parse_file(app_path, f)
    if (is.null(parsed)) next
    calls <- find_source_calls(parsed)
    file_dir <- dirname(f)
    for (call in calls) {
      base <- if (isTRUE(call$chdir)) file_dir else "."
      to_file <- normalize_relpath(base, call$path)
      from_files <- c(from_files, f)
      paths <- c(paths, call$path)
      to_files <- c(to_files, to_file)
      chdirs <- c(chdirs, isTRUE(call$chdir))
      conditionals <- c(conditionals, call$conditional)
      lines <- c(lines, as.integer(call$line))
      cols <- c(cols, as.integer(call$col))
    }
  }

  tibble::tibble(
    from_file = from_files,
    path = paths,
    to_file = to_files,
    chdir = chdirs,
    conditional = conditionals,
    line = lines,
    col = cols
  )
 })
}
