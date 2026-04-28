#' Pick the left-hand value if non-NULL, else the right.
#'
#' Backports R 4.4's base `%||%` for older R. Used package-wide for
#' default values.
#'
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' TRUE when `x` is the R_MissingArg sentinel.
#'
#' Empty arg slots in calls (trailing commas in `c(1, 2, )`, missing
#' formals in `alist(x =)`, etc.) parse into R's missing-arg sentinel.
#' Once that sentinel is bound to a local variable, *reading* the
#' variable errors with "argument 'x' is missing, with no default" --
#' which fires before any `is.null` / `is.symbol` check the AST walkers
#' use to skip empty slots.
#'
#' We detect the sentinel by forcing the argument inside `tryCatch`:
#' the underlying error is captured and we report it as a clean
#' boolean. Non-missing values force without error and return FALSE.
#'
#' @noRd
is_missing_arg <- function(x) {
  !tryCatch({ force(x); TRUE }, error = function(e) FALSE)
}

#' Lexically normalize `path` relative to `base_dir`.
#'
#' Resolves `.` and `..` segments without touching the filesystem so paths
#' stay deterministic across platforms. `base_dir == "."` (a top-level file)
#' leaves `path` unchanged. `..` segments that escape above the root are
#' clipped — we don't model paths outside the app root.
#'
#' @noRd
normalize_relpath <- function(base_dir, path) {
  combined <- if (identical(base_dir, ".") || identical(base_dir, "")) {
    path
  } else {
    paste(base_dir, path, sep = "/")
  }
  parts <- strsplit(combined, "/", fixed = TRUE)[[1L]]

  out <- character()
  for (p in parts) {
    if (p == "" || p == ".") next
    if (p == "..") {
      if (length(out)) out <- out[-length(out)]
      next
    }
    out <- c(out, p)
  }
  paste(out, collapse = "/")
}
