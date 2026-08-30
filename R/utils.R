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

#' Slugify a feature/module identity into a flat-filesystem-safe name.
#'
#' Module identities use forward slashes (e.g. `app/view/mod_card`) to
#' encode the source path. The `--` separator is the chosen replacement:
#' `_` collides with module-name word separators (`mod_card`) and `.`
#' collides with file extensions. See spec 04 "Filename slug rule".
#'
#' Pure transformation; the original identity remains the canonical key
#' everywhere it is read by humans (doc stub headings, widget titles,
#' inventory.json `feature` field).
#'
#' @noRd
slugify_artifact_name <- function(name) {
  if (is.null(name) || (length(name) == 1L && is.na(name))) {
    return(NA_character_)
  }
  gsub("/", "--", as.character(name), fixed = TRUE)
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

#' A numbered step reporter for a multi-step run.
#'
#' `svt_validate()` prints one line per pipeline stage. Without a count
#' the reader learns what is happening now but not how much is left, and
#' on a large app the gap between two lines is long enough for that to
#' matter. The reporter closes over the planned step list so each line
#' carries its position: `[2/6] Building reactive graph`.
#'
#' The step list is built by the caller and can be shorter than the full
#' pipeline (the testing layer is optional), so the denominator always
#' describes the run actually happening.
#'
#' @noRd
new_step_reporter <- function(labels) {
  i <- 0L
  n <- length(labels)
  width <- nchar(as.character(n))
  function(msg = NULL) {
    i <<- i + 1L
    text <- msg %||% labels[[i]]
    idx <- formatC(i, width = width)
    cli::cli_alert_info("[{idx}/{n}] {text}")
    invisible(i)
  }
}

#' Progress-bar format string with a counter and the item in flight.
#'
#' `{cli::pb_current}/{cli::pb_total}` answers "how far in", and `item`
#' — evaluated in the calling frame, so it tracks the loop variable —
#' answers "on what". The bar stays transient: it clears when the step
#' finishes, leaving only the step line behind.
#'
#' @noRd
svt_bar_format <- function(label, item = "{item}") {
  # No percent: the bar already shows it, and on a narrow terminal the
  # item name is the first thing cli truncates. `pb_eta_str` prints its
  # own "ETA:" prefix.
  paste0(
    "{cli::pb_spin} ", label,
    " {cli::pb_current}/{cli::pb_total} ", item,
    " {cli::pb_bar} {cli::pb_eta_str}"
  )
}
