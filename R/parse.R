#' Parse one file, retaining srcrefs.
#'
#' Returns the parse() result, or NULL on parse failure.
#'
#' @noRd
parse_file <- function(app_path, relpath) {
  full_path <- file.path(app_path, relpath)
  tryCatch(
    parse(file = full_path, keep.source = TRUE),
    error = function(e) NULL
  )
}
