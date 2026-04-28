#' The starting set: files Shiny picks up by convention.
#'
#' Traditional Shiny: `app.R`, `ui.R`, `server.R`, `global.R` if present
#' at the app root, plus every `*.R` file under `R/` recursively.
#' `.Rprofile` is also included when present — it runs at R startup and,
#' in rhino apps, sets `options(box.path = ...)`. Rhino apps additionally
#' get `app/main.R`: `rhino::app()` boots it via `box::use(app/main)`,
#' which we can't follow statically, so we add it directly when it exists.
#' Source-following is done by `enumerate_app_files()`.
#'
#' @noRd
enumerate_starting_set <- function(app_path) {
  top_level_candidates <- c("app.R", "ui.R", "server.R", "global.R",
                            ".Rprofile", "app/main.R")
  top_level <- top_level_candidates[
    file.exists(file.path(app_path, top_level_candidates))
  ]

  r_dir <- file.path(app_path, "R")
  r_files <- if (dir.exists(r_dir)) {
    found <- list.files(r_dir, pattern = "\\.R$", recursive = TRUE)
    if (length(found)) file.path("R", found) else character()
  } else {
    character()
  }

  files <- c(top_level, r_files)
  files <- gsub("\\\\", "/", files, fixed = FALSE)
  sort(files)
}

#' Enumerate every R file reachable in an app.
#'
#' Begins with `enumerate_starting_set()`, then walks every file's AST,
#' collects static `source()` targets, and adds resolved paths whose
#' files exist on disk. Iterates to a fixpoint so transitive `source()`
#' chains are followed; cycles are bounded by the visited set.
#'
#' @param app_path Path to the app root.
#'
#' @return A character vector of relative paths (forward-slash, sorted).
#'
#' @noRd
enumerate_app_files <- function(app_path) {
  known <- enumerate_starting_set(app_path)
  parsed_set <- character()
  box_path_base <- discover_box_path(app_path) %||% "."

  repeat {
    pending <- setdiff(known, parsed_set)
    if (!length(pending)) break

    discovered <- character()
    for (f in pending) {
      parsed <- parse_file(app_path, f)
      parsed_set <- c(parsed_set, f)
      if (is.null(parsed)) next

      file_dir <- dirname(f)

      for (call in find_source_calls(parsed)) {
        # `chdir = TRUE` makes the inner source() resolve relative to the
        # calling file's directory; default (FALSE) resolves against the
        # process cwd, which we model as the app root.
        base <- if (isTRUE(call$chdir)) file_dir else "."
        target <- normalize_relpath(base, call$path)
        if (!nzchar(target)) next
        if (target %in% known) next
        if (!file.exists(file.path(app_path, target))) next
        discovered <- c(discovered, target)
      }

      for (clause in find_box_use_calls(parsed)) {
        if (is.na(clause$local_path)) next
        base <- if (isTRUE(clause$absolute)) box_path_base else file_dir
        target <- resolve_box_local(base, clause$local_path)
        if (is.null(target) || !nzchar(target)) next
        if (target %in% known) next
        if (!file.exists(file.path(app_path, target))) next
        discovered <- c(discovered, target)
      }
    }

    new_files <- setdiff(unique(discovered), known)
    if (!length(new_files)) break
    known <- c(known, new_files)
  }

  sort(unique(known))
}
