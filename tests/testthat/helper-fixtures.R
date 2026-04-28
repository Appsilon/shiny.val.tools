#' Materialize a fixture app whose contents include hidden dotfiles.
#'
#' R CMD build silently drops files whose name starts with `.`, so fixtures
#' that need a literal `.Rprofile` (rhino apps) store it as `_Rprofile`
#' and rely on this helper to copy the fixture to a tempdir and restore
#' the dotfile name. Result is cached per-process so tests within a single
#' run share one materialized copy.
materialize_fixture_with_dotfiles <- function(fixture) {
  cache_env <- materialize_fixture_with_dotfiles__cache
  if (!is.null(cache_env[[fixture]])) return(cache_env[[fixture]])

  src <- system.file("extdata", fixture, package = "shiny.val.tools")
  if (!nzchar(src)) stop("fixture not installed: ", fixture)

  dest <- file.path(tempfile(paste0("svt-fixture-", fixture, "-")))
  dir.create(dest, recursive = TRUE)
  file.copy(list.files(src, full.names = TRUE, all.files = TRUE,
                       no.. = TRUE),
            dest, recursive = TRUE)

  underscored <- file.path(dest, "_Rprofile")
  if (file.exists(underscored)) {
    file.rename(underscored, file.path(dest, ".Rprofile"))
  }

  cache_env[[fixture]] <- dest
  dest
}

materialize_fixture_with_dotfiles__cache <- new.env(parent = emptyenv())
