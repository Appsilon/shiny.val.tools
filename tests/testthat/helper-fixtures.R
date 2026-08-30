#' Materialize a fixture app whose contents include hidden dotfiles.
#'
#' Delegates to the package's own `materialize_example()` so tests exercise the
#' same copy-and-restore-dotfile path that `svt_run_example()` uses. Result is
#' cached per-process so tests within a single run share one materialized copy.
materialize_fixture_with_dotfiles <- function(fixture) {
  cache_env <- materialize_fixture_with_dotfiles__cache
  if (!is.null(cache_env[[fixture]])) return(cache_env[[fixture]])

  cache_env[[fixture]] <- materialize_example(fixture)
  cache_env[[fixture]]
}

materialize_fixture_with_dotfiles__cache <- new.env(parent = emptyenv())

#' Path to a bundled fixture app.
#'
#' `system.file()` rather than `test_path()`: under `R CMD check` the tests run
#' against the installed package, where no `inst/` directory exists.
fixture_path <- function(name) {
  path <- system.file("extdata", name, package = "shiny.val.tools")
  if (!nzchar(path)) stop("fixture not installed: ", name)
  path
}
