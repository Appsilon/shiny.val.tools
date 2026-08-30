#' Run one of the bundled example Shiny apps
#'
#' The package ships small Shiny apps under `inst/extdata/`, each exercising a
#' specific source pattern. This runs one of them so you can see the app the
#' analysis artifacts describe.
#'
#' Only the apps that actually start are offered. `cycle_app` (deliberately
#' circular `source()`) and `box_in_R` (no `shinyApp()` entrypoint) exist to
#' exercise static analysis and are not runnable.
#'
#' The app is copied to a temporary directory before running, so edits made
#' while exploring never touch the installed package.
#'
#' @param app Name of the example app to run. One of `"traditional_basic"`,
#'   `"traditional_with_source"`, `"rhino_basic"` or `"rhino_multi_module"`.
#' @param ... Passed on to [shiny::runApp()].
#'
#' @return Whatever [shiny::runApp()] returns, invisibly for its side effect of
#'   running the app.
#'
#' @examples
#' if (interactive()) {
#'   svt_run_example("rhino_multi_module")
#' }
#' @export
svt_run_example <- function(app = c("traditional_basic",
                                    "traditional_with_source",
                                    "rhino_basic",
                                    "rhino_multi_module"),
                            ...) {
  app <- match.arg(app)

  require_pkg("shiny", app)
  if (startsWith(app, "rhino_")) require_pkg("rhino", app)

  app_dir <- materialize_example(app)
  apply_app_profile(app_dir)

  cli::cli_inform(c(i = "Running example app {.val {app}} from {.path {app_dir}}"))
  shiny::runApp(app_dir, ...)
}

#' Copy an example app to a temporary directory
#'
#' `R CMD build` silently drops files whose name starts with `.`, so fixtures
#' needing a literal `.Rprofile` (the rhino apps) ship it as `_Rprofile`. This
#' copies the app out and restores the dotfile name.
#'
#' @param app Name of the example app directory under `inst/extdata/`.
#' @return Path to the materialized copy.
#' @noRd
materialize_example <- function(app) {
  src <- system.file("extdata", app, package = "shiny.val.tools")
  if (!nzchar(src)) {
    cli::cli_abort("Example app {.val {app}} is not installed.")
  }

  dest <- tempfile(paste0("svt-example-", app, "-"))
  dir.create(dest, recursive = TRUE)
  file.copy(
    list.files(src, full.names = TRUE, all.files = TRUE, no.. = TRUE),
    dest,
    recursive = TRUE
  )

  underscored <- file.path(dest, "_Rprofile")
  if (file.exists(underscored)) {
    file.rename(underscored, file.path(dest, ".Rprofile"))
  }

  dest
}

#' Apply an app's `.Rprofile` to the current session
#'
#' `runApp()` does not source `.Rprofile` in an already-running session, but the
#' rhino apps depend on it to set `box.path`. Sourcing it by hand — with the
#' working directory set, since the profile calls `getwd()` — is what makes
#' absolute `box::use(app/view/...)` imports resolve.
#'
#' @noRd
apply_app_profile <- function(app_dir) {
  profile <- file.path(app_dir, ".Rprofile")
  if (!file.exists(profile)) return(invisible(NULL))

  old_wd <- setwd(app_dir)
  on.exit(setwd(old_wd), add = TRUE)
  source(profile, local = new.env())

  invisible(NULL)
}

#' @noRd
require_pkg <- function(pkg, app) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg {pkg}} is required to run example app {.val {app}}.",
      i = "Install it with {.run install.packages(\"{pkg}\")}."
    ))
  }
  invisible(NULL)
}
