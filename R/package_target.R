#' Is this analysis target a source R package rather than a Shiny app?
#'
#' A package that ships Shiny modules for other apps to consume has no
#' `app.R` / `ui.R` / `server.R` entrypoint — its modules *are* the
#' deliverable. `DESCRIPTION` plus `NAMESPACE` is the discriminator:
#' `DESCRIPTION` alone also appears in non-package project layouts.
#'
#' @noRd
is_package_target <- function(path) {
  file.exists(file.path(path, "DESCRIPTION")) &&
    file.exists(file.path(path, "NAMESPACE"))
}

#' Names exported by a source package's `NAMESPACE`.
#'
#' Returns `character()` for anything that is not a source package, so
#' callers can treat "no exports declared" and "not a package" alike.
#'
#' @noRd
package_exports <- function(path) {
  if (!is_package_target(path)) return(character())

  ns <- tryCatch(
    parseNamespaceFile(basename(path), dirname(path)),
    error = function(e) NULL
  )
  if (is.null(ns)) return(character())

  sort(unique(as.character(ns$exports)))
}

#' The exported Shiny modules of a package target.
#'
#' A module qualifies when the function wrapping its `moduleServer()` call
#' is exported. The paired UI function is matched by stem — `<stem>_server`
#' to `<stem>_ui` — which is the dominant convention for packaged modules;
#' `ui_fn` is `NA` when no such export exists.
#'
#' `module` is the graph's own module identity (file-path derived), kept so
#' callers can join back to node namespaces. `server_fn` is the name the
#' package's consumers actually call, and is what artifacts are keyed by.
#'
#' @noRd
exported_modules <- function(app_path) {
  empty <- tibble::tibble(
    module = character(), server_fn = character(), ui_fn = character()
  )
  if (!is_package_target(app_path)) return(empty)

  exports <- package_exports(app_path)
  if (!length(exports)) return(empty)

  defs <- build_definitions_table(app_path)
  mod_rows <- defs[defs$kind == "module_server" &
                     !is.na(defs$name) &
                     !is.na(defs$wrapper_binding), , drop = FALSE]
  if (!nrow(mod_rows)) return(empty)

  keep <- mod_rows$wrapper_binding %in% exports
  mod_rows <- mod_rows[keep, , drop = FALSE]
  if (!nrow(mod_rows)) return(empty)

  server_fn <- mod_rows$wrapper_binding
  ui_candidate <- paste0(sub("_server$", "", server_fn), "_ui")
  ui_fn <- ifelse(ui_candidate %in% exports, ui_candidate, NA_character_)

  ord <- order(server_fn)
  tibble::tibble(
    module = mod_rows$name[ord],
    server_fn = server_fn[ord],
    ui_fn = ui_fn[ord]
  )
}
