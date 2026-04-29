#' Filename of the artifact manifest within `out_dir`.
#'
#' The manifest is the package's record of what it wrote. On re-run we
#' compare on-disk MD5s against the manifest's recorded MD5s to tell
#' apart "files we wrote that nobody touched" (pristine) from "files we
#' wrote that the user has since edited". See spec 05 "Output directory
#' lifecycle".
#'
#' @noRd
svt_manifest_filename <- function() ".svt_manifest.json"

#' @noRd
manifest_path <- function(out_dir) {
  file.path(out_dir, svt_manifest_filename())
}

#' Plan the set of artifact paths a render run will produce.
#'
#' Paths are returned relative to `out_dir`, sorted, deduplicated. The
#' set drives both orphan detection (paths in the prior manifest minus
#' the planned set) and manifest emission after a successful run.
#'
#' `inventory_features` is the `inventory$features` named list (or
#' NULL); per-record inventories only become artifacts when a record
#' has a corresponding entry there.
#'
#' @noRd
plan_artifact_paths <- function(records, inventory_features) {
  paths <- character()
  for (rec in records) {
    slug <- slugify_artifact_name(rec$name)
    paths <- c(paths,
               paste0(slug, ".md"),
               paste0(slug, ".html"))
    if (!is.null(inventory_features) &&
        !is.null(inventory_features[[rec$name]])) {
      paths <- c(paths, paste0(slug, "/inventory.json"))
    }
  }
  paths <- c(paths, "index.md", "index.html")
  sort(unique(paths))
}

#' Read the prior artifact manifest, if any.
#'
#' Returns NULL when no manifest is present or when it is unreadable.
#' Returns a tibble with columns `path` and `md5` otherwise. A malformed
#' manifest is treated like a missing one — the lifecycle check then
#' falls through to the "non-empty without manifest" abort, which is the
#' safe default (we won't silently overwrite when we can't tell what's
#' ours).
#'
#' @noRd
read_artifact_manifest <- function(out_dir) {
  p <- manifest_path(out_dir)
  if (!file.exists(p)) return(NULL)
  raw <- tryCatch(
    paste(readLines(p, warn = FALSE), collapse = "\n"),
    error = function(e) NULL
  )
  if (is.null(raw) || !nzchar(raw)) return(NULL)
  parsed <- tryCatch(yaml::yaml.load(raw), error = function(e) NULL)
  if (is.null(parsed) || is.null(parsed$artifacts)) return(NULL)

  arts <- parsed$artifacts
  if (!length(arts)) {
    return(tibble::tibble(path = character(), md5 = character()))
  }
  paths <- vapply(arts,
                  function(a) if (is.null(a$path)) NA_character_ else a$path,
                  character(1))
  md5s <- vapply(arts,
                 function(a) if (is.null(a$md5)) NA_character_ else a$md5,
                 character(1))
  ok <- !is.na(paths) & !is.na(md5s)
  tibble::tibble(path = paths[ok], md5 = md5s[ok])
}

#' Write the artifact manifest for a successful render.
#'
#' `paths` is a character vector relative to `out_dir`. The on-disk
#' artifacts must already exist; their MD5s are computed at write time.
#' The `artifacts` array is sorted by `path` for byte-deterministic
#' output across runs.
#'
#' @noRd
write_artifact_manifest <- function(out_dir, paths) {
  paths <- sort(unique(paths))
  arts <- vapply(paths, function(p) {
    fp <- file.path(out_dir, p)
    md5 <- if (file.exists(fp)) unname(tools::md5sum(fp)) else NA_character_
    paste0("{\"path\":", json_str(p),
           ",\"md5\":", json_str(md5), "}")
  }, character(1))
  body <- paste0("{\"schema_version\":\"1.0\",\"artifacts\":[",
                 paste(arts, collapse = ","),
                 "]}")
  out_path <- manifest_path(out_dir)
  writeLines(body, out_path)
  out_path
}

#' Lifecycle check before any artifact is written.
#'
#' Resolves the four cases enumerated in spec 05 "Re-run rules":
#'   - missing dir   -> create, proceed (NULL).
#'   - empty dir     -> proceed (NULL).
#'   - non-empty + no manifest -> abort.
#'   - non-empty + manifest    -> identify orphans; abort if any orphan
#'                                has been edited (md5 mismatch); else
#'                                proceed and return the prior manifest.
#'
#' Returns the prior manifest tibble (or NULL) so the caller can pass it
#' to `delete_pristine_orphans()` after writing the new artifact set.
#'
#' Aborts via `stop()`.
#'
#' @noRd
lifecycle_check <- function(out_dir, planned_paths) {
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
    return(NULL)
  }

  contents <- list.files(out_dir, all.files = TRUE, no.. = TRUE,
                         include.dirs = TRUE, recursive = FALSE)
  if (!length(contents)) return(NULL)

  prior <- read_artifact_manifest(out_dir)
  if (is.null(prior)) {
    stop(
      "svt_render(): out_dir '", out_dir,
      "' is non-empty and has no ", svt_manifest_filename(), ". ",
      "Cannot determine which files are previously generated artifacts. ",
      "Pass a fresh path or remove the existing directory.",
      call. = FALSE
    )
  }

  orphans <- setdiff(prior$path, planned_paths)
  if (!length(orphans)) return(prior)

  edited <- character()
  for (p in orphans) {
    fp <- file.path(out_dir, p)
    if (!file.exists(fp)) next
    expected <- prior$md5[prior$path == p]
    actual <- unname(tools::md5sum(fp))
    if (!identical(expected, actual)) edited <- c(edited, p)
  }

  if (length(edited)) {
    stop(
      "svt_render(): refusing to remove ", length(edited),
      " orphan artifact(s) with manual edits. ",
      "Back them up or restore the originals before re-running.\n",
      paste(paste0("  - ", file.path(out_dir, edited)),
            collapse = "\n"),
      call. = FALSE
    )
  }

  prior
}

#' Delete pristine orphans recorded in the prior manifest but no longer
#' planned.
#'
#' For each orphan path:
#'   - unlink the file itself,
#'   - unlink the sibling `<name>_files/` libdir if the path was an HTML
#'     artifact (defensive — `cleanup_widget_libdir()` already handles
#'     this at write time, but a prior run could have aborted before
#'     cleanup),
#'   - drop the parent directory if it is now empty (the per-feature
#'     `<slug>/` dir that holds `inventory.json`).
#'
#' Called only after `lifecycle_check()` has confirmed no orphan has
#' been edited.
#'
#' @noRd
delete_pristine_orphans <- function(out_dir, prior, planned_paths) {
  if (is.null(prior) || !nrow(prior)) return(invisible())

  orphans <- setdiff(prior$path, planned_paths)
  if (!length(orphans)) return(invisible())

  for (p in orphans) {
    fp <- file.path(out_dir, p)
    if (file.exists(fp)) unlink(fp)
    if (grepl("\\.html$", p)) {
      libdir <- sub("\\.html$", "_files", fp)
      if (dir.exists(libdir)) unlink(libdir, recursive = TRUE)
    }
    parent <- dirname(fp)
    if (parent != out_dir && dir.exists(parent) &&
        !length(list.files(parent, all.files = TRUE, no.. = TRUE))) {
      unlink(parent, recursive = TRUE)
    }
  }
  invisible()
}
