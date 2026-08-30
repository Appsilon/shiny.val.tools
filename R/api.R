#' Parse a Shiny app's source.
#'
#' Enumerates every R file reachable from the app root (the conventional
#' starting set plus transitive `source()` and `box::use()` follow),
#' parses each retaining srcrefs, and returns the parsed bundle as an
#' `svt_parsed` object.
#'
#' `app_path` may be a directory or a `.zip` archive; archives are
#' extracted to a session-scoped tempdir and the extracted root is used
#' for all subsequent steps.
#'
#' @param app_path Path to a Shiny app root or a `.zip` archive.
#'
#' @return An `svt_parsed` object — a list with `app_path`, `files`
#'   (relative paths), and `asts` (named list, parse() results keyed by
#'   relpath).
#'
#' @export
svt_parse <- function(app_path) {
  with_svt_cache({
    resolved <- resolve_app_path(app_path)
    files <- enumerate_app_files(resolved)

    asts <- vector("list", length(files))
    cli::cli_progress_bar(
      format = svt_bar_format("Parsing files", "{.file {files[[i]]}}"),
      total = length(files), clear = TRUE
    )
    for (i in seq_along(files)) {
      asts[[i]] <- parse_file(resolved, files[[i]])
      cli::cli_progress_update()
    }
    cli::cli_progress_done()
    names(asts) <- files

    structure(
      list(app_path = resolved, files = files, asts = asts),
      class = "svt_parsed"
    )
  })
}

#' Build the reactive graph from a parsed app.
#'
#' Returns the five intermediate tables (Files, Imports, Sources,
#' Definitions, References) plus the assembled Nodes, Edges, and
#' Warnings tables as an `svt_graph` object. The original `app_path`
#' is carried through so downstream steps (slicing, inventory, render)
#' can re-resolve files.
#'
#' @param parsed An `svt_parsed` object from `svt_parse()`.
#'
#' @return An `svt_graph` object — the graph tibbles plus `app_path`.
#'
#' @export
svt_build_graph <- function(parsed) {
  if (!inherits(parsed, "svt_parsed")) {
    stop("svt_build_graph() requires an svt_parsed object.", call. = FALSE)
  }
  with_svt_cache({
    g <- build_graph(parsed$app_path)
    g$app_path <- parsed$app_path
    structure(g, class = "svt_graph")
  })
}

#' Slice a graph into feature and module subgraphs.
#'
#' Without a manifest, the default rule applies (one feature per
#' top-level output / observer; one module subgraph per `moduleServer`
#' definition). With a manifest, manifest-declared features supersede
#' default features at the same root, and module subgraphs are still
#' produced for every module definition.
#'
#' Manifest validation is run before application; with `lenient = FALSE`
#' (the default) any SVT-W101/W102/W105 issue or a name collision aborts.
#' With `lenient = TRUE` the issues are reported via `cli` warnings and
#' slicing proceeds best-effort.
#'
#' @param graph An `svt_graph` from `svt_build_graph()`.
#' @param manifest Path to `features.yml`, an in-memory manifest list,
#'   or `NULL` for default slicing.
#' @param lenient If `TRUE`, manifest issues become warnings rather than
#'   aborts.
#'
#' @return An `svt_features` object — a list of feature/module records
#'   with `manifest_supplied`, `manifest_issues`, `unclaimed`, and
#'   `app_path` carried alongside.
#'
#' @export
svt_slice <- function(graph, manifest = NULL, lenient = FALSE) {
  if (!inherits(graph, "svt_graph")) {
    stop("svt_slice() requires an svt_graph object.", call. = FALSE)
  }

  manifest_supplied <- !is.null(manifest)
  m <- normalize_manifest(manifest)

  issues <- validate_manifest(m, graph)
  if (nrow(issues) && !isTRUE(lenient)) {
    msg <- paste(paste0("- ", issues$code, ": ", issues$message),
                 collapse = "\n")
    stop("Manifest validation failed:\n", msg, call. = FALSE)
  }
  if (nrow(issues) && isTRUE(lenient)) {
    for (i in seq_len(nrow(issues))) {
      warning(issues$code[i], ": ", issues$message[i], call. = FALSE)
    }
  }

  feats <- if (manifest_supplied && length(m$features)) {
    apply_manifest(m, graph)
  } else {
    default_slice(graph)
  }

  mods <- module_slice(graph, graph$app_path)
  combined <- c(feats, mods)

  unclaimed <- detect_unclaimed_outputs(graph, m, manifest_supplied)

  structure(
    list(
      records = combined,
      manifest = m,
      manifest_supplied = manifest_supplied,
      manifest_issues = issues,
      unclaimed = unclaimed,
      graph = graph,
      app_path = graph$app_path
    ),
    class = "svt_features"
  )
}

#' Build per-feature inventories.
#'
#' For every feature/module in `features`, resolves call sites against
#' the file's imports, classifies direct vs transitive, and emits the
#' function-centric FunctionCall and derived PackageUse views.
#'
#' @param graph An `svt_graph`.
#' @param features An `svt_features` object.
#'
#' @return An `svt_inventory` object — a named list keyed by feature
#'   name, each entry the per-feature inventory record.
#'
#' @export
svt_inventory <- function(graph, features) {
  if (!inherits(graph, "svt_graph")) {
    stop("svt_inventory() requires an svt_graph.", call. = FALSE)
  }
  if (!inherits(features, "svt_features")) {
    stop("svt_inventory() requires an svt_features object.", call. = FALSE)
  }
  with_svt_cache({
    inv <- build_inventory(graph, features$records, graph$app_path)
    structure(
      list(features = inv, app_path = graph$app_path),
      class = "svt_inventory"
    )
  })
}

#' Derive the test surface for every feature and module.
#'
#' The test surface answers "what is there to test in this feature":
#' the inputs a test must set (stimuli), the outputs and returned
#' reactives it can assert on (observables), the intermediates between
#' them, the trusted terminals, and the app-defined helpers it calls.
#' Everything is derived from the subgraph — the app is never run.
#'
#' The package targets `testthat` only: every feature and module surface
#' is driven through [shiny::testServer()], and each helper gets a plain
#' `testthat` unit stub. Observables whose render function produces a
#' value that cannot usefully be compared (`renderPlot`, `renderUI`,
#' `downloadHandler`, ...) are annotated `opaque` and flagged SVT-W312,
#' which points the assertion at the helper that computed the data.
#'
#' Coverage in this layer means *exercised*, never *correct*.
#'
#' @param features An `svt_features` object.
#' @param inventory An `svt_inventory` object, or `NULL`. Supplies the
#'   called-function set from which helpers are derived.
#'
#' @return An `svt_test_surface` object — a named list of per-subgraph
#'   surface records.
#'
#' @export
svt_test_surface <- function(features, inventory = NULL) {
  if (!inherits(features, "svt_features")) {
    stop("svt_test_surface() requires an svt_features object.", call. = FALSE)
  }
  if (!is.null(inventory) && !inherits(inventory, "svt_inventory")) {
    stop("svt_test_surface() requires an svt_inventory object or NULL.",
         call. = FALSE)
  }
  with_svt_cache({
    surfaces <- build_test_surface(
      features$graph, features$records,
      inventory_features = if (is.null(inventory)) NULL else inventory$features,
      app_path = features$app_path
    )
    structure(
      list(surfaces = surfaces, app_path = features$app_path),
      class = "svt_test_surface"
    )
  })
}

#' @export
print.svt_test_surface <- function(x, ...) {
  cat("<svt_test_surface>\n")
  cat("  surfaces: ", length(x$surfaces), "\n", sep = "")
  invisible(x)
}

#' @export
summary.svt_test_surface <- function(object, ...) {
  cat("svt_test_surface summary\n")
  for (nm in names(object$surfaces)) {
    s <- object$surfaces[[nm]]
    cat("  - ", nm, " [", s$harness, "]: ",
        nrow(s$stimuli), " stimuli, ",
        nrow(s$observables), " observables, ",
        nrow(s$helpers), " helpers",
        if (length(s$blockers)) paste0(" (blocked: ",
                                       paste(s$blockers, collapse = ", "), ")")
        else "",
        "\n", sep = "")
  }
  invisible(object)
}

#' Write test scaffolds for a derived test surface.
#'
#' One file per surface plus one per helper group. The generated code is
#' a harness with the plumbing filled in and the assertions left blank:
#' every `test_that()` block opens with a `skip()` marker, which is
#' visible in `testthat` output, countable, non-blocking, and detectable
#' by the coverage classifier. Removing that line is the developer's
#' explicit act of taking ownership of the test.
#'
#' Assertions and expected values are never generated — an expected value
#' is a human judgment about the analysis, and a generated one would be
#' worse than no test at all.
#'
#' Overwrite rules. With `target = "staging"` files land in
#' `<out_dir>/tests/`: a scaffold nobody touched refreshes, and one whose
#' contents have moved away from the artifact manifest's record is a test
#' somebody wrote and is left alone. With `target = "app"` files land in
#' the app's own `tests/testthat/` and an existing path is **never**
#' written — it is skipped with SVT-W309.
#'
#' @param surface An `svt_test_surface` object.
#' @param out_dir The validation directory. Scaffolds are written to
#'   `<out_dir>/tests/` under `target = "staging"`.
#' @param target `"staging"` (default) or `"app"`.
#' @param features Character vector of surface names to scaffold
#'   (`NULL` = all).
#'
#' @return A tibble with one row per scaffold: `name`, `kind`
#'   (`surface` / `helpers`), `path`, `status` (`written`, `unchanged`,
#'   `skipped_edited`, `skipped_exists`) and `warnings`.
#'
#' @export
svt_scaffold_tests <- function(surface, out_dir = "validation",
                               target = c("staging", "app"),
                               features = NULL) {
  if (!inherits(surface, "svt_test_surface")) {
    stop("svt_scaffold_tests() requires an svt_test_surface object.",
         call. = FALSE)
  }
  target <- match.arg(target)

  with_svt_cache({
    prior <- if (target == "staging") read_artifact_manifest(out_dir) else NULL
    written <- write_scaffolds(surface$surfaces, out_dir, surface$app_path,
                               target = target, features = features,
                               prior = prior)

    # Keep the staging tree lifecycle-managed: a scaffold that is not in
    # the manifest cannot be told apart from a hand-written file later,
    # and so would never refresh.
    if (target == "staging" && nrow(written$results)) {
      keep <- written$results$status != "skipped_edited" |
        written$rel_paths %in% names(written$preserved)
      write_artifact_manifest(
        out_dir,
        c(if (is.null(prior)) character() else prior$path,
          written$rel_paths[keep]),
        overrides = written$preserved
      )
    }

    res <- written$results
    n_new <- sum(res$status %in% c("written", "unchanged"))
    if (n_new) {
      cli::cli_alert_info(
        "{n_new} scaffold{?s} written; assertions not filled in (SVT-W303)"
      )
    }
    n_skipped <- sum(res$status %in% c("skipped_edited", "skipped_exists"))
    if (n_skipped) {
      cli::cli_alert_warning(
        "{n_skipped} scaffold{?s} left untouched (already written by hand)"
      )
    }
    res
  })
}

#' Render artifacts for every feature/module.
#'
#' Per feature/module: writes `<out_dir>/<name>.md` (the doc stub, with
#' Functions called and Packages used populated from the inventory),
#' `<out_dir>/<name>/inventory.json`, and `<out_dir>/<name>.html` (the
#' interactive visNetwork widget).
#'
#' @param features An `svt_features` object.
#' @param inventory An `svt_inventory` object.
#' @param out_dir Directory to write artifacts to. Created if missing.
#' @param surface An `svt_test_surface` object, or `NULL`. When `NULL`
#'   the `## Test surface` doc-stub sections and the per-feature
#'   `test_surface.json` artifacts are omitted entirely rather than
#'   rendered empty.
#' @param scaffold If `TRUE`, also write test scaffolds into
#'   `<out_dir>/tests/` as lifecycle-tracked artifacts. Requires
#'   `surface`. Off by default: generating files into a validated
#'   repository is opt-in, always.
#'
#' @return An `svt_validation` object — paths and counts.
#'
#' @export
svt_render <- function(features, inventory, out_dir = "validation",
                       surface = NULL, scaffold = FALSE) {
  if (!inherits(features, "svt_features")) {
    stop("svt_render() requires an svt_features object.", call. = FALSE)
  }
  if (!inherits(inventory, "svt_inventory")) {
    stop("svt_render() requires an svt_inventory object.", call. = FALSE)
  }
  if (!is.null(surface) && !inherits(surface, "svt_test_surface")) {
    stop("svt_render() requires an svt_test_surface object or NULL.",
         call. = FALSE)
  }

  surfaces <- if (is.null(surface)) NULL else surface$surfaces
  scaffold <- isTRUE(scaffold) && !is.null(surfaces)
  scaffold_rel <- if (scaffold) scaffold_plan(surfaces)$rel_path else character()
  planned <- plan_artifact_paths(features$records, inventory$features,
                                 surfaces, scaffold_rel)
  prior <- lifecycle_check(out_dir, planned)

  graph <- features$graph
  app_path <- features$app_path

  doc_paths <- character()
  inv_paths <- character()
  html_paths <- character()
  surface_paths <- character()

  cli::cli_progress_bar(
    format = svt_bar_format("Rendering artifacts", "{.field {rec$name}}"),
    total = length(features$records), clear = TRUE
  )
  for (rec in features$records) {
    inv_rec <- inventory$features[[rec$name]]
    surf_rec <- if (is.null(surfaces)) NULL else surfaces[[rec$name]]
    doc_paths <- c(doc_paths,
                   write_doc_stub(rec, graph, out_dir, inventory = inv_rec,
                                  surface = surf_rec))
    if (!is.null(inv_rec)) {
      inv_paths <- c(inv_paths, write_inventory_json(inv_rec, out_dir))
    }
    if (!is.null(surf_rec)) {
      surface_paths <- c(surface_paths,
                         write_test_surface_json(surf_rec, out_dir))
    }
    html_paths <- c(html_paths,
                    write_feature_html(rec, graph, out_dir,
                                       inventory_record = inv_rec,
                                       app_path = app_path))
    cli::cli_progress_update()
  }
  cli::cli_progress_done()

  report_md <- write_validation_report(features, inventory, graph, out_dir,
                                       app_path,
                                       report_meta = features$manifest$report)
  index_md <- write_index_md(features, inventory, graph, out_dir, app_path)
  index_html <- write_index_html(features, inventory, graph, out_dir, app_path)

  scaffold_res <- NULL
  scaffold_preserved <- NULL
  if (scaffold) {
    written <- write_scaffolds(surfaces, out_dir, app_path,
                               target = "staging", prior = prior)
    scaffold_res <- written$results
    scaffold_preserved <- written$preserved
  }

  delete_pristine_orphans(out_dir, prior, planned)
  manifest_file <- write_artifact_manifest(out_dir, planned,
                                           overrides = scaffold_preserved)

  structure(
    list(
      out_dir = out_dir,
      doc_stubs = doc_paths,
      inventories = inv_paths,
      widgets = html_paths,
      test_surfaces = surface_paths,
      scaffolds = if (is.null(scaffold_res)) character() else scaffold_res$path,
      index = c(index_md, index_html),
      manifest = manifest_file,
      manifest_issues = features$manifest_issues,
      unclaimed = features$unclaimed,
      n_features = sum(vapply(features$records,
                              function(r) identical(r$kind, "feature"),
                              logical(1))),
      n_modules = sum(vapply(features$records,
                             function(r) identical(r$kind, "module"),
                             logical(1)))
    ),
    class = "svt_validation"
  )
}

#' End-to-end: parse, slice, inventory, render.
#'
#' @param app_path Path to a Shiny app root or a `.zip` archive.
#' @param manifest Path to `features.yml`, an in-memory manifest list,
#'   or `NULL`. When `NULL`, `app_path/features.yml` is used if present.
#' @param out_dir Where artifacts are written. Created if missing.
#' @param features Character vector of feature names to include
#'   (`NULL` = all).
#' @param modules Character vector of module names to include
#'   (`NULL` = all).
#' @param lenient If `TRUE`, manifest issues become warnings.
#' @param tests How far to take the testing layer (spec 06):
#'   `"surface"` derives each subgraph's test surface and writes the
#'   `test_surface.json` artifacts and doc-stub sections; `"off"` skips
#'   the layer entirely.
#' @param scaffold If `TRUE`, also write test scaffolds into
#'   `<out_dir>/tests/`. Off by default: generating files into a
#'   validated repository is opt-in, always.
#'
#' @return An `svt_validation` object.
#'
#' @export
svt_validate <- function(app_path,
                         manifest = NULL,
                         out_dir = "validation",
                         features = NULL,
                         modules = NULL,
                         lenient = FALSE,
                         tests = c("surface", "off"),
                         scaffold = FALSE) {
  tests <- match.arg(tests)
  with_svt_cache({
    cli::cli_h1("shiny.val.tools: validating {.path {app_path}}")

    step <- new_step_reporter(c(
      "Parsing app sources",
      "Building reactive graph",
      "Slicing into features and modules",
      "Building inventories",
      if (tests != "off") "Deriving test surfaces",
      if (isTRUE(scaffold)) "Rendering artifacts and scaffolds"
      else "Rendering artifacts"
    ))

    step()
    parsed <- svt_parse(app_path)

    manifest_arg <- manifest
    if (is.null(manifest_arg)) {
      candidate <- file.path(parsed$app_path, "features.yml")
      if (file.exists(candidate)) manifest_arg <- candidate
    }

    step()
    graph <- svt_build_graph(parsed)

    step()
    feats <- svt_slice(graph, manifest = manifest_arg, lenient = lenient)
    feats$records <- filter_records(feats$records, features, modules)

    step()
    inv <- svt_inventory(graph, feats)

    surface <- NULL
    if (tests != "off") {
      step()
      surface <- svt_test_surface(feats, inv)
    }

    step()
    result <- svt_render(feats, inv, out_dir = out_dir, surface = surface,
                         scaffold = scaffold)

    cli::cli_alert_success(
      "Done: {result$n_features} feature{?s}, {result$n_modules} module{?s} \\
       written to {.path {result$out_dir}}"
    )
    result
  })
}

#' Per-feature node/edge/warning counts.
#'
#' Accepts an `svt_graph` (returns one row per feature in the default
#' slice) or an `svt_features` (returns one row per record in that
#' object). Useful as a fast orientation check.
#'
#' @param x An `svt_graph` or `svt_features` object.
#'
#' @export
svt_summary <- function(x) {
  if (inherits(x, "svt_graph")) {
    feats <- default_slice(x)
    return(summarize_features(feats, x))
  }
  if (inherits(x, "svt_features")) {
    g <- list(nodes = NULL, edges = NULL)
    return(summarize_features(x$records, g))
  }
  stop("svt_summary() needs an svt_graph or svt_features.", call. = FALSE)
}

#' All warnings, as a flat tibble.
#'
#' For an `svt_graph`, returns the global warnings table. For an
#' `svt_features`, prepends the manifest issues. For an `svt_inventory`,
#' returns the union of every feature's inventory warnings tagged with
#' the owning feature name.
#'
#' @param x An `svt_graph`, `svt_features`, or `svt_inventory` object.
#'
#' @export
svt_warnings <- function(x) {
  if (inherits(x, "svt_graph")) return(x$warnings)
  if (inherits(x, "svt_features")) {
    issues <- x$manifest_issues
    if (!nrow(issues)) {
      return(tibble::tibble(
        code = character(), name = character(), message = character()
      ))
    }
    return(issues)
  }
  if (inherits(x, "svt_inventory")) {
    parts <- list()
    for (nm in names(x$features)) {
      w <- x$features[[nm]]$warnings
      if (!nrow(w)) next
      w$feature <- nm
      parts[[length(parts) + 1L]] <- w
    }
    if (!length(parts)) {
      return(tibble::tibble(
        feature = character(), code = character(), file = character(),
        line = integer(), col = integer(), message = character()
      ))
    }
    return(do.call(rbind, parts))
  }
  stop("svt_warnings() needs an svt_graph, svt_features, or svt_inventory.",
       call. = FALSE)
}

#' Outputs/observers not claimed by any manifest-declared feature.
#'
#' Returns the tibble computed at slice time (SVT-W103). Only meaningful
#' when an explicit manifest was supplied — otherwise the default rule
#' trivially claims every root and the table is empty.
#'
#' @param features An `svt_features` object.
#'
#' @export
svt_unclaimed <- function(features) {
  if (!inherits(features, "svt_features")) {
    stop("svt_unclaimed() requires an svt_features object.", call. = FALSE)
  }
  features$unclaimed
}

#' Pretty-print a single file's AST.
#'
#' Uses `lobstr::ast()` when available; falls back to base `print()`
#' on the parsed expression otherwise.
#'
#' @param file_path Path to the R file to inspect.
#'
#' @export
svt_inspect <- function(file_path) {
  parsed <- tryCatch(parse(file = file_path, keep.source = TRUE),
                     error = function(e) {
                       stop("Could not parse ", file_path, ": ",
                            conditionMessage(e), call. = FALSE)
                     })
  if (requireNamespace("lobstr", quietly = TRUE)) {
    return(lobstr::ast(!!parsed))
  }
  warning("lobstr not installed; using base print(). ",
          "Install lobstr for tree-style AST output.", call. = FALSE)
  print(parsed)
  invisible(parsed)
}

#' Generate a starter `features.yml` for a graph.
#'
#' Writes the YAML to `out_path` (or returns it as a string when
#' `out_path` is NULL). Each top-level output/observer becomes one
#' entry with `intended_use`/`risk_classification`/`rationale` left as
#' `~` for the developer to fill.
#'
#' @param graph An `svt_graph`.
#' @param out_path Path to write the YAML to. `NULL` returns the YAML
#'   as a string instead of writing.
#'
#' @export
svt_manifest_template <- function(graph, out_path = "features.yml") {
  if (!inherits(graph, "svt_graph")) {
    stop("svt_manifest_template() requires an svt_graph.", call. = FALSE)
  }
  text <- manifest_template(graph)
  if (is.null(out_path)) return(text)
  writeLines(text, out_path)
  invisible(out_path)
}

#' Validate a manifest against a graph.
#'
#' Returns a tibble of issues; an empty tibble means "ready to use".
#' Wraps `validate_manifest()` so callers can inspect issues without
#' triggering the slice-time abort.
#'
#' @param manifest Path to `features.yml` or an in-memory manifest list.
#' @param graph An `svt_graph`.
#'
#' @export
svt_manifest_validate <- function(manifest, graph) {
  if (!inherits(graph, "svt_graph")) {
    stop("svt_manifest_validate() requires an svt_graph.", call. = FALSE)
  }
  validate_manifest(normalize_manifest(manifest), graph)
}

# print / summary methods ------------------------------------------------

#' @export
print.svt_parsed <- function(x, ...) {
  cat("<svt_parsed>\n")
  cat("  app_path: ", x$app_path, "\n", sep = "")
  cat("  files:    ", length(x$files), "\n", sep = "")
  invisible(x)
}

#' @export
summary.svt_parsed <- function(object, ...) {
  cat("svt_parsed summary\n")
  cat("  app_path: ", object$app_path, "\n", sep = "")
  cat("  files (", length(object$files), "):\n", sep = "")
  for (f in object$files) cat("    - ", f, "\n", sep = "")
  invisible(object)
}

#' @export
print.svt_graph <- function(x, ...) {
  cat("<svt_graph>\n")
  cat("  nodes:    ", nrow(x$nodes), "\n", sep = "")
  cat("  edges:    ", nrow(x$edges), "\n", sep = "")
  cat("  warnings: ", nrow(x$warnings), "\n", sep = "")
  invisible(x)
}

#' @export
summary.svt_graph <- function(object, ...) {
  cat("svt_graph summary\n")
  cat("  files:    ", nrow(object$files), "\n", sep = "")
  cat("  imports:  ", nrow(object$imports), "\n", sep = "")
  cat("  sources:  ", nrow(object$sources), "\n", sep = "")
  cat("  nodes:    ", nrow(object$nodes), "\n", sep = "")
  cat("  edges:    ", nrow(object$edges), "\n", sep = "")
  cat("  warnings: ", nrow(object$warnings), "\n", sep = "")
  if (nrow(object$warnings)) {
    by_code <- table(object$warnings$code)
    for (cd in names(by_code)) {
      cat("    - ", cd, ": ", by_code[[cd]], "\n", sep = "")
    }
  }
  invisible(object)
}

#' @export
print.svt_features <- function(x, ...) {
  feats <- Filter(function(r) identical(r$kind, "feature"), x$records)
  mods <- Filter(function(r) identical(r$kind, "module"), x$records)
  cat("<svt_features>\n")
  cat("  features: ", length(feats), "\n", sep = "")
  cat("  modules:  ", length(mods), "\n", sep = "")
  cat("  manifest: ", if (x$manifest_supplied) "supplied" else "default", "\n",
      sep = "")
  invisible(x)
}

#' @export
summary.svt_features <- function(object, ...) {
  print(object)
  if (length(object$records)) {
    for (r in object$records) {
      cat("  - ", r$kind, ":", r$name, " (",
          length(r$node_ids), " nodes)\n", sep = "")
    }
  }
  if (nrow(object$manifest_issues)) {
    cat("  manifest issues (", nrow(object$manifest_issues), "):\n", sep = "")
    for (i in seq_len(nrow(object$manifest_issues))) {
      cat("    - ", object$manifest_issues$code[i], ": ",
          object$manifest_issues$message[i], "\n", sep = "")
    }
  }
  invisible(object)
}

#' @export
print.svt_inventory <- function(x, ...) {
  cat("<svt_inventory>\n")
  cat("  features:  ", length(x$features), "\n", sep = "")
  invisible(x)
}

#' @export
summary.svt_inventory <- function(object, ...) {
  cat("svt_inventory summary\n")
  for (nm in names(object$features)) {
    inv <- object$features[[nm]]
    cat("  - ", nm, ": ", nrow(inv$functions), " functions, ",
        nrow(inv$packages), " packages, ",
        nrow(inv$warnings), " warnings\n", sep = "")
  }
  invisible(object)
}

#' @export
print.svt_validation <- function(x, ...) {
  cat("<svt_validation>\n")
  cat("  out_dir:     ", x$out_dir, "\n", sep = "")
  cat("  features:    ", x$n_features, "\n", sep = "")
  cat("  modules:     ", x$n_modules, "\n", sep = "")
  cat("  doc stubs:   ", length(x$doc_stubs), "\n", sep = "")
  cat("  inventories: ", length(x$inventories), "\n", sep = "")
  cat("  widgets:     ", length(x$widgets), "\n", sep = "")
  cat("  surfaces:    ", length(x$test_surfaces), "\n", sep = "")
  cat("  scaffolds:   ", length(x$scaffolds), "\n", sep = "")
  cat("  index:       ", length(x$index), "\n", sep = "")
  cat("  manifest:    ", length(x$manifest), "\n", sep = "")
  invisible(x)
}

#' @export
summary.svt_validation <- function(object, ...) {
  print(object)
  for (p in object$doc_stubs) cat("    - ", p, "\n", sep = "")
  invisible(object)
}

# helpers ----------------------------------------------------------------

#' If `app_path` is a `.zip`, extract to a session tempdir; else return
#' the normalized absolute path. Extracted directories are cached per
#' archive path so repeated parses share a single extracted copy.
#'
#' @noRd
resolve_app_path <- function(app_path) {
  if (!is.character(app_path) || length(app_path) != 1L || is.na(app_path)) {
    stop("app_path must be a single, non-NA string.", call. = FALSE)
  }
  if (grepl("\\.zip$", app_path, ignore.case = TRUE) && file.exists(app_path)) {
    cache <- svt_zip_cache
    if (!is.null(cache[[app_path]])) return(cache[[app_path]])
    dest <- file.path(tempfile("svt-zip-"))
    dir.create(dest, recursive = TRUE)
    utils::unzip(app_path, exdir = dest)
    contents <- list.files(dest, full.names = TRUE)
    root <- if (length(contents) == 1L && dir.exists(contents)) {
      contents[1L]
    } else {
      dest
    }
    cache[[app_path]] <- root
    return(root)
  }
  if (!dir.exists(app_path)) {
    stop("app_path does not exist: ", app_path, call. = FALSE)
  }
  normalizePath(app_path, winslash = "/", mustWork = TRUE)
}

svt_zip_cache <- new.env(parent = emptyenv())

#' Accept either a path or an in-memory list and return the canonical
#' manifest shape (with `features` and `modules` always present).
#'
#' @noRd
normalize_manifest <- function(manifest) {
  if (is.null(manifest)) return(empty_manifest())
  if (is.character(manifest) && length(manifest) == 1L) {
    return(read_manifest(manifest))
  }
  if (is.list(manifest)) {
    return(list(
      features = as.list(manifest$features %||% list()),
      modules = as.list(manifest$modules %||% list()),
      report = manifest$report
    ))
  }
  stop("manifest must be a path or a list.", call. = FALSE)
}

#' @noRd
filter_records <- function(records, features, modules) {
  if (is.null(features) && is.null(modules)) return(records)
  Filter(function(r) {
    if (identical(r$kind, "feature")) {
      return(is.null(features) || r$name %in% features)
    }
    if (identical(r$kind, "module")) {
      return(is.null(modules) || r$name %in% modules)
    }
    TRUE
  }, records)
}

#' @noRd
summarize_features <- function(feats, graph) {
  if (!length(feats)) {
    return(tibble::tibble(
      name = character(), kind = character(),
      nodes = integer(), edges = integer()
    ))
  }
  tibble::tibble(
    name = vapply(feats, function(f) f$name, character(1)),
    kind = vapply(feats, function(f) f$kind, character(1)),
    nodes = vapply(feats, function(f) length(f$node_ids), integer(1)),
    edges = vapply(feats, function(f) length(f$edge_ids), integer(1))
  )
}

