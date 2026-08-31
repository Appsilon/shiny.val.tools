#' The skip marker every generated `test_that()` block opens with.
#'
#' Visible in `testthat` output, countable, non-blocking, and detectable
#' by the coverage classifier: an unfilled scaffold is classified
#' `scaffold`, never `covered`. Removing this line is the developer's
#' explicit act of taking ownership of the test.
#'
#' @noRd
scaffold_skip_marker <- function() {
  'skip("SVT scaffold - assertions not written (SVT-W303)")'
}

#' Which flavor of harness header a generated scaffold needs.
#'
#' `package` — the modules are exported by a source package, so the test
#' loads the package. `box` — a rhino/box app, so the test imports through
#' `box::use()`. `traditional` — `source()`-based, so the test sources the
#' defining file and its known dependencies.
#'
#' @noRd
scaffold_flavor <- function(app_path) {
  if (is_package_target(app_path)) return("package")
  imports <- build_imports_table(app_path)
  if (nrow(imports) && any(!is.na(imports$local_path))) return("box")
  "traditional"
}

#' The `Package:` field of a source package target.
#' @noRd
package_target_name <- function(app_path) {
  dcf <- tryCatch(read.dcf(file.path(app_path, "DESCRIPTION"), fields = "Package"),
                  error = function(e) NULL)
  if (is.null(dcf) || !nrow(dcf) || is.na(dcf[1L, 1L])) return(basename(app_path))
  as.character(dcf[1L, 1L])
}

#' The Definitions row describing a module surface, or NULL.
#'
#' Module records are keyed by the graph's module identity for apps and by
#' the exported server function for package targets (spec 03), so both are
#' accepted as the lookup key.
#'
#' @noRd
module_def_row <- function(name, defs) {
  rows <- defs[defs$kind == "module_server", , drop = FALSE]
  if (!nrow(rows)) return(NULL)
  hit <- rows$name == name |
    (!is.na(rows$wrapper_binding) & rows$wrapper_binding == name)
  if (!any(hit)) return(NULL)
  rows[which(hit)[1L], , drop = FALSE]
}

#' The `args = list(...)` a module's `testServer()` call needs.
#'
#' A module wrapper takes `id` plus whatever the module needs passed in
#' (another module's returned reactive, a dataset, a flag). A scaffold
#' that names only `id` does not run for those modules, so every formal
#' is emitted — `id` seeded with a test namespace, the rest with NULL for
#' the developer to fill.
#'
#' @noRd
render_module_args <- function(def_row) {
  formals_txt <- if (is.null(def_row) || is.na(def_row$wrapper_formals)) {
    ""
  } else {
    def_row$wrapper_formals
  }
  nms <- if (!nzchar(formals_txt)) "id" else
    strsplit(formals_txt, ",", fixed = TRUE)[[1L]]
  if (!"id" %in% nms) nms <- c("id", nms)
  parts <- vapply(nms, function(n) {
    if (identical(n, "id")) 'id = "test"' else paste0(n, " = NULL")
  }, character(1))
  paste0("args = list(", paste(parts, collapse = ", "), ")")
}

#' Format a `session$setInputs()` block from the surface's stimuli.
#'
#' Names are padded to a common width so the trailing "drives" comments
#' line up; the padding is derived from the names themselves and so stays
#' deterministic.
#'
#' @noRd
render_stimuli_block <- function(stimuli, indent, blockers = character()) {
  if (!nrow(stimuli)) {
    return(paste0(indent, if (length(blockers)) {
      paste0("# (no stimuli derived - the subgraph has blockers: ",
             paste(blockers, collapse = ", "), ")")
    } else {
      "# (no inputs drive this subgraph - it is driven by its arguments)"
    }))
  }
  width <- max(nchar(stimuli$name))
  last <- nrow(stimuli)
  rows <- vapply(seq_len(last), function(i) {
    drives <- stimuli$drives[[i]]
    comment <- if (length(drives)) {
      paste0("  # drives: ", paste(drives, collapse = ", "))
    } else {
      "  # drives nothing in this subgraph"
    }
    paste0(indent, "  ", formatC(stimuli$name[i], width = -width), " = NULL",
           if (i < last) "," else "", comment)
  }, character(1))
  c(paste0(indent, "session$setInputs("), rows, paste0(indent, ")"))
}

#' Format the observable / internal / terminal comment blocks.
#'
#' Assertions are never generated — an expected value is a human judgment
#' about the analysis. What is generated is the plumbing plus, for an
#' opaque observable, the redirection to where the real assertion belongs.
#'
#' @noRd
render_assertion_block <- function(surface, indent) {
  out <- character()

  if (nrow(surface$observables)) {
    out <- c(out, paste0(indent, "# observables"))
    for (i in seq_len(nrow(surface$observables))) {
      nm <- surface$observables$name[i]
      call <- surface$observables$def_call[i]
      call_text <- if (is.na(call)) "unknown call" else call
      if (identical(surface$observables$observable_via[i], "opaque")) {
        out <- c(out,
          paste0(indent, "# output$", nm, " is ", call_text,
                 " - opaque under testServer (SVT-W312)."),
          paste0(indent, "# Assert that it is produced and check its ",
                 "structure; assert the analysis"),
          paste0(indent, "# itself on the reactive or helper that computed ",
                 "the data."))
      } else if (identical(surface$observables$kind[i], "reactive")) {
        out <- c(out, paste0(indent, "# expect_equal(", nm, "(), ...)   # ",
                             call_text))
      } else {
        out <- c(out, paste0(indent, "# expect_equal(output$", nm,
                             ", ...)   # ", call_text))
      }
    }
  }

  if (nrow(surface$internals)) {
    out <- c(out, "",
             paste0(indent, "# internals - readable by name under testServer"))
    for (nm in surface$internals$name) {
      out <- c(out, paste0(indent, "# expect_equal(", nm, "(), ...)"))
    }
  }

  if (identical(surface$kind, "module")) {
    out <- c(out, "", paste0(indent, "# returned reactives"),
             paste0(indent, "# expect_equal(session$returned(), ...)"))
  }

  if (nrow(surface$terminals)) {
    out <- c(out, "", paste0(indent, "# terminals - trusted, validated separately"))
    for (i in seq_len(nrow(surface$terminals))) {
      art <- surface$terminals$artifact[i]
      out <- c(out, paste0(indent, "#   ", surface$terminals$name[i],
                           if (is.na(art)) "" else paste0(" - see ", art)))
    }
  }

  out
}

#' The generated file header, shared by every scaffold.
#' @noRd
scaffold_header <- function(surface, label) {
  c(paste0("# Generated by shiny.val.tools ", svt_version(),
           " - scaffold; fill in the assertions."),
    paste0("# ", label, ": ", surface$name,
           "    surface_hash: ", substr(surface$surface_hash, 1L, 8L)),
    "# Written for tests/testthat/ - paths resolve relative to that directory.",
    paste0("# @covers ", if (identical(surface$kind, "module")) "module" else
           "feature", ": ", surface$name))
}

#' The analysing package's version, as stamped into generated files and
#' into the validation report's document control block.
#'
#' Degrades to "(unknown)" rather than erroring when the package is not
#' installed (e.g. under `devtools::load_all()` in a bare checkout).
#'
#' @noRd
svt_version <- function() {
  v <- tryCatch(as.character(utils::packageVersion("shiny.val.tools")),
                error = function(e) NA_character_)
  if (is.na(v)) "(unknown)" else v
}

#' Render one surface's scaffold file.
#'
#' @noRd
render_surface_scaffold <- function(surface, flavor, defs, sources, app_path) {
  is_module <- identical(surface$kind, "module")
  header <- scaffold_header(surface, if (is_module) "Module" else "Feature")
  body_indent <- "    "

  preamble <- character()
  if (is_module) {
    def_row <- module_def_row(surface$name, defs)
    if (flavor == "box") {
      identity <- if (is.null(def_row)) surface$name else def_row$name
      alias <- basename(identity)
      preamble <- c(
        "",
        "box::use(",
        "  shiny[testServer],",
        "  testthat[expect_equal, skip, test_that],",
        ")",
        paste0("box::use(", identity, ")")
      )
      server_expr <- paste0("testServer(", alias, "$server")
    } else if (flavor == "package") {
      preamble <- c("", paste0("library(", package_target_name(app_path), ")"))
      binding <- if (is.null(def_row) || is.na(def_row$wrapper_binding)) {
        surface$name
      } else {
        def_row$wrapper_binding
      }
      server_expr <- paste0("shiny::testServer(", binding)
    } else {
      def_file <- if (is.null(def_row)) NA_character_ else def_row$from_file
      preamble <- c("",
        "# The module file is sourced directly, so the packages its body uses",
        "# must be attached here the way the app's global.R attaches them.",
        "library(shiny)",
        'app_root <- testthat::test_path("..", "..")',
        "# Dependency list is best-effort: the files this module's helpers are",
        "# defined in, plus what the Sources table knows. Complete it if the",
        "# module reaches further.")
      for (dep in scaffold_dependencies(surface, def_file, sources)) {
        preamble <- c(preamble,
                      paste0('source(file.path(app_root, "', dep, '"))'))
      }
      if (!is.na(def_file)) {
        preamble <- c(preamble,
                      paste0('source(file.path(app_root, "', def_file, '"))'))
      }
      binding <- if (is.null(def_row) || is.na(def_row$wrapper_binding)) {
        surface$name
      } else {
        def_row$wrapper_binding
      }
      server_expr <- paste0("shiny::testServer(", binding)
    }
    open_call <- paste0("  ", server_expr, ", ", render_module_args(def_row),
                        ", {")
  } else {
    if (flavor == "box") {
      preamble <- c("",
        "box::use(",
        "  shiny[testServer],",
        "  testthat[expect_equal, skip, test_that],",
        ")")
      open_call <- '  testServer(testthat::test_path("..", ".."), {'
    } else {
      open_call <-
        '  shiny::testServer(testthat::test_path("..", ".."), {'
    }
  }

  title <- paste0(if (is_module) "module " else "feature ", surface$name,
                  " - observables respond to stimuli")

  squeeze_blanks(c(
    header,
    preamble,
    "",
    paste0('test_that("', title, '", {'),
    paste0("  ", scaffold_skip_marker()),
    "",
    open_call,
    paste0(body_indent, "# stimuli"),
    render_stimuli_block(surface$stimuli, body_indent, surface$blockers),
    "",
    render_assertion_block(surface, body_indent),
    "  })",
    "})",
    ""))
}

#' Collapse runs of blank lines to one.
#'
#' A surface with no observables or no internals leaves its section empty,
#' and two adjacent separators would render as a double blank line. The
#' generated file is read by humans; keep it tidy.
#'
#' @noRd
squeeze_blanks <- function(lines) {
  blank <- !nzchar(trimws(lines))
  keep <- !(blank & c(FALSE, blank[-length(blank)]))
  lines[keep]
}

#' Files a traditional-flavor module scaffold must source before its own.
#'
#' The surface already knows where each helper it calls is defined, which
#' is a more precise answer than the Sources table alone can give; the
#' Sources table adds whatever the defining file pulls in explicitly.
#'
#' @noRd
scaffold_dependencies <- function(surface, def_file, sources) {
  helper_files <- if (nrow(surface$helpers)) unique(surface$helpers$file) else
    character()
  sourced <- if (nrow(sources) && !is.na(def_file)) {
    sources$to_file[sources$from_file == def_file]
  } else {
    character()
  }
  deps <- unique(c(sort(sourced), sort(helper_files)))
  deps[!is.na(deps) & deps != def_file]
}

#' Render one surface's helper unit stubs.
#'
#' Helper stubs are plain `testthat` and need no Shiny harness at all,
#' which is what makes a `method`-category helper the cheapest and
#' highest-value test in the app.
#'
#' @noRd
render_helper_scaffold <- function(surface) {
  helpers <- surface$helpers
  if (!nrow(helpers)) return(NULL)

  out <- c(
    paste0("# Generated by shiny.val.tools ", svt_version(),
           " - scaffold; fill in the assertions."),
    paste0("# Helpers called by ", surface$kind, " ", surface$name,
           "    surface_hash: ", substr(surface$surface_hash, 1L, 8L)),
    "# Ordered by category: method first - that is where the analytical",
    "# risk concentrates, and it needs no Shiny harness to test."
  )

  for (i in seq_len(nrow(helpers))) {
    out <- c(out, "",
      paste0("# @covers fn: ", helpers$fn[i]),
      paste0('test_that("', helpers$fn[i], '", {'),
      paste0("  ", scaffold_skip_marker()),
      paste0("  # Defined at ", helpers$file[i], ":", helpers$line[i], "."),
      paste0("  # Category: ", helpers$category[i],
             if (identical(helpers$category[i], "unset"))
               " (declare package_categories: {app: ...} in features.yml)"
             else " - declared in features.yml"),
      paste0("  # expect_equal(", helpers$fn[i], "(...), ...)"),
      "})")
  }
  c(out, "")
}

#' Plan the scaffold files a set of surfaces produces.
#'
#' Paths are relative to `out_dir`; the same plan drives both writing and
#' lifecycle tracking, so a scaffold is never mistaken for an orphan.
#'
#' @noRd
scaffold_plan <- function(surfaces, features = NULL) {
  names_v <- character(); kinds <- character(); rels <- character()
  for (nm in sort(names(surfaces))) {
    if (!is.null(features) && !nm %in% features) next
    slug <- slugify_artifact_name(nm)
    names_v <- c(names_v, nm)
    kinds <- c(kinds, "surface")
    rels <- c(rels, paste0("tests/test-svt-", slug, ".R"))
    if (nrow(surfaces[[nm]]$helpers)) {
      names_v <- c(names_v, nm)
      kinds <- c(kinds, "helpers")
      rels <- c(rels, paste0("tests/test-svt-helpers-", slug, ".R"))
    }
  }
  tibble::tibble(name = names_v, kind = kinds, rel_path = rels)
}

#' Write scaffold files, honouring the never-overwrite-an-edit rule.
#'
#' Staging: a file we wrote and nobody touched refreshes; a file whose
#' MD5 has moved away from the manifest's record is a test somebody
#' wrote, and is left alone. A file with no manifest record is left alone
#' too — we only overwrite what we can prove we generated.
#'
#' App: an existing path is never written. SVT-W309.
#'
#' @noRd
write_scaffolds <- function(surfaces, out_dir, app_path, target = "staging",
                            features = NULL, prior = NULL) {
  plan <- scaffold_plan(surfaces, features)
  flavor <- scaffold_flavor(app_path)
  defs <- build_definitions_table(app_path)
  sources <- build_sources_table(app_path)

  base_dir <- if (target == "app") {
    file.path(app_path, "tests", "testthat")
  } else {
    out_dir
  }

  names_v <- character(); kinds <- character(); rels <- character()
  paths <- character(); statuses <- character(); warns <- list()
  preserved <- character()

  for (i in seq_len(nrow(plan))) {
    surface <- surfaces[[plan$name[i]]]
    text <- if (plan$kind[i] == "surface") {
      render_surface_scaffold(surface, flavor, defs, sources, app_path)
    } else {
      render_helper_scaffold(surface)
    }
    if (is.null(text)) next

    rel <- plan$rel_path[i]
    path <- if (target == "app") {
      file.path(base_dir, basename(rel))
    } else {
      file.path(base_dir, rel)
    }
    dir <- dirname(path)
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

    status <- "written"
    row_warns <- "SVT-W303"

    if (file.exists(path)) {
      if (target == "app") {
        status <- "skipped_exists"
        row_warns <- "SVT-W309"
      } else {
        recorded <- if (is.null(prior)) NULL else prior$md5[prior$path == rel]
        actual <- unname(tools::md5sum(path))
        if (!length(recorded) || !identical(recorded[1L], actual)) {
          status <- "skipped_edited"
          row_warns <- character()
          if (length(recorded)) {
            preserved[rel] <- recorded[1L]
          }
        }
      }
    }

    if (status == "written") {
      new_text <- paste(text, collapse = "\n")
      if (file.exists(path) &&
          identical(paste(readLines(path, warn = FALSE), collapse = "\n"),
                    new_text)) {
        status <- "unchanged"
      } else {
        writeLines(text, path)
      }
    }

    # Accumulated alongside the row rather than recovered from `plan` by
    # position afterwards: the loop can `next` past a plan row (a renderer
    # that declines to emit), and positional indexing would then shift
    # every later row onto the wrong name and kind.
    names_v <- c(names_v, plan$name[i])
    kinds <- c(kinds, plan$kind[i])
    rels <- c(rels, rel)
    paths <- c(paths, path)
    statuses <- c(statuses, status)
    warns[[length(warns) + 1L]] <- row_warns
  }

  list(
    results = tibble::tibble(
      name = names_v,
      kind = kinds,
      path = paths,
      status = statuses,
      warnings = warns
    ),
    rel_paths = rels,
    preserved = preserved
  )
}
