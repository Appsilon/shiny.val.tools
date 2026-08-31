#' The manifest entry for a feature or module name, or NULL.
#'
#' Features and modules share one name space (the manifest validator
#' enforces it), so one lookup over both blocks is unambiguous.
#'
#' @noRd
manifest_entry <- function(manifest, name) {
  for (block in list(manifest$features %||% list(),
                     manifest$modules %||% list())) {
    for (e in block) {
      if (identical(e$name %||% NA_character_, name)) return(e)
    }
  }
  NULL
}

#' Resolve a `testServer()` first argument to a module identity, or NA.
#'
#' Three shapes, all of which a real app produces: the module identity
#' itself, a `box::use()` alias (`counter` for `app/view/counter`), and a
#' wrapper binding (`mod_card_server`) recorded in the Definitions table.
#'
#' @noRd
resolve_module_target <- function(server_target, module_names, defs) {
  if (is.na(server_target)) return(NA_character_)
  if (server_target %in% module_names) return(server_target)
  by_base <- module_names[basename(module_names) == server_target]
  if (length(by_base) == 1L) return(by_base)
  if (nrow(defs) && "wrapper_binding" %in% names(defs)) {
    hit <- defs$kind == "module_server" & !is.na(defs$wrapper_binding) &
      defs$wrapper_binding == server_target
    if (any(hit)) {
      nm <- defs$name[which(hit)[1L]]
      if (nm %in% module_names) return(nm)
    }
  }
  NA_character_
}

#' Link every discovered test to the surfaces it exercises.
#'
#' Annotation is the contract and wins outright; inference is the
#' convenience that follows. Each resolved link records which mechanism
#' produced it, so a reviewer reading the matrix can see how much of it
#' is inferred.
#'
#' Returns the tests table with two added list-columns: `targets`
#' (`"feature:km_plot"`, `"module:app/view/mod_card"`, `"fn:compute"`)
#' and `link` (the mechanism, one value per row).
#'
#' @noRd
link_tests <- function(tests, surfaces, defs) {
  if (!nrow(tests)) {
    tests$targets <- list()
    tests$link <- character()
    tests$unknown_targets <- list()
    return(tests)
  }

  kinds <- vapply(surfaces, function(s) s$kind, character(1))
  feature_names <- names(surfaces)[kinds == "feature"]
  module_names <- names(surfaces)[kinds == "module"]
  helper_names <- unique(unlist(lapply(surfaces, function(s) s$helpers$fn),
                                use.names = FALSE))
  helper_names <- helper_names %||% character()

  known <- c(paste0("feature:", feature_names),
             paste0("module:", module_names),
             paste0("fn:", helper_names))

  targets <- vector("list", nrow(tests))
  unknown <- vector("list", nrow(tests))
  link <- character(nrow(tests))

  for (i in seq_len(nrow(tests))) {
    if (isTRUE(tests$is_support[i])) {
      targets[[i]] <- character()
      unknown[[i]] <- character()
      link[i] <- NA_character_
      next
    }

    ann <- tests$annotations[[i]]
    unknown[[i]] <- sort(setdiff(ann, known))
    resolved <- sort(intersect(ann, known))

    if (length(resolved)) {
      targets[[i]] <- resolved
      link[i] <- "annotation"
      next
    }

    inferred <- character()
    mod <- resolve_module_target(tests$server_target[i], module_names, defs)
    if (!is.na(mod)) {
      inferred <- paste0("module:", mod)
    } else if (identical(tests$harness[i], "testserver")) {
      # A testServer() block driving the app directory: the outputs it
      # reads name the features it exercises.
      touched <- tests$touched_outputs[[i]]
      for (nm in feature_names) {
        if (any(surfaces[[nm]]$observables$name %in% touched)) {
          inferred <- c(inferred, paste0("feature:", nm))
        }
      }
    } else {
      called <- tests$called_functions[[i]]
      hits <- intersect(helper_names, called)
      if (length(hits)) inferred <- paste0("fn:", sort(hits))
    }

    targets[[i]] <- sort(unique(inferred))
    link[i] <- if (length(inferred)) "inference" else NA_character_
  }

  tests$targets <- targets
  tests$link <- link
  tests$unknown_targets <- unknown
  tests
}

#' Row indexes of the tests mapped to one surface.
#'
#' A `fn:` link maps a helper unit test to every surface whose helper set
#' contains that function — the helper is where the feature's analytical
#' risk lives, so exercising it is evidence about the feature.
#'
#' @noRd
tests_for_surface <- function(tests, surface) {
  if (!nrow(tests)) return(integer())
  key <- paste0(surface$kind, ":", surface$name)
  fn_keys <- if (nrow(surface$helpers)) paste0("fn:", surface$helpers$fn) else
    character()
  which(vapply(tests$targets,
               function(t) any(t %in% c(key, fn_keys)), logical(1)))
}

#' Classify one surface's coverage status.
#'
#' `covered` requires every observable exercised and at least one
#' stimulus set; a surface with no stimuli satisfies the second clause
#' vacuously, since a module driven only by its arguments is not
#' permanently uncoverable. Coverage means *exercised*, never *correct*.
#'
#' @noRd
classify_surface <- function(surface, tests, idx, manifest_tests) {
  observables <- surface$observables$name
  stimuli <- surface$stimuli$name
  # `manifest_tests` is a tibble: length() would count its columns.
  n_manifest <- nrow(manifest_tests)

  if (!length(idx) && !n_manifest) {
    return(list(status = "uncovered",
                unexercised = list(observables = observables,
                                   stimuli = stimuli)))
  }

  filled_idx <- idx[vapply(idx, function(i) isTRUE(tests$filled[i]),
                           logical(1))]
  if (!length(filled_idx) && !n_manifest) {
    return(list(status = "scaffold",
                unexercised = list(observables = observables,
                                   stimuli = stimuli)))
  }

  # A manifest `tests:` entry is a human declaration of evidence this
  # tool cannot read. We record where the credit came from rather than
  # second-guessing it, exactly as with `verification: not_required`.
  if (n_manifest) {
    return(list(status = "covered",
                unexercised = list(observables = character(),
                                   stimuli = character())))
  }

  touched_in <- unique(unlist(tests$touched_inputs[filled_idx],
                              use.names = FALSE)) %||% character()
  reads <- unique(c(unlist(tests$touched_outputs[filled_idx],
                           use.names = FALSE),
                    unlist(tests$called_functions[filled_idx],
                           use.names = FALSE))) %||% character()

  missing_obs <- setdiff(observables, reads)
  missing_stim <- setdiff(stimuli, touched_in)
  stimulus_ok <- !length(stimuli) || length(intersect(stimuli, touched_in))

  status <- if (!length(missing_obs) && stimulus_ok) "covered" else "partial"
  list(status = status,
       unexercised = list(observables = missing_obs, stimuli = missing_stim))
}

#' Build the coverage record for every surface.
#'
#' @noRd
build_test_coverage <- function(surfaces, records, manifest, test_path,
                                app_path, results = NULL) {
  tests <- build_tests_table(test_path, app_path)
  defs <- build_definitions_table(app_path)
  tests <- link_tests(tests, surfaces, defs)
  results_tbl <- read_test_results(results)
  tests$result <- stamp_results(tests, results_tbl)

  by_name <- list()
  for (rec in records) by_name[[rec$name]] <- rec

  entries <- list()
  for (nm in names(surfaces)) {
    surface <- surfaces[[nm]]
    rec <- by_name[[nm]]
    entry <- manifest_entry(manifest, nm)

    mtests <- manifest_test_links(entry, app_path)
    idx <- tests_for_surface(tests, surface)

    verification <- entry$verification %||% "required"
    warns <- character()

    if (identical(verification, "not_required")) {
      # SVT-W311 (a waiver with no rationale) is a manifest-validity
      # issue and is raised by `validate_manifest()`, alongside its
      # risk-classification twin SVT-W105. Raising it here too would
      # double-count it in every aggregate.
      status <- "waived"
      unexercised <- list(observables = character(), stimuli = character())
    } else {
      cls <- classify_surface(surface, tests, idx, mtests$rows)
      status <- cls$status
      unexercised <- cls$unexercised
      warns <- c(warns, switch(status,
                               uncovered = "SVT-W301",
                               partial = "SVT-W302",
                               scaffold = "SVT-W303",
                               character()))
    }

    warns <- c(warns, mtests$warnings)
    mapped <- rbind(mapped_tests_tibble(tests, idx), mtests$rows)
    if (nrow(mapped) && any(mapped$result %in% c("fail", "missing"))) {
      warns <- c(warns, "SVT-W310")
    }

    entries[[nm]] <- list(
      name = nm,
      kind = surface$kind,
      intended_use_declared = declared_intended_use(rec),
      risk_classification = rec$risk_classification %||% NA_character_,
      surface_hash = surface$surface_hash,
      status = status,
      tests = mapped,
      unexercised = unexercised,
      warnings = unique(warns)
    )
  }

  orphan_idx <- which(!tests$is_support &
                        vapply(tests$targets, function(t) !length(t),
                               logical(1)))
  unknown_idx <- which(vapply(tests$unknown_targets,
                              function(t) length(t) > 0L, logical(1)))

  list(
    entries = entries[order(names(entries))],
    tests = tests,
    orphans = orphan_idx,
    unknown = unknown_idx,
    inputs = list(test_path = test_path,
                  test_path_rel = relative_to_app(test_path, app_path),
                  results = results),
    summary = coverage_summary(entries, orphan_idx)
  )
}

#' `path` expressed relative to `app_path`, or its basename.
#'
#' Artifacts must carry no absolute path: the determinism contract says
#' the same source produces byte-identical output, and a machine-specific
#' prefix breaks that on the first checkout elsewhere.
#'
#' @noRd
relative_to_app <- function(path, app_path) {
  if (is.null(path)) return(NA_character_)
  if (is.null(app_path)) return(basename(path))
  p <- normalizePath(path, winslash = "/", mustWork = FALSE)
  a <- normalizePath(app_path, winslash = "/", mustWork = FALSE)
  if (startsWith(p, paste0(a, "/"))) return(substring(p, nchar(a) + 2L))
  basename(p)
}

#' Was an intended use declared for this record?
#' @noRd
declared_intended_use <- function(rec) {
  iu <- rec$intended_use %||% NA_character_
  !is.null(iu) && length(iu) && !is.na(iu[1L]) &&
    nzchar(trimws(as.character(iu[1L])))
}

#' The mapped-tests tibble for one surface.
#' @noRd
mapped_tests_tibble <- function(tests, idx) {
  if (!length(idx)) return(empty_mapped_tests())
  tibble::tibble(
    file = tests$file[idx],
    line = tests$line[idx],
    desc = tests$desc[idx],
    harness = tests$harness[idx],
    link = tests$link[idx],
    filled = tests$filled[idx],
    md5 = tests$md5[idx],
    result = tests$result[idx]
  )
}

#' @noRd
empty_mapped_tests <- function() {
  tibble::tibble(file = character(), line = integer(), desc = character(),
                 harness = character(), link = character(),
                 filled = logical(), md5 = character(), result = character())
}

#' Manifest-declared evidence links for one surface.
#'
#' `file:` entries are checked for existence (missing → SVT-W305);
#' `external:` entries are recorded verbatim and never checked, because
#' a UAT record or a `valtools` test case id lives outside this repo.
#'
#' @noRd
manifest_test_links <- function(entry, app_path) {
  raw <- entry$tests %||% list()
  if (!length(raw)) {
    return(list(rows = empty_mapped_tests(), warnings = character()))
  }
  files <- character(); lines <- integer(); descs <- character()
  warns <- character()
  for (t in raw) {
    t <- as.list(t)
    note <- t$note %||% NA_character_
    if (!is.null(t$file)) {
      files <- c(files, as.character(t$file))
      if (!is.null(app_path) &&
          !file.exists(file.path(app_path, as.character(t$file)))) {
        warns <- c(warns, "SVT-W305")
      }
    } else if (!is.null(t$external)) {
      files <- c(files, as.character(t$external))
    } else {
      next
    }
    lines <- c(lines, NA_integer_)
    descs <- c(descs, if (is.null(note) || is.na(note)) NA_character_ else
      as.character(note))
  }
  if (!length(files)) {
    return(list(rows = empty_mapped_tests(), warnings = warns))
  }
  list(
    rows = tibble::tibble(
      file = files, line = lines, desc = descs,
      harness = rep("external", length(files)),
      link = rep("manifest", length(files)),
      filled = rep(TRUE, length(files)),
      md5 = rep(NA_character_, length(files)),
      result = rep("unknown", length(files))
    ),
    warnings = unique(warns)
  )
}

#' Status counts plus the orphan count.
#' @noRd
coverage_summary <- function(entries, orphan_idx) {
  statuses <- vapply(entries, function(e) e$status, character(1))
  counts <- vapply(c("covered", "partial", "scaffold", "uncovered", "waived"),
                   function(s) sum(statuses == s), integer(1))
  as.list(c(counts, orphan_tests = length(orphan_idx)))
}

# Result ingestion -------------------------------------------------------

#' Read a `testthat::JunitReporter()` XML report.
#'
#' JUnit XML is the only format read: it is documented, producer-
#' independent, and already what CI emits. Parsing is delegated to
#' `xml2` — hand-rolling an XML reader to save a suggested dependency
#' would reinvent the one part of this path that is easy to get subtly
#' wrong.
#'
#' We ingest, we never execute: running the suite needs the app's data,
#' credentials and package library, and staying out of it keeps the
#' package's static-analysis-only contract intact.
#'
#' @noRd
read_test_results <- function(path) {
  if (is.null(path)) return(NULL)
  if (!file.exists(path)) {
    stop("test_results file does not exist: ", path, call. = FALSE)
  }
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("Reading a JUnit report requires the 'xml2' package.", call. = FALSE)
  }
  doc <- xml2::read_xml(path)
  cases <- xml2::xml_find_all(doc, ".//testcase")
  if (!length(cases)) {
    return(tibble::tibble(name = character(), result = character()))
  }
  name <- xml2::xml_attr(cases, "name")
  result <- vapply(cases, function(node) {
    if (length(xml2::xml_find_all(node, "./error|./failure"))) return("fail")
    if (length(xml2::xml_find_all(node, "./skipped"))) return("skip")
    "pass"
  }, character(1))
  tibble::tibble(name = name, result = result)
}

#' Stamp an ingested result onto every discovered test row.
#'
#' `missing` — the test exists in the source but not in the report — is
#' as much a finding as a failure: it is a test CI did not run.
#'
#' @noRd
stamp_results <- function(tests, results_tbl) {
  if (!nrow(tests)) return(character())
  if (is.null(results_tbl)) return(rep("unknown", nrow(tests)))
  vapply(seq_len(nrow(tests)), function(i) {
    hit <- which(results_tbl$name == tests$desc[i])
    if (!length(hit)) return("missing")
    results_tbl$result[hit[1L]]
  }, character(1))
}

#' Abort when a high-risk feature has no mapped test.
#'
#' The one place the package takes a position: a high-risk feature with
#' no test is the defect worth failing a build over. Off by default;
#' `strict_verification = TRUE` is the intended CI gate.
#'
#' @noRd
enforce_strict_verification <- function(coverage) {
  offenders <- Filter(function(e) {
    identical(e$risk_classification, "high") && identical(e$status, "uncovered")
  }, coverage$entries)
  if (!length(offenders)) return(invisible())
  stop("strict_verification: SVT-W301 on high-risk ",
       ngettext(length(offenders), "feature", "features"), ":\n",
       paste(paste0("  - ", vapply(offenders, function(e) e$name,
                                   character(1))), collapse = "\n"),
       call. = FALSE)
}
