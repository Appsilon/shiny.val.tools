#' The disclaimer every coverage artifact carries.
#'
#' Coverage in this layer means *exercised*, never *correct*. A feature
#' reported `covered` has tests that reach its roots; whether they assert
#' the right thing is the reviewer's call, and the artifact says so where
#' it will be read rather than in a footnote.
#'
#' @noRd
coverage_disclaimer <- function() {
  paste0("Coverage here means **exercised**, never **correct**. A feature ",
         "reported `covered` has tests that reach its observables; whether ",
         "those tests assert the right thing is a reviewer's judgment. ",
         "Tests are never executed by this tool.")
}

#' Render `traceability.json`.
#'
#' `schema_version` 1.0, under the same additive-only stability contract
#' as `inventory.json`. Every list is sorted by the derivation, so the
#' output is byte-deterministic.
#'
#' @noRd
render_traceability_json <- function(coverage) {
  entry_objs <- vapply(coverage$entries, function(e) {
    t <- e$tests
    test_objs <- if (!nrow(t)) character() else vapply(seq_len(nrow(t)),
      function(i) json_object(list(
        file = json_str(t$file[i]),
        line = json_int(t$line[i]),
        desc = json_str_or_null(t$desc[i]),
        harness = json_str(t$harness[i]),
        link = json_str_or_null(t$link[i]),
        filled = json_bool(t$filled[i]),
        md5 = json_str_or_null(t$md5[i]),
        result = json_str(t$result[i])
      )), character(1))

    json_object(list(
      name = json_str(e$name),
      kind = json_str(e$kind),
      intended_use_declared = json_bool(e$intended_use_declared),
      risk_classification = json_str_or_null(e$risk_classification),
      surface_hash = json_str(e$surface_hash),
      status = json_str(e$status),
      tests = json_array(test_objs),
      unexercised = json_object(list(
        observables = json_str_array(e$unexercised$observables),
        stimuli = json_str_array(e$unexercised$stimuli)
      )),
      warnings = json_str_array(sort(e$warnings))
    ))
  }, character(1), USE.NAMES = FALSE)

  tests <- coverage$tests
  orphan_objs <- vapply(coverage$orphans, function(i) json_object(list(
    file = json_str(tests$file[i]),
    line = json_int(tests$line[i]),
    desc = json_str_or_null(tests$desc[i]),
    warnings = json_str_array("SVT-W304")
  )), character(1), USE.NAMES = FALSE)

  unknown_objs <- vapply(coverage$unknown, function(i) json_object(list(
    file = json_str(tests$file[i]),
    line = json_int(tests$line[i]),
    targets = json_str_array(tests$unknown_targets[[i]]),
    warnings = json_str_array("SVT-W305")
  )), character(1), USE.NAMES = FALSE)

  s <- coverage$summary
  json_object(list(
    schema_version = json_str("1.0"),
    inputs = json_object(list(
      test_path = json_str_or_null(coverage$inputs$test_path_rel),
      results = json_str_or_null(coverage$inputs$results)
    )),
    entries = json_array(entry_objs),
    orphan_tests = json_array(orphan_objs),
    unknown_targets = json_array(unknown_objs),
    summary = json_object(list(
      covered = json_int(s$covered),
      partial = json_int(s$partial),
      scaffold = json_int(s$scaffold),
      uncovered = json_int(s$uncovered),
      waived = json_int(s$waived),
      orphan_tests = json_int(s$orphan_tests)
    ))
  ))
}

#' Render `traceability.md` — the auditor-facing matrix.
#'
#' @noRd
render_traceability_md <- function(features, coverage) {
  out <- c("# Verification traceability", "", coverage_disclaimer(), "")

  out <- c(out, "## Summary", render_coverage_summary(features, coverage), "")
  out <- c(out, "## Matrix", render_coverage_matrix(coverage), "")
  out <- c(out, "## Gaps", render_coverage_gaps(coverage), "")
  out <- c(out, "## Orphan tests", render_orphan_tests(coverage), "")
  out <- c(out, "## Unknown targets", render_unknown_targets(coverage), "")
  out <- c(out, "## Reviewers",
           "- Developer: __________________ Date: __________",
           "- Validation Engineer: __________________ Date: __________", "")

  list(text = paste(out, collapse = "\n"),
       auto_keys = c("Summary", "Matrix", "Gaps", "Orphan tests",
                     "Unknown targets"))
}

#' The status counts plus the two completeness gates.
#'
#' SVT-W103 and SVT-W301 are duals — behaviour no feature claims, and a
#' claimed feature no test exercises. Neither number means much alone,
#' and the pair is the headline an auditor is actually asking for.
#'
#' @noRd
render_coverage_summary <- function(features, coverage) {
  s <- coverage$summary
  n_unclaimed <- if (is.null(features$unclaimed)) 0L else
    nrow(features$unclaimed)
  n_uncovered <- s$uncovered

  c(md_table(c("Status", "Count"), c(
      paste0("| covered | ", s$covered, " |"),
      paste0("| partial | ", s$partial, " |"),
      paste0("| scaffold | ", s$scaffold, " |"),
      paste0("| uncovered | ", s$uncovered, " |"),
      paste0("| waived | ", s$waived, " |"),
      paste0("| orphan tests | ", s$orphan_tests, " |")
    )),
    "",
    "Completeness gates:",
    "",
    paste0("- SVT-W103 - behaviour the user can reach that no feature ",
           "claims: ", n_unclaimed),
    paste0("- SVT-W301 - a claimed feature that no test exercises: ",
           n_uncovered))
}

#' @noRd
render_coverage_matrix <- function(coverage) {
  if (!length(coverage$entries)) return("(no features or modules)")
  rows <- vapply(coverage$entries, function(e) {
    slug <- slugify_artifact_name(e$name)
    tests <- if (!nrow(e$tests)) "(none)" else
      paste(paste0(e$tests$file, ":", ifelse(is.na(e$tests$line), "",
                                             e$tests$line)),
            collapse = "<br>")
    results <- if (!nrow(e$tests)) "-" else
      paste(unique(e$tests$result), collapse = ", ")
    paste0("| ", e$name,
           " | ", if (e$intended_use_declared) "yes" else "no",
           " | ", declared_or_gap(e$risk_classification),
           " | ", e$status,
           " | ", tests,
           " | ", results,
           " | [", slug, ".md](", slug, ".md) |")
  }, character(1), USE.NAMES = FALSE)
  md_table(c("Feature", "Intended use", "Risk", "Status", "Tests", "Result",
             "Doc"), rows)
}

#' The work list: one row per uncovered or partial entry, worst risk first.
#'
#' @noRd
render_coverage_gaps <- function(coverage) {
  gaps <- Filter(function(e) e$status %in% c("uncovered", "partial",
                                             "scaffold"),
                 coverage$entries)
  if (!length(gaps)) return("(none)")

  rank <- vapply(gaps, function(e) {
    r <- match(e$risk_classification %||% NA_character_,
               c("high", "medium", "low"))
    if (is.na(r)) 4L else as.integer(r)
  }, integer(1))
  gaps <- gaps[order(rank, vapply(gaps, function(e) e$name, character(1)))]

  rows <- vapply(gaps, function(e) {
    missing <- c(
      if (length(e$unexercised$observables))
        paste0("observables: ", paste(e$unexercised$observables,
                                      collapse = ", ")),
      if (length(e$unexercised$stimuli))
        paste0("stimuli: ", paste(e$unexercised$stimuli, collapse = ", "))
    )
    paste0("| ", e$name,
           " | ", declared_or_gap(e$risk_classification),
           " | ", e$status,
           " | ", if (length(missing)) paste(missing, collapse = "; ") else
             "(nothing derivable)",
           " |")
  }, character(1), USE.NAMES = FALSE)
  md_table(c("Feature", "Risk", "Status", "Not exercised"), rows)
}

#' The reverse view: tests that map to nothing.
#' @noRd
render_orphan_tests <- function(coverage) {
  if (!length(coverage$orphans)) return("(none)")
  t <- coverage$tests
  rows <- vapply(coverage$orphans, function(i) {
    paste0("| ", t$file[i], ":", t$line[i],
           " | ", declared_or_gap(t$desc[i]),
           " | SVT-W304 |")
  }, character(1), USE.NAMES = FALSE)
  c(md_table(c("Test", "Description", "Warning"), rows),
    "",
    paste0("An orphan maps to no feature, module, or helper. The remedy is ",
           "a `# @covers` annotation in the test, or a manifest `tests:` ",
           "entry."))
}

#' Annotations that name something the graph does not contain.
#'
#' A renamed feature leaves its `@covers` behind, and the test then
#' silently covers nothing. SVT-W305 is what makes that visible.
#'
#' @noRd
render_unknown_targets <- function(coverage) {
  if (!length(coverage$unknown)) return("(none)")
  t <- coverage$tests
  rows <- vapply(coverage$unknown, function(i) {
    paste0("| ", t$file[i], ":", t$line[i],
           " | ", paste(t$unknown_targets[[i]], collapse = ", "),
           " | SVT-W305 |")
  }, character(1), USE.NAMES = FALSE)
  c(md_table(c("Test", "Annotation", "Warning"), rows),
    "",
    paste0("An annotation naming a feature, module, or helper that is not ",
           "in the graph - usually a rename the test did not follow."))
}

#' Render the `## Test coverage` doc-stub section.
#' @noRd
render_test_coverage_section <- function(entry) {
  if (is.null(entry)) return("(not assessed)")
  tests <- if (!nrow(entry$tests)) "(none)" else
    paste(vapply(seq_len(nrow(entry$tests)), function(i) {
      paste0(entry$tests$file[i],
             if (is.na(entry$tests$line[i])) "" else
               paste0(":", entry$tests$line[i]),
             " (", entry$tests$harness[i], ", ", entry$tests$link[i],
             ", ", entry$tests$result[i], ")")
    }, character(1)), collapse = "; ")

  missing <- c(
    if (length(entry$unexercised$observables))
      paste0("observable ", paste(entry$unexercised$observables,
                                  collapse = ", ")),
    if (length(entry$unexercised$stimuli))
      paste0("stimulus ", paste(entry$unexercised$stimuli, collapse = ", "))
  )

  out <- c(paste0("Status: ", entry$status, ". Tests: ", tests, "."))
  if (length(missing)) {
    out <- c(out, paste0("Not exercised: ", paste(missing, collapse = ", "),
                         "."))
  }
  if (length(entry$warnings)) {
    out <- c(out, "",
             vapply(sort(entry$warnings),
                    function(c) paste0("- ", c, " - ", warning_message(c)),
                    character(1)))
  }
  out
}

#' Render the index's `## Verification coverage` section.
#' @noRd
render_verification_coverage_section <- function(features, coverage) {
  c(render_coverage_summary(features, coverage),
    "",
    "[Traceability matrix](traceability.md)")
}

# Staleness --------------------------------------------------------------

#' Read the prior `traceability.json`, if any.
#'
#' YAML is a JSON superset, so the reader the artifact manifest already
#' uses parses this too — no JSON dependency is added for a file we
#' ourselves wrote.
#'
#' @noRd
read_prior_traceability <- function(out_dir) {
  p <- file.path(out_dir, "traceability.json")
  if (!file.exists(p)) return(NULL)
  raw <- tryCatch(paste(readLines(p, warn = FALSE), collapse = "\n"),
                  error = function(e) NULL)
  if (is.null(raw) || !nzchar(raw)) return(NULL)
  parsed <- tryCatch(yaml::yaml.load(raw), error = function(e) NULL)
  if (is.null(parsed) || is.null(parsed$entries)) return(NULL)
  parsed
}

#' Flag surfaces whose test surface moved while their tests did not.
#'
#' The verification analogue of the "inventory changed since last
#' regeneration" marker. Deliberately conservative: an md5 change is not
#' evidence the test was updated for the right reason, only that somebody
#' touched it — so the warning clears on any edit, and says so.
#'
#' @noRd
detect_stale_coverage <- function(coverage, prior) {
  if (is.null(prior)) return(coverage)
  by_name <- list()
  for (e in prior$entries) {
    if (!is.null(e$name)) by_name[[e$name]] <- e
  }

  for (nm in names(coverage$entries)) {
    entry <- coverage$entries[[nm]]
    old <- by_name[[nm]]
    if (is.null(old) || !nrow(entry$tests)) next
    if (identical(old$surface_hash, entry$surface_hash)) next

    old_md5 <- list()
    for (t in old$tests %||% list()) {
      if (!is.null(t$file) && !is.null(t$md5)) old_md5[[t$file]] <- t$md5
    }
    unchanged <- vapply(seq_len(nrow(entry$tests)), function(i) {
      f <- entry$tests$file[i]
      recorded <- old_md5[[f]]
      !is.null(recorded) && identical(recorded, entry$tests$md5[i])
    }, logical(1))
    if (length(unchanged) && all(unchanged)) {
      coverage$entries[[nm]]$warnings <- unique(c(entry$warnings, "SVT-W307"))
    }
  }
  coverage
}

# Writers ----------------------------------------------------------------

#' Write `traceability.json` and `traceability.md` to `out_dir`.
#'
#' Returns the two paths.
#'
#' @noRd
write_traceability <- function(features, coverage, out_dir) {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  json_path <- file.path(out_dir, "traceability.json")
  writeLines(render_traceability_json(coverage), json_path)

  rendered <- render_traceability_md(features, coverage)
  md_path <- file.path(out_dir, "traceability.md")
  existing <- if (file.exists(md_path)) {
    paste(readLines(md_path, warn = FALSE), collapse = "\n")
  } else {
    NULL
  }
  writeLines(merge_doc_stub(rendered, existing), md_path)

  c(json_path, md_path)
}
