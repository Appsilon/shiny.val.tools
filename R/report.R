#' Render the validation summary report — the artifact a sponsor signs.
#'
#' Per spec 07. `index.md` is the navigation hub; this is the signable
#' document. Sections 1-8 are auto-filled and refreshed on every run;
#' Conclusion and Approval are human-authored and preserved by
#' `merge_doc_stub()`.
#'
#' Carries no generation timestamp: the determinism contract requires the
#' same source + manifest + package version to yield byte-identical
#' artifacts, and a clock reading would break that on every run.
#'
#' `report_meta` is the manifest's optional `report:` block. Every field is
#' optional; absent fields render as "(not declared)" so the gap is visible
#' on the signed page rather than silently missing.
#'
#' @noRd
render_validation_report <- function(features, inventory, graph, app_path,
                                     report_meta = NULL) {
  meta <- as.list(report_meta %||% list())
  records <- features$records %||% list()

  title <- meta$title %||% "Validation Summary Report"
  system_name <- meta$system_name %||% target_basename(app_path)

  out <- c(paste0("# ", title, ": ", system_name), "")

  out <- c(out, "## Document control",
           report_control_table(meta, system_name), "")
  out <- c(out, "## System identification",
           report_identification(app_path, system_name, meta), "")
  out <- c(out, "## Scope of analysis",
           report_scope_table(records, graph, app_path), "")
  out <- c(out, "## Method and limitations", report_limitations(), "")
  out <- c(out, "## Features and risk classification",
           report_risk_table(records), "")
  out <- c(out, "## Verification status", report_verification(), "")
  out <- c(out, "## Analysis findings",
           report_findings_table(features, inventory, graph), "")

  out <- c(out, "## Conclusion",
           paste0("(not authored - the analysis produces evidence; the ",
                  "validation conclusion is written and owned by a human ",
                  "reviewer)"),
           "")

  out <- c(out, "## Approval", report_approval_table(meta), "")

  list(
    text = paste(out, collapse = "\n"),
    auto_keys = c("Document control", "System identification",
                  "Scope of analysis", "Method and limitations",
                  "Features and risk classification", "Verification status",
                  "Analysis findings")
  )
}

#' @noRd
target_basename <- function(app_path) {
  if (is.null(app_path) || !nzchar(app_path)) return("Target")
  basename(normalizePath(app_path, winslash = "/", mustWork = FALSE))
}

#' @noRd
declared_or_gap <- function(x) {
  if (is.null(x) || !length(x) || is.na(x[1L]) || !nzchar(as.character(x[1L]))) {
    return("(not declared)")
  }
  as.character(x[1L])
}

#' @noRd
md_table <- function(header, rows) {
  c(paste0("| ", paste(header, collapse = " | "), " |"),
    paste0("|", paste(rep(" --- ", length(header)), collapse = "|"), "|"),
    rows)
}

#' @noRd
report_control_table <- function(meta, system_name) {
  # `system_name` is already resolved (declared value, else target basename),
  # so the two tables never disagree on the identity of the system.
  meta$system_name <- system_name
  fields <- c(document_id = "Document ID", sponsor = "Sponsor",
              system_name = "System name", system_version = "System version",
              sop_reference = "SOP reference")
  rows <- vapply(names(fields), function(key) {
    paste0("| ", fields[[key]], " | ", declared_or_gap(meta[[key]]), " |")
  }, character(1), USE.NAMES = FALSE)

  rows <- c(rows, paste0("| Analysis tool | shiny.val.tools ",
                         svt_version(), " |"))
  md_table(c("Field", "Value"), rows)
}

#' @noRd
report_identification <- function(app_path, system_name, meta) {
  target_type <- if (!is.null(app_path) && nzchar(app_path) &&
                     is_package_target(app_path)) {
    "R package (Shiny module library)"
  } else {
    "Shiny application"
  }
  rev <- git_commit_short(app_path)
  rev_text <- if (is.na(rev)) "(target is not a git repository)" else rev

  md_table(c("Field", "Value"), c(
    paste0("| System | ", system_name, " |"),
    paste0("| Version | ", declared_or_gap(meta$system_version), " |"),
    paste0("| Target type | ", target_type, " |"),
    paste0("| Source revision | ", rev_text, " |"),
    paste0("| Analysis tool | shiny.val.tools ", svt_version(), " |")
  ))
}

#' @noRd
report_scope_table <- function(records, graph, app_path) {
  feats <- Filter(function(r) identical(r$kind, "feature"), records)
  mods <- Filter(function(r) identical(r$kind, "module"), records)
  files <- tryCatch(length(enumerate_app_files(app_path)),
                    error = function(e) NA_integer_)

  md_table(c("Scope", "Count"), c(
    paste0("| Source files analysed | ", files, " |"),
    paste0("| Reactive graph nodes | ", nrow(graph$nodes), " |"),
    paste0("| Reactive graph edges | ", nrow(graph$edges), " |"),
    paste0("| Features | ", length(feats), " |"),
    paste0("| Modules | ", length(mods), " |")
  ))
}

#' @noRd
report_limitations <- function() {
  c("The evidence in this packet is produced by static analysis of R source.",
    "The application is never executed and its tests are never run.",
    "The following are declared limitations of the method, not defects:",
    "",
    "- Dynamic IDs - identifiers constructed at runtime are not resolved.",
    "- `renderUI` / `insertUI` inputs - inputs created during UI rendering.",
    "- Named access into `reactiveValues()` - best-effort by name.",
    "- Metaprogramming - `do.call`, `eval(parse(...))`, dynamic dispatch.",
    "- Cross-package re-exports - origin may resolve to the re-exporter.",
    "- Conditional `source()` / `box::use()` - best-effort.",
    "- Analysis stops at the R boundary: no JavaScript, `www/` assets, or",
    "  htmlwidgets internals are traced.")
}

#' @noRd
report_risk_table <- function(records) {
  if (!length(records)) return("(no features or modules were sliced)")

  rows <- vapply(records, function(r) {
    paste0("| ", r$name,
           " | ", r$kind %||% "feature",
           " | ", declared_or_gap(r$risk_classification),
           " | ", if (is.null(r$intended_use) ||
                      !nzchar(paste(r$intended_use, collapse = ""))) {
                    "no"
                  } else {
                    "yes"
                  },
           " | ", slugify_artifact_name(r$name), ".md |")
  }, character(1), USE.NAMES = FALSE)

  md_table(c("Name", "Kind", "Risk classification", "Intended use declared",
             "Detail"), rows)
}

#' The Verification status section.
#'
#' Stated, never omitted: an absent section reads as "nothing to report",
#' which would be misleading in a signed document. The tool derives each
#' subgraph's test surface (spec 06 phases 1-2) but does not discover,
#' map, or run tests, so it makes no coverage claim.
#'
#' @noRd
report_verification <- function() {
  c("**Test coverage was not assessed by this tool.**",
    "",
    "This analysis derives each feature's and module's *test surface* - the",
    "inputs a test must set, the observable results, the intermediate",
    "reactives, the trusted terminals and the app-defined helpers - and",
    "emits it per subgraph as `test_surface.json` and a `## Test surface`",
    "section in the corresponding detail document. Harness scaffolds are",
    "written on request.",
    "",
    "The tool does **not** discover the application's existing tests, map",
    "them to features, or execute anything. No statement about test",
    "coverage or verification status is made by this report; any such claim",
    "must be established and evidenced separately.")
}

#' The Analysis findings section — every warning code the run raised.
#'
#' Shares `aggregate_warning_counts()` with the index, deliberately. The
#' section claims completeness, so it has to include the graph-level
#' Warnings table (SVT-W001..W010, W104) and not just the inventory codes
#' and the node list-column — and an auditor reading the index and the
#' report side by side must not be shown two different numbers for the
#' same run.
#'
#' Ordered most-frequent-first, ties broken by code, so the ordering is
#' deterministic without a clock or a hash-map iteration order.
#'
#' @noRd
report_findings_table <- function(features, inventory, graph) {
  agg <- aggregate_warning_counts(features, inventory, graph)
  if (!nrow(agg)) {
    return("No analysis warnings were raised.")
  }

  agg <- agg[order(-agg$count, agg$code), , drop = FALSE]
  rows <- vapply(seq_len(nrow(agg)), function(i) {
    paste0("| ", agg$code[i], " | ", as.integer(agg$count[i]),
           " | ", warning_message(agg$code[i]), " |")
  }, character(1), USE.NAMES = FALSE)

  c("Every code raised during analysis is reported here. Counts are",
    "occurrences, not distinct defects.",
    "",
    md_table(c("Code", "Occurrences", "Meaning"), rows))
}

#' @noRd
report_approval_table <- function(meta) {
  approvers <- meta$approvers %||% list()
  rows <- if (length(approvers)) {
    vapply(approvers, function(a) {
      a <- as.list(a)
      paste0("| ", declared_or_gap(a$role), " | ", declared_or_gap(a$name),
             " |  |  |")
    }, character(1), USE.NAMES = FALSE)
  } else {
    c("|  |  |  |  |", "|  |  |  |  |")
  }

  c(md_table(c("Role", "Name", "Signature", "Date"), rows),
    "",
    paste0("Signing this report attests to review of the evidence it ",
           "references. It does not attest to any statement generated by ",
           "the analysis tool."))
}

#' Write the validation summary report, preserving human-authored sections.
#'
#' @noRd
write_validation_report <- function(features, inventory, graph, out_dir,
                                    app_path, report_meta = NULL) {
  rendered <- render_validation_report(features, inventory, graph, app_path,
                                       report_meta = report_meta)
  path <- file.path(out_dir, "validation-report.md")
  existing <- if (file.exists(path)) {
    paste(readLines(path, warn = FALSE), collapse = "\n")
  } else {
    NULL
  }
  writeLines(merge_doc_stub(rendered, existing), path)
  path
}
