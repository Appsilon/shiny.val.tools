#' Read a `features.yml` manifest from disk.
#'
#' Returns a list with two components: `features` (list of feature
#' records) and `modules` (list of module records). A missing file
#' yields an empty manifest (`features = list()`, `modules = list()`)
#' so callers can run with no manifest as the no-op case.
#'
#' YAML parsing is intentionally lenient at this layer — structural
#' validation against the graph is the job of `validate_manifest()`.
#' Only the most basic shape errors (e.g. top-level value is not a
#' mapping) are surfaced here.
#'
#' @noRd
read_manifest <- function(path) {
  if (is.null(path)) return(empty_manifest())
  if (!file.exists(path)) return(empty_manifest())
  raw <- yaml::read_yaml(path)
  if (is.null(raw)) return(empty_manifest())
  if (!is.list(raw)) {
    stop("Manifest at ", path, " is not a YAML mapping", call. = FALSE)
  }
  features <- raw$features %||% list()
  modules <- raw$modules %||% list()
  list(features = as.list(features), modules = as.list(modules))
}

#' Empty manifest — the canonical shape for the no-op case.
#' @noRd
empty_manifest <- function() list(features = list(), modules = list())

#' Validate a manifest against a graph and return a tibble of issues.
#'
#' Issue rows have columns `code`, `name`, `message`. An empty tibble
#' signals "ready to use". `code` is one of the SVT-W101..W105 strings;
#' `name` is the offending feature/module name (NA when not applicable);
#' `message` mirrors `warning_message(code)` plus context.
#'
#' Implemented checks:
#'   - SVT-W101 — Manifest references unknown root.
#'   - SVT-W102 — Two features claim the same root.
#'   - SVT-W105 — `risk_classification = not_required` without `rationale`.
#'   - **fatal** — name collision across features+modules. Emitted as a
#'                 raw issue row with `code = "manifest_name_collision"`;
#'                 a name collision is fatal regardless of `lenient`.
#'
#' SVT-W103 (unclaimed output) lives in `unclaimed_outputs()` since it
#' is a graph-level completeness check, not a manifest validity check.
#' SVT-W104 (orphan module instance) is a graph-build concern, surfaced
#' separately by `detect_orphan_module_instances()`.
#'
#' @noRd
validate_manifest <- function(manifest, graph) {
  issues <- list()

  emit <- function(code, name, message) {
    issues[[length(issues) + 1L]] <<- tibble::tibble(
      code = code,
      name = name %||% NA_character_,
      message = message
    )
  }

  features <- manifest$features %||% list()
  modules  <- manifest$modules  %||% list()

  feat_names <- vapply(features,
                       function(x) x$name %||% NA_character_, character(1))
  mod_names  <- vapply(modules,
                       function(x) x$name %||% NA_character_, character(1))

  # Name uniqueness — every name is unique across features and modules;
  # a collision is fatal regardless of `lenient`.
  all_names <- c(feat_names, mod_names)
  all_names <- all_names[!is.na(all_names)]
  dups <- all_names[duplicated(all_names)]
  for (d in unique(dups)) {
    emit("manifest_name_collision", d,
         paste0("Name '", d, "' used by multiple features/modules"))
  }

  # SVT-W101 / W102 — root resolution.
  root_claims <- list()  # node_id -> feature_name (first claimant)

  resolve_root <- function(root_entry, owner_name) {
    # Each root entry is `{output: <name>}` or `{observer: <name>}`.
    # For module-namespaced roots: `{output: <namespace>/<name>}`.
    if (!is.list(root_entry) || !length(root_entry)) return(NULL)
    kind <- names(root_entry)[1L]
    raw_name <- root_entry[[1L]]
    if (!is.character(raw_name) || length(raw_name) != 1L || is.na(raw_name)) {
      return(NULL)
    }

    if (grepl("/", raw_name, fixed = TRUE)) {
      parts <- strsplit(raw_name, "/", fixed = TRUE)[[1L]]
      ns <- parts[1L]
      bare <- paste(parts[-1L], collapse = "/")
    } else {
      ns <- NA_character_
      bare <- raw_name
    }

    type_match <- if (kind == "output") "output"
                  else if (kind == "observer") "observer"
                  else NA_character_
    if (is.na(type_match)) return(NULL)

    nodes <- graph$nodes
    ns_node <- ifelse(is.na(nodes$namespace), NA_character_, nodes$namespace)
    candidates <- which(nodes$type == type_match & nodes$name == bare &
                          ((is.na(ns) & is.na(ns_node)) |
                             (!is.na(ns) & !is.na(ns_node) & ns_node == ns)))
    if (!length(candidates)) {
      emit("SVT-W101", owner_name,
           paste0("Manifest root ", kind, ": ", raw_name, " not found in graph"))
      return(NULL)
    }
    nodes$id[candidates[1L]]
  }

  for (i in seq_along(features)) {
    f <- features[[i]]
    nm <- f$name %||% NA_character_

    # SVT-W105 — not_required without rationale.
    rc <- f$risk_classification %||% NA_character_
    rationale <- f$rationale
    if (!is.na(rc) && identical(rc, "not_required")) {
      if (is.null(rationale) ||
          (is.character(rationale) && (!nzchar(rationale) || is.na(rationale)))) {
        emit("SVT-W105", nm,
             "risk_classification = not_required requires a rationale")
      }
    }

    roots <- f$roots %||% list()
    for (r in roots) {
      rid <- resolve_root(r, nm)
      if (is.null(rid)) next
      prev <- root_claims[[rid]]
      if (!is.null(prev)) {
        emit("SVT-W102", nm,
             paste0("Root claimed by both '", prev, "' and '", nm, "'"))
      } else {
        root_claims[[rid]] <- nm
      }
    }
  }

  if (!length(issues)) {
    return(tibble::tibble(code = character(),
                          name = character(),
                          message = character()))
  }
  do.call(rbind, issues)
}

#' Apply a manifest to default-sliced features.
#'
#' A feature in the manifest has a `name`, an `intended_use`, a
#' `risk_classification`, optional `rationale`, a `roots` list, and
#' optional `package_categories` and `reviewers`. The slicer uses the
#' `roots` to compute the upstream closure; the other fields decorate
#' the feature record.
#'
#' Manifest-declared features replace default-sliced features whose
#' name matches. Default-sliced features whose root is **claimed** by a
#' manifest feature are dropped (they're absorbed). Default-sliced
#' features whose root is unclaimed remain — so default behavior holds
#' for outputs not mentioned in the manifest. The unclaimed-output
#' check (SVT-W103) is run on the resulting feature set elsewhere.
#'
#' Returns a list of feature records (kind = "feature") with manifest
#' metadata merged in.
#'
#' @noRd
apply_manifest <- function(manifest, graph) {
  default <- default_slice(graph)
  features <- manifest$features %||% list()
  if (!length(features)) return(default)

  default_by_root <- character()
  for (i in seq_along(default)) {
    default_by_root[default[[i]]$roots[1L]] <- default[[i]]$name
  }

  manifest_records <- list()
  claimed_roots <- character()

  for (f in features) {
    nm <- f$name %||% NA_character_
    if (is.na(nm)) next

    root_ids <- character()
    for (r in f$roots %||% list()) {
      rid <- resolve_manifest_root(r, graph$nodes)
      if (!is.null(rid)) root_ids <- unique(c(root_ids, rid))
    }
    if (!length(root_ids)) next  # validation already flagged it; drop.

    closure <- character()
    for (rid in root_ids) {
      closure <- unique(c(closure,
                          upstream_closure(rid, graph$nodes, graph$edges)))
    }
    edge_idx <- if (nrow(graph$edges)) {
      which(graph$edges$source_id %in% closure &
              graph$edges$target_id %in% closure)
    } else {
      integer()
    }

    pkg_cats <- f$package_categories %||% list()
    reviewers <- f$reviewers %||% list()
    intended_use <- f$intended_use %||% NA_character_
    risk <- f$risk_classification %||% NA_character_
    rationale <- f$rationale %||% NA_character_

    rec <- new_feature_record(
      name = nm,
      kind = "feature",
      roots = root_ids,
      node_ids = closure,
      edge_ids = edge_idx,
      intended_use = if (is.character(intended_use)) intended_use else NA_character_,
      risk_classification = if (is.character(risk)) risk else NA_character_,
      rationale = if (is.character(rationale)) rationale else NA_character_,
      package_categories = as.list(pkg_cats),
      reviewers = as.list(reviewers)
    )
    manifest_records[[length(manifest_records) + 1L]] <- rec
    claimed_roots <- unique(c(claimed_roots, root_ids))
  }

  # Drop default features whose root has been claimed by a manifest entry.
  default_filtered <- Filter(function(d) {
    !any(d$roots %in% claimed_roots)
  }, default)

  c(manifest_records, default_filtered)
}

#' Resolve a manifest root entry to a node id (or NULL).
#'
#' Mirrors the resolver in `validate_manifest()` but does not emit
#' issues. Used at apply time to build the feature subgraph.
#'
#' @noRd
resolve_manifest_root <- function(root_entry, nodes) {
  if (!is.list(root_entry) || !length(root_entry)) return(NULL)
  kind <- names(root_entry)[1L]
  raw_name <- root_entry[[1L]]
  if (!is.character(raw_name) || length(raw_name) != 1L || is.na(raw_name)) {
    return(NULL)
  }

  if (grepl("/", raw_name, fixed = TRUE)) {
    parts <- strsplit(raw_name, "/", fixed = TRUE)[[1L]]
    ns <- parts[1L]
    bare <- paste(parts[-1L], collapse = "/")
  } else {
    ns <- NA_character_
    bare <- raw_name
  }

  type_match <- if (kind == "output") "output"
                else if (kind == "observer") "observer"
                else NA_character_
  if (is.na(type_match)) return(NULL)

  ns_node <- ifelse(is.na(nodes$namespace), NA_character_, nodes$namespace)
  hit <- which(nodes$type == type_match & nodes$name == bare &
                 ((is.na(ns) & is.na(ns_node)) |
                    (!is.na(ns) & !is.na(ns_node) & ns_node == ns)))
  if (!length(hit)) return(NULL)
  nodes$id[hit[1L]]
}

#' Detect SVT-W103 — unclaimed top-level outputs / observers.
#'
#' After slicing, every top-level output and observer is checked; any
#' that is not the root of a manifest-declared feature emits SVT-W103.
#' SVT-W103 fires only when an explicit manifest is provided — the
#' default slicing rule trivially claims every root, so the warning is
#' meaningless without a user-authored manifest. Callers can pass
#' `manifest_supplied = FALSE` to short-circuit; the detector returns
#' an empty tibble.
#'
#' @noRd
detect_unclaimed_outputs <- function(graph, manifest, manifest_supplied) {
  if (!isTRUE(manifest_supplied)) return(empty_warnings())

  nodes <- graph$nodes
  is_top <- is.na(nodes$namespace) | !nzchar(nodes$namespace)
  is_root_kind <- nodes$type %in% c("output", "observer")
  candidates <- nodes[is_top & is_root_kind, , drop = FALSE]
  if (!nrow(candidates)) return(empty_warnings())

  claimed <- character()
  for (f in manifest$features %||% list()) {
    for (r in f$roots %||% list()) {
      rid <- resolve_manifest_root(r, nodes)
      if (!is.null(rid)) claimed <- c(claimed, rid)
    }
  }

  unclaimed <- candidates[!candidates$id %in% claimed, , drop = FALSE]
  if (!nrow(unclaimed)) return(empty_warnings())

  tibble::tibble(
    code = rep("SVT-W103", nrow(unclaimed)),
    file = unclaimed$file,
    line = as.integer(unclaimed$line),
    col = as.integer(unclaimed$col),
    message = paste0(warning_message("SVT-W103"), ": ",
                     unclaimed$type, ":", unclaimed$fq_name)
  )
}

#' Detect SVT-W104 — orphan module-instance nodes.
#'
#' A `module_instance` node refers to a wrapper function. If no
#' `module_server` definition with the same wrapper name exists in the
#' enumerated source, the instantiation is orphan — likely a missing
#' `source()` or a misnamed call.
#'
#' Note: `build_module_instance_nodes()` only emits a `module_instance`
#' node when the wrapper name is in the wrapper-set derived from the
#' Definitions table — orphans are therefore impossible by construction
#' today. The detector exists for completeness; if the resolution rules
#' ever broaden to import-aware wrappers, this becomes a real check.
#'
#' @noRd
detect_orphan_module_instances <- function(graph, app_path) {
  nodes <- graph$nodes
  if (!nrow(nodes)) return(empty_warnings())
  inst <- nodes[nodes$type == "module_instance", , drop = FALSE]
  if (!nrow(inst)) return(empty_warnings())

  defs <- build_definitions_table(app_path)
  wrapper_names <- if (nrow(defs)) {
    unique(defs$name[defs$kind == "module_server" & !is.na(defs$name)])
  } else {
    character()
  }

  orphan <- inst[!inst$name %in% wrapper_names, , drop = FALSE]
  if (!nrow(orphan)) return(empty_warnings())

  tibble::tibble(
    code = rep("SVT-W104", nrow(orphan)),
    file = orphan$file,
    line = as.integer(orphan$line),
    col = as.integer(orphan$col),
    message = paste0(warning_message("SVT-W104"), ": ", orphan$name)
  )
}

#' Generate a starter `features.yml` from a graph.
#'
#' Produces one entry per default-sliced feature root. Empty fields
#' (`intended_use`, `risk_classification`, etc.) are written as `~`
#' placeholders so the developer fills them in.
#'
#' @noRd
manifest_template <- function(graph) {
  features <- default_slice(graph)
  if (!length(features)) {
    return("features: []\nmodules: []\n")
  }

  feat_blocks <- vapply(features, function(f) {
    nodes <- graph$nodes
    root_node <- nodes[nodes$id == f$roots[1L], , drop = FALSE]
    kind <- if (nrow(root_node)) root_node$type[1L] else "output"
    paste0(
      "  - name: ", f$name, "\n",
      "    intended_use: ~\n",
      "    risk_classification: ~\n",
      "    rationale: ~\n",
      "    roots:\n",
      "      - ", kind, ": ", f$name, "\n"
    )
  }, character(1))

  paste0("features:\n", paste(feat_blocks, collapse = ""), "modules: []\n")
}
