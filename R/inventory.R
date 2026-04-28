#' Resolve every call-site reference to a function origin.
#'
#' Implements the function-origin resolution priority chain:
#'   1. Explicit `pkg::fn` / `pkg:::fn` -> resolution = "explicit".
#'      `:::` access additionally records SVT-W201.
#'   2. `box::use(pkg)` / `box::use(alias = pkg)` aliased call
#'      (`alias$fn(...)`) -> resolution = "box_declared".
#'   3. `box::use(pkg[fn1, fn2])` function-set match for a bare-name call
#'      -> resolution = "box_declared".
#'   4. `library(pkg)` / `require(pkg)` walk: a bare-name call resolves
#'      against the union of namespace exports of the file's loaded
#'      packages. Exactly one match -> "library_walk"; multiple ->
#'      "ambiguous" + SVT-W202; zero -> falls through.
#'   5. Anything else -> "unresolved" + SVT-W203.
#'
#' Returns a tibble with one row per call-site reference, with columns:
#'   `from_file`, `name`, `package`, `fq_name`, `resolution`, `internal`,
#'   `container` (the alias, when applicable),
#'   `in_def_kind`, `in_def_name`, `in_def_namespace`,
#'   `line`, `col`, `warnings` (list-column of SVT-W2** codes).
#'
#' Resolution data is computed once per graph; per-feature inventories
#' filter from this table downstream.
#'
#' @noRd
resolve_function_origins <- function(refs, imports) {
  empty <- tibble::tibble(
    from_file = character(), name = character(), package = character(),
    fq_name = character(), resolution = character(), internal = logical(),
    container = character(),
    in_def_kind = character(), in_def_name = character(),
    in_def_namespace = character(),
    line = integer(), col = integer(),
    warnings = list()
  )
  if (!nrow(refs)) return(empty)

  calls <- refs[refs$kind == "call", , drop = FALSE]
  if (!nrow(calls)) return(empty)

  per_file <- per_file_import_index(imports)
  exports_cache <- new.env(parent = emptyenv())

  packages <- character(nrow(calls))
  resolutions <- character(nrow(calls))
  warnings_v <- vector("list", nrow(calls))

  for (i in seq_len(nrow(calls))) {
    file <- calls$from_file[i]
    name <- calls$name[i]
    pkg <- calls$package[i]
    container <- calls$container[i]
    internal <- isTRUE(calls$internal[i])
    file_ix <- per_file[[file]] %||% empty_file_imports()
    warns <- character()

    # 1. Explicit pkg::fn / pkg:::fn.
    if (!is.na(pkg)) {
      packages[i] <- pkg
      resolutions[i] <- "explicit"
      if (internal) warns <- c(warns, "SVT-W201")
      warnings_v[[i]] <- warns
      next
    }

    # 2. alias$fn matching a box::use(alias = pkg) or box::use(pkg) clause.
    if (!is.na(container) && nzchar(container)) {
      hit <- file_ix$alias_to_pkg[[container]]
      if (!is.null(hit)) {
        packages[i] <- hit
        resolutions[i] <- "box_declared"
        warnings_v[[i]] <- warns
        next
      }
      packages[i] <- NA_character_
      resolutions[i] <- "unresolved"
      warnings_v[[i]] <- c(warns, "SVT-W203")
      next
    }

    # 3. Bare-name match against any box::use(pkg[fn1, fn2]) function set.
    fn_hits <- file_ix$fn_to_pkg[[name]]
    if (length(fn_hits)) {
      fn_hits <- sort(unique(fn_hits))
      if (length(fn_hits) == 1L) {
        packages[i] <- fn_hits
        resolutions[i] <- "box_declared"
        warnings_v[[i]] <- warns
        next
      }
      packages[i] <- fn_hits[1L]
      resolutions[i] <- "ambiguous"
      warnings_v[[i]] <- c(warns, "SVT-W202")
      next
    }

    # 4. Library walk against installed namespaces.
    walk_pkgs <- file_ix$library_pkgs
    if (length(walk_pkgs)) {
      hits <- character()
      for (p in walk_pkgs) {
        exp <- namespace_exports_cached(p, exports_cache)
        if (name %in% exp) hits <- c(hits, p)
      }
      hits <- sort(unique(hits))
      if (length(hits) == 1L) {
        packages[i] <- hits
        resolutions[i] <- "library_walk"
        warnings_v[[i]] <- warns
        next
      }
      if (length(hits) > 1L) {
        packages[i] <- hits[1L]
        resolutions[i] <- "ambiguous"
        warnings_v[[i]] <- c(warns, "SVT-W202")
        next
      }
    }

    # 5. Unresolved.
    packages[i] <- NA_character_
    resolutions[i] <- "unresolved"
    warnings_v[[i]] <- c(warns, "SVT-W203")
  }

  fq_names <- ifelse(is.na(packages),
                     paste0("<unknown>::", calls$name),
                     paste0(packages, "::", calls$name))

  tibble::tibble(
    from_file = calls$from_file,
    name = calls$name,
    package = packages,
    fq_name = fq_names,
    resolution = resolutions,
    internal = as.logical(calls$internal),
    container = calls$container,
    in_def_kind = calls$in_def_kind,
    in_def_name = calls$in_def_name,
    in_def_namespace = calls$in_def_namespace,
    line = as.integer(calls$line),
    col = as.integer(calls$col),
    warnings = warnings_v
  )
}

#' Index per-file imports into resolver-friendly lookup tables.
#'
#' Output: a named list keyed by `from_file`, each entry an
#' `empty_file_imports()`-shaped list:
#'   `alias_to_pkg`  named list, alias -> pkg
#'                   (covers `box::use(pkg)` and `box::use(alias = pkg)`).
#'   `fn_to_pkg`     named list, fn name -> char vec of candidate pkgs
#'                   (from `box::use(pkg[fn1, fn2])`).
#'   `library_pkgs`  unique sorted vector of pkgs from library/require.
#'
#' Whole-namespace `box::use(pkg[...])` clauses are recorded as aliases
#' too (the import binds `pkg`), but contribute nothing to `fn_to_pkg`
#' since the function set is open. SVT-W005 flags the brittleness.
#'
#' @noRd
per_file_import_index <- function(imports) {
  out <- list()
  if (!nrow(imports)) return(out)

  for (f in unique(imports$from_file)) {
    rows <- imports[imports$from_file == f, , drop = FALSE]
    alias_to_pkg <- list()
    fn_to_pkg <- list()
    library_pkgs <- character()

    for (i in seq_len(nrow(rows))) {
      kind <- rows$kind[i]
      pkg <- rows$package[i]
      if (kind %in% c("library", "require") && !is.na(pkg)) {
        library_pkgs <- c(library_pkgs, pkg)
        next
      }
      if (kind != "box_use" || is.na(pkg)) next

      alias <- rows$alias[i]
      bind <- if (!is.na(alias) && nzchar(alias)) alias else pkg
      alias_to_pkg[[bind]] <- pkg

      fn_set <- rows$function_set[[i]]
      if (length(fn_set)) {
        for (fn in fn_set) {
          prev <- fn_to_pkg[[fn]] %||% character()
          fn_to_pkg[[fn]] <- unique(c(prev, pkg))
        }
      }
    }

    out[[f]] <- list(
      alias_to_pkg = alias_to_pkg,
      fn_to_pkg = fn_to_pkg,
      library_pkgs = sort(unique(library_pkgs))
    )
  }
  out
}

#' @noRd
empty_file_imports <- function() {
  list(alias_to_pkg = list(), fn_to_pkg = list(),
       library_pkgs = character())
}

#' Return the exported names of `pkg`, with caching across calls.
#'
#' Uses `getNamespaceExports()` which requires the package to be
#' installed and loadable. Failures (package missing, broken install)
#' return an empty vector — the resolver then falls through to
#' "unresolved" rather than aborting.
#'
#' @noRd
namespace_exports_cached <- function(pkg, cache) {
  if (!is.null(cache[[pkg]])) return(cache[[pkg]])
  exp <- tryCatch(getNamespaceExports(pkg), error = function(e) character())
  cache[[pkg]] <- exp
  exp
}

#' Compute the set of direct packages for an app.
#'
#' "Direct" means any of:
#'   - appears in `library()` / `require()` in the source,
#'   - appears in any `box::use()` clause,
#'   - listed in the app's `DESCRIPTION` Imports/Depends (for packaged
#'     Shiny apps).
#'
#' Returns a sorted, deduplicated character vector.
#'
#' @noRd
direct_packages <- function(imports, app_path = NULL) {
  pkgs <- character()
  if (nrow(imports)) {
    declared <- imports$package[
      imports$kind %in% c("library", "require", "box_use") &
        !is.na(imports$package)
    ]
    pkgs <- c(pkgs, declared)
  }
  if (!is.null(app_path)) {
    desc_pkgs <- description_imports(app_path)
    pkgs <- c(pkgs, desc_pkgs)
  }
  sort(unique(pkgs))
}

#' Read Imports + Depends fields from the app's DESCRIPTION (if any).
#'
#' Returns a character vector of package names, each stripped of
#' version constraints. Returns `character()` when no DESCRIPTION is
#' present — typical for non-packaged apps.
#'
#' @noRd
description_imports <- function(app_path) {
  desc_path <- file.path(app_path, "DESCRIPTION")
  if (!file.exists(desc_path)) return(character())
  raw <- tryCatch(read.dcf(desc_path), error = function(e) NULL)
  if (is.null(raw) || !nrow(raw)) return(character())

  fields <- intersect(c("Imports", "Depends"), colnames(raw))
  if (!length(fields)) return(character())

  out <- character()
  for (fld in fields) {
    val <- raw[1L, fld]
    if (is.na(val) || !nzchar(val)) next
    parts <- strsplit(val, ",", fixed = TRUE)[[1L]]
    parts <- trimws(parts)
    parts <- sub("\\s*\\(.*$", "", parts)  # drop (>= 1.2.3) version pins
    parts <- parts[nzchar(parts) & parts != "R"]
    out <- c(out, parts)
  }
  sort(unique(out))
}

#' Read package versions from `renv.lock`, if present.
#'
#' Returns a named character vector: `c(pkg = "version", ...)`. Empty
#' when no lockfile exists or `renv` is unavailable.
#'
#' @noRd
read_lockfile_versions <- function(app_path) {
  lock <- file.path(app_path, "renv.lock")
  if (!file.exists(lock)) return(character())
  if (!requireNamespace("renv", quietly = TRUE)) return(character())
  raw <- tryCatch(renv::lockfile_read(lock), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$Packages)) return(character())
  out <- vapply(raw$Packages,
                function(p) p$Version %||% NA_character_,
                character(1))
  out <- out[!is.na(out)]
  out
}

#' Build per-feature inventories for every feature in `features`.
#'
#' Returns a named list keyed by feature name. Each entry has:
#'   `feature`           - feature name
#'   `kind`              - "feature" or "module"
#'   `functions`         - tibble of FunctionCall rows
#'   `packages`          - tibble of derived PackageUse rows
#'   `package_versions`  - named char vec from renv.lock (or empty)
#'   `warnings`          - tibble (`code`, `file`, `line`, `col`, `message`)
#'
#' Categories come from the feature's `package_categories` map (manifest);
#' otherwise `unset` and SVT-W205 fires for any package non-trivially used.
#'
#' @noRd
build_inventory <- function(graph, features, app_path = NULL) {
  refs <- if (!is.null(app_path)) {
    build_references_table(app_path)
  } else {
    empty_refs()
  }
  resolved <- resolve_function_origins(refs, graph$imports)
  direct <- direct_packages(graph$imports, app_path)
  versions <- if (!is.null(app_path)) read_lockfile_versions(app_path)
              else character()

  out <- list()
  if (!length(features)) return(out)

  for (f in features) {
    out[[f$name]] <- build_feature_inventory(f, graph, resolved, direct,
                                             versions)
  }
  out
}

#' Build the inventory for one feature/module subgraph.
#'
#' @noRd
build_feature_inventory <- function(feature, graph, resolved, direct,
                                    versions) {
  fn_calls <- filter_resolved_for_feature(resolved, feature, graph)

  cats <- as.list(feature$package_categories %||% list())
  cats[!nzchar(names(cats) %||% "")] <- NULL
  warnings_acc <- list()

  push_warning <- function(code, file, line, col) {
    warnings_acc[[length(warnings_acc) + 1L]] <<- tibble::tibble(
      code = code, file = file %||% NA_character_,
      line = as.integer(line %||% NA_integer_),
      col = as.integer(col %||% NA_integer_),
      message = warning_message(code)
    )
  }

  # Per-call warnings (W201/202/203/204). W205 fires per package.
  per_row_warnings <- character(nrow(fn_calls))
  per_row_warnings[] <- ""
  rcats <- rep(NA_character_, nrow(fn_calls))
  rdirect <- rep(FALSE, nrow(fn_calls))

  if (nrow(fn_calls)) {
    for (i in seq_len(nrow(fn_calls))) {
      pkg <- fn_calls$package[i]
      is_direct <- !is.na(pkg) && pkg %in% direct
      rdirect[i] <- is_direct
      cat <- if (!is.na(pkg)) cats[[pkg]] %||% NA_character_ else NA_character_
      rcats[i] <- if (is.character(cat) && length(cat) == 1L && nzchar(cat)) {
        cat
      } else {
        NA_character_
      }
      warns <- fn_calls$warnings[[i]]
      if (!is.na(pkg) && !is_direct) {
        warns <- c(warns, "SVT-W204")
      }
      for (w in warns) {
        push_warning(w, fn_calls$from_file[i],
                     fn_calls$line[i], fn_calls$col[i])
      }
      per_row_warnings[i] <- paste(warns, collapse = ",")
    }
  }

  per_site <- if (nrow(fn_calls)) {
    tibble::tibble(
      package = fn_calls$package,
      fn = fn_calls$name,
      fq_name = fn_calls$fq_name,
      file = fn_calls$from_file,
      line = as.integer(fn_calls$line),
      col = as.integer(fn_calls$col),
      resolution = fn_calls$resolution,
      direct = rdirect,
      category = ifelse(is.na(rcats), "unset", rcats),
      warnings = lapply(strsplit(per_row_warnings, ",", fixed = TRUE),
                        function(x) x[nzchar(x) & !is.na(x)])
    )
  } else {
    NULL
  }

  fc <- collate_function_calls(per_site)

  pkg_view <- derive_package_view(fc)

  if (nrow(pkg_view)) {
    for (i in seq_len(nrow(pkg_view))) {
      if (identical(pkg_view$category[i], "unset") &&
          !is.na(pkg_view$package[i]) &&
          pkg_view$call_count[i] > 0L) {
        push_warning("SVT-W205", NA_character_, NA_integer_, NA_integer_)
      }
    }
  }

  feature_pkgs <- unique(pkg_view$package[!is.na(pkg_view$package)])
  fv <- versions[intersect(names(versions), feature_pkgs)]
  for (p in setdiff(feature_pkgs, names(fv))) {
    push_warning("SVT-W206", NA_character_, NA_integer_, NA_integer_)
  }

  warnings_tbl <- if (length(warnings_acc)) {
    do.call(rbind, warnings_acc)
  } else {
    empty_warnings()
  }

  list(
    feature = feature$name,
    kind = feature$kind %||% "feature",
    functions = fc,
    packages = pkg_view,
    package_versions = fv,
    warnings = warnings_tbl
  )
}

#' Filter the global resolved table to the calls inside this feature.
#'
#' Membership is by enclosing definition: every resolved call carries
#' `in_def_kind` / `in_def_name` / `in_def_namespace`. The owning node id
#' is derived from those, and the call is kept iff that id is in the
#' feature's node set.
#'
#' @noRd
filter_resolved_for_feature <- function(resolved, feature, graph) {
  if (!nrow(resolved)) return(resolved)

  in_def_id <- vapply(seq_len(nrow(resolved)), function(i) {
    kind <- resolved$in_def_kind[i]
    name <- resolved$in_def_name[i]
    ns <- resolved$in_def_namespace[i]
    if (is.na(kind) || is.na(name)) return(NA_character_)
    node_id(kind, ns, NA_character_, name)
  }, character(1))

  keep <- !is.na(in_def_id) & in_def_id %in% (feature$node_ids %||% character())
  resolved[keep, , drop = FALSE]
}

#' Collate per-call-site rows into per-(package, fn) FunctionCall rows.
#'
#' The FunctionCall schema groups every call site for the same
#' `(package, fn)` under `call_sites: [...]`. Inputs may be NULL (no
#' calls) — returns the canonical empty tibble.
#'
#' Resolution / direct / category come from the first per-site row in
#' source order. Warnings are unioned. Sites are sorted by (file, line,
#' col) for determinism.
#'
#' @noRd
collate_function_calls <- function(per_site) {
  if (is.null(per_site) || !nrow(per_site)) return(empty_function_calls())

  key <- paste(per_site$package, per_site$fn, sep = "")
  ord <- order(key, per_site$file, per_site$line, per_site$col)
  per_site <- per_site[ord, , drop = FALSE]
  key <- key[ord]

  by_key <- split(seq_len(nrow(per_site)), factor(key, levels = unique(key)))

  rows <- lapply(by_key, function(idx) {
    sub <- per_site[idx, , drop = FALSE]
    sites <- tibble::tibble(
      file = sub$file,
      line = as.integer(sub$line),
      col = as.integer(sub$col)
    )
    warns <- unique(unlist(sub$warnings, use.names = FALSE))
    warns <- warns[!is.na(warns) & nzchar(warns)]
    tibble::tibble(
      package = sub$package[1L],
      fn = sub$fn[1L],
      fq_name = sub$fq_name[1L],
      call_sites = list(sites),
      resolution = sub$resolution[1L],
      direct = any(sub$direct),
      category = sub$category[1L],
      warnings = list(warns)
    )
  })
  do.call(rbind, rows)
}

#' Derive the package-level view from the FunctionCall tibble.
#'
#' PackageUse rows are a per-package roll-up; multi-category packages
#' get `category = "mixed"`. `call_count` is the total number of call
#' sites across all listed functions in this package.
#'
#' @noRd
derive_package_view <- function(fn_calls) {
  empty <- tibble::tibble(
    package = character(), functions = list(), direct = logical(),
    category = character(), call_count = integer()
  )
  if (!nrow(fn_calls)) return(empty)

  by_pkg <- split(seq_len(nrow(fn_calls)), fn_calls$package)
  rows <- list()
  for (p in sort(names(by_pkg))) {
    sub <- fn_calls[by_pkg[[p]], , drop = FALSE]
    fns <- sort(unique(sub$fn))
    cats <- unique(sub$category)
    cat <- if (length(cats) == 1L) {
      cats
    } else if (all(cats == "unset" | cats == cats[1L])) {
      cats[1L]
    } else {
      "mixed"
    }
    site_count <- sum(vapply(sub$call_sites, nrow, integer(1)))
    rows[[length(rows) + 1L]] <- tibble::tibble(
      package = p,
      functions = list(fns),
      direct = any(sub$direct),
      category = cat,
      call_count = as.integer(site_count)
    )
  }
  if (!length(rows)) return(empty)
  do.call(rbind, rows)
}

#' @noRd
empty_function_calls <- function() {
  tibble::tibble(
    package = character(), fn = character(), fq_name = character(),
    call_sites = list(), resolution = character(), direct = logical(),
    category = character(), warnings = list()
  )
}

#' @noRd
empty_refs <- function() {
  tibble::tibble(
    from_file = character(), kind = character(), name = character(),
    namespace = character(), container = character(), package = character(),
    internal = logical(), in_def_kind = character(),
    in_def_name = character(), in_def_namespace = character(),
    line = integer(), col = integer()
  )
}

#' Render one feature's inventory as a JSON document.
#'
#' Emits the canonical inventory.json shape with `schema_version` 1.0.
#' Stability-contracted: additive changes only within v1, no removals
#' or renames. Uses base R for serialization (no jsonlite dep) so the
#' package keeps its lean dependency surface.
#'
#' Returns a single JSON string. Determinism: keys sorted, function rows
#' in (package, fn, file, line) order, package rows in (package) order.
#'
#' @noRd
render_inventory_json <- function(inv) {
  fns <- inv$functions
  pkgs <- inv$packages

  if (nrow(fns)) {
    ord <- order(fns$package, fns$fn)
    fns <- fns[ord, , drop = FALSE]
  }
  if (nrow(pkgs)) {
    pkgs <- pkgs[order(pkgs$package), , drop = FALSE]
  }

  fn_objs <- if (nrow(fns)) {
    vapply(seq_len(nrow(fns)), function(i) {
      sites_tbl <- fns$call_sites[[i]]
      sites_json <- if (!nrow(sites_tbl)) {
        character()
      } else {
        vapply(seq_len(nrow(sites_tbl)), function(k) {
          json_object(list(file = json_str(sites_tbl$file[k]),
                            line = json_int(sites_tbl$line[k]),
                            col = json_int(sites_tbl$col[k])))
        }, character(1))
      }
      json_object(list(
        package = json_str_or_null(fns$package[i]),
        `function` = json_str(fns$fn[i]),
        fq_name = json_str(fns$fq_name[i]),
        call_sites = json_array(sites_json),
        resolution = json_str(fns$resolution[i]),
        direct = json_bool(fns$direct[i]),
        category = json_str(fns$category[i]),
        warnings = json_str_array(fns$warnings[[i]])
      ))
    }, character(1))
  } else {
    character()
  }

  pkg_objs <- if (nrow(pkgs)) {
    vapply(seq_len(nrow(pkgs)), function(i) {
      json_object(list(
        package = json_str(pkgs$package[i]),
        functions = json_str_array(pkgs$functions[[i]]),
        direct = json_bool(pkgs$direct[i]),
        category = json_str(pkgs$category[i]),
        call_count = json_int(pkgs$call_count[i])
      ))
    }, character(1))
  } else {
    character()
  }

  pv <- inv$package_versions
  pv_obj <- if (length(pv)) {
    nm <- names(pv)
    ord <- order(nm)
    nm <- nm[ord]; pv <- pv[ord]
    pairs <- vapply(seq_along(pv),
                    function(i) paste0(json_str(nm[i]), ":", json_str(pv[i])),
                    character(1))
    paste0("{", paste(pairs, collapse = ","), "}")
  } else {
    "{}"
  }

  body <- list(
    feature = json_str(inv$feature),
    schema_version = json_str("1.0"),
    package_versions = pv_obj,
    functions = json_array(fn_objs),
    packages = json_array(pkg_objs)
  )

  json_object(body)
}

#' Write the inventory.json artifact for one feature.
#'
#' Layout: `<out_dir>/<feature>/inventory.json`. The directory is
#' created when missing. Returns the file path written.
#'
#' @noRd
write_inventory_json <- function(inv, out_dir) {
  feat_dir <- file.path(out_dir, inv$feature)
  if (!dir.exists(feat_dir)) dir.create(feat_dir, recursive = TRUE)
  path <- file.path(feat_dir, "inventory.json")
  text <- render_inventory_json(inv)
  writeLines(text, path)
  path
}

# JSON helpers -----------------------------------------------------------
#
# Hand-rolled to keep the package free of a JSON dependency. The
# inventory.json stability contract is the only consumer; we control
# the shape and can guarantee determinism here.

#' @noRd
json_str <- function(x) {
  if (is.null(x) || (is.character(x) && length(x) == 1L && is.na(x))) {
    return("null")
  }
  s <- as.character(x)
  s <- gsub("\\\\", "\\\\\\\\", s)
  s <- gsub("\"", "\\\\\"", s)
  s <- gsub("\n", "\\\\n", s, fixed = TRUE)
  s <- gsub("\r", "\\\\r", s, fixed = TRUE)
  s <- gsub("\t", "\\\\t", s, fixed = TRUE)
  paste0("\"", s, "\"")
}

#' @noRd
json_str_or_null <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return("null")
  json_str(x)
}

#' @noRd
json_int <- function(x) {
  if (is.null(x) || is.na(x)) return("null")
  as.character(as.integer(x))
}

#' @noRd
json_bool <- function(x) if (isTRUE(x)) "true" else "false"

#' @noRd
json_array <- function(elems) {
  if (!length(elems)) return("[]")
  paste0("[", paste(elems, collapse = ","), "]")
}

#' @noRd
json_str_array <- function(xs) {
  if (!length(xs)) return("[]")
  paste0("[", paste(vapply(xs, json_str, character(1)), collapse = ","), "]")
}

#' @noRd
json_object <- function(kv) {
  if (!length(kv)) return("{}")
  keys <- names(kv)
  pairs <- vapply(seq_along(kv), function(i) {
    paste0(json_str(keys[i]), ":", as.character(kv[[i]]))
  }, character(1))
  paste0("{", paste(pairs, collapse = ","), "}")
}
