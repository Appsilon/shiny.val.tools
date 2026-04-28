# Public API and end-to-end workflow

## Purpose

Defines the exported function surface, the end-to-end workflow signature, return value classes, and conventions.

## Naming convention

All exported functions use the prefix `svt_` (shiny.val.tools). This namespaces the API surface and makes it greppable in user code.

## End-to-end function

```r
svt_validate(
  app_path,
  manifest = NULL,
  out_dir  = "validation",
  features = NULL,
  modules  = NULL,
  lenient  = FALSE
)
```

Arguments:

- `app_path` — path to a Shiny app root, or path to a `.zip` archive (auto-extracted to a temp dir).
- `manifest` — path to `features.yml`. If `NULL`, looks for `features.yml` in `app_path`. If absent, uses default one-feature-per-output slicing (per spec 02).
- `out_dir` — where artifacts are written. Created if missing.
- `features` — character vector of feature names to process. `NULL` = all.
- `modules` — character vector of module names to process. `NULL` = all.
- `lenient` — if `TRUE`, manifest validation errors become warnings rather than aborts.

Returns an `svt_validation` object: a list of artifact paths and summary metadata.

## Step-by-step API

```r
parsed    <- svt_parse(app_path)
graph     <- svt_build_graph(parsed)
features  <- svt_slice(graph, manifest = manifest)
inventory <- svt_inventory(graph, features)
artifacts <- svt_render(features, inventory, out_dir = out_dir)
```

Each step accepts the previous step's output and returns a typed list. Users can inspect or modify intermediate outputs without re-running earlier steps.

## Inspection helpers

```r
svt_summary(graph_or_features)      # node/edge/warning counts per feature
svt_warnings(graph_or_features)     # tibble of all warnings with file:line
svt_unclaimed(graph, features)      # tibble of outputs not claimed by any feature
svt_inspect(file_path)              # lobstr::ast() pretty-print of a file's AST
```

`svt_inspect()` requires the `lobstr` package (declared `Suggests:`); it falls back to base `print()` with a `cli` warning if `lobstr` is unavailable.

## Manifest helpers

```r
svt_manifest_template(graph, out_path = "features.yml")
```

Generates a starter `features.yml` from the graph using the default one-feature-per-output rule. The developer fills in `intended_use`, `risk_classification`, and category fields.

```r
svt_manifest_validate(manifest, graph)
```

Loads and validates the manifest against the graph. Returns a tibble of validation issues; an empty tibble signals "ready to use".

## Return value classes

- **`svt_parsed`** — output of `svt_parse()`. Contains: file roster, raw ASTs, source map.
- **`svt_graph`** — output of `svt_build_graph()`. Contains the tibbles defined in spec 01 (files, imports, sources, nodes, edges, warnings). Has `print` and `summary` methods.
- **`svt_features`** — output of `svt_slice()`. Contains per-feature subgraphs and manifest-merged metadata.
- **`svt_inventory`** — output of `svt_inventory()`. Contains per-feature function-centric and package-derived inventories per spec 03.
- **`svt_validation`** — output of `svt_validate()`. Contains paths to all artifacts and summary statistics.

All class objects support `print` and `summary`. Print methods produce orientation-level output; full inspection goes through the underlying tibbles.

## Error handling

The package uses `cli` for messages. Three severity levels:

- **info** — progress and counts during long operations.
- **warn** — for any SVT-W*** code triggered.
- **error** — only for unrecoverable issues (manifest references undefined node with `lenient = FALSE`, file not found, parse failure on a required file).

## Determinism contract

Given the same `app_path` content (file hashes), the same `manifest`, and the same package version, `svt_validate()` produces byte-identical output. This contract underwrites the diff-based regeneration semantics in spec 02 and the determinism claim in spec 04.

## Concurrency

v1 is single-threaded. The parse and inventory steps are the candidates for parallelism; deferring until profiling shows the need.

## Implementation order suggestion

Specs are written in dependency order; implementation should follow:

1. Spec 01 (graph model + AST extraction). The five intermediate tables are the foundation.
2. Spec 02 (slicing + manifest), built on the graph from step 1. Default rule first; manifest support second.
3. Spec 03 (inventory), built on the graph + features. Function-origin resolution first; categories and renv/pak integration second.
4. Spec 04 (rendering), once features and inventory are stable.
5. Spec 05 (public API), the user-facing wrapper. End-to-end function last.

Each step has its own warning code range, so the warning surface can be expanded incrementally without renumbering.

## Out of scope for this spec

- CLI / Rscript wrapper — `svt_validate()` is callable from Rscript directly.
- A Shiny dashboard for browsing artifacts — overview non-goal.
- Programmatic access to historical artifacts — deferred per "no diff across versions" non-goal.
