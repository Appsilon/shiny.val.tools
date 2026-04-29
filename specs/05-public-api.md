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
- `out_dir` — where artifacts are written. Created if missing. If it exists and is non-empty, see "Output directory lifecycle" below — `svt_render()` only proceeds when it can prove the contents are its own previously generated artifacts.
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

## Output directory lifecycle

Validation output accumulates across runs — `## Reviewers` sections get signed, manifests get refined, the file roster shifts as the app grows. The lifecycle has to refresh auto-filled content without ever silently destroying human-authored content.

### Generation manifest

Every successful `svt_render()` call writes `validation/.svt_manifest.json` listing every artifact it produced, each with an MD5 hash (from `tools::md5sum()`) of the file content as written:

```json
{
  "schema_version": "1.0",
  "artifacts": [
    {"path": "index.md",                              "md5": "..."},
    {"path": "index.html",                            "md5": "..."},
    {"path": "app--view--mod_card.md",                "md5": "..."},
    {"path": "app--view--mod_card.html",              "md5": "..."},
    {"path": "app--view--mod_card/inventory.json",    "md5": "..."}
  ]
}
```

Paths are relative to `out_dir`. The manifest is the **proof of authorship**: any file listed in it that still hashes to its recorded value is one we wrote and nobody touched.

### First run vs re-run

- **`out_dir` does not exist** → create, render, write manifest. Proceed.
- **`out_dir` exists, is empty** → render, write manifest. Proceed.
- **`out_dir` exists, non-empty, no `.svt_manifest.json` present** → abort. We can't tell which files are ours and refuse to risk overwriting unknown content. Error message names a remediation: pass a fresh path or remove the existing directory.
- **`out_dir` exists, non-empty, manifest present** → proceed under the re-run rules below.

### Re-run rules (manifest present)

For each artifact path the prior manifest lists:

1. **File missing on disk** — user removed it; ignore.
2. **File present, current MD5 matches manifest** — pristine (we wrote it; nobody touched).
3. **File present, current MD5 differs from manifest** — edited (user touched).

Compute orphans = paths in the prior manifest that are not in the current run's planned artifact set:

- **Pristine orphans** → delete silently.
- **Edited orphans** → abort before writing anything. The error message lists each orphan with its path. The user backs them up (or accepts the loss) and re-runs.

For paths that are in both prior manifest and current run:

- **Doc stubs (`*.md`)** — always pass through `merge_doc_stub()`. Auto-filled sections (`## Intended use`, `## Risk classification`, `## Rationale`, `## Reactive subgraph`, `## Module contract`, `## Functions called`, `## Packages used`, `## Warnings`) refresh from the freshly rendered version. Non-auto sections (`## Reviewers`, plus any reviewer-introduced sections) are taken verbatim from the on-disk file. The merge runs whether the file is pristine or edited — it is cheap and idempotent on pristine inputs.
- **HTML widgets and `inventory.json`** — blind overwrite. No human-editable content lives in these.

After all writes succeed, `.svt_manifest.json` is rewritten with fresh hashes for the current artifact set. If any write fails partway through, the on-disk manifest is left at the prior version (the next run will see the in-flight artifact as "missing" or "pristine" and recover).

### Files we did not generate

A foreign file the user dropped into `out_dir` is by definition not in the manifest. The lifecycle never touches such files: it neither inspects nor deletes them. They are also not preserved in any sense — they simply persist on disk because nothing in the package walks them.

## Determinism contract

Given the same `app_path` content (file hashes), the same `manifest`, and the same package version, `svt_validate()` produces byte-identical output. This contract underwrites the diff-based regeneration semantics in spec 02 and the determinism claim in spec 04. The `.svt_manifest.json` artifact is itself deterministic: its `artifacts` array is sorted by `path` and its hashes are content-derived.

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
