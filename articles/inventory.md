# Per-feature inventory: functions and packages

The per-feature inventory answers: *which functions does this feature
actually call, and where do they come from?* Built by
[`svt_inventory()`](https://appsilon.github.io/shiny.val.tools/reference/svt_inventory.md),
the inventory is the canonical artifact for QC-risk-based validation: it
ties function calls to the feature’s declared intended use, not to the
app globally.

## Function-centric, package view derived

The canonical unit is a **function call**, not a package. For each
feature and each module subgraph, the package emits:

    FunctionCall {
      package      : string                # e.g. "dplyr"
      function     : string                # e.g. "filter"
      fq_name      : string                # "dplyr::filter"
      call_sites   : [{file, line, col}]   # all locations within this subgraph
      resolution   : enum {explicit, box_declared, library_walk,
                           ambiguous, unresolved}
      direct       : bool                  # true if from a direct import
      category     : enum {utility, framework, method, unset}
      warnings     : [warning_code]
    }

The package list view is **derived**:

    PackageUse {
      package      : string
      functions    : [string]             # union of fn names in this subgraph
      direct       : bool                 # any direct call → direct
      category     : enum {utility, framework, method, mixed, unset}
      call_count   : int                  # total call sites across functions
    }

Both views are emitted. Function-centric is canonical; package view is
convenience for consumers operating at the package layer.

## Function-origin resolution

Resolution is attempted per call site in priority order. The first match
wins; the `resolution` field records which rule fired.

1.  **Explicit `pkg::fn` or `pkg:::fn`** — unambiguous. `:::` access
    additionally emits **SVT-W201** (“internal function access”).
2.  **Box-declared, function set:** the call name appears in a
    `box::use(pkg[fn1, fn2])` clause for the file. Resolves to
    `pkg::fn`.
3.  **Box-declared, qualified:** call site uses `pkg$fn` or `alias$fn`
    matching a `box::use(pkg)` or `box::use(alias = pkg)` clause.
    Resolves via alias.
4.  **Library walk:** call site uses a bare name; the file has
    [`library(pkg)`](https://rdrr.io/r/base/library.html) (or
    [`require(pkg)`](https://rdrr.io/r/base/library.html)) and `pkg`
    exports a function with that name. Resolves to `pkg::fn`.
5.  **Ambiguous:** multiple loaded namespaces export the same name. All
    candidates are recorded; **SVT-W202** (“ambiguous origin”) is
    emitted; `resolution = ambiguous`.
6.  **Unresolved:** no rule applies. **SVT-W203** (“unresolved function
    call”) emitted; `package = "<unknown>"`; `resolution = unresolved`.

Resolution data is computed once at parse time and cached on the graph
object. Per-feature inventories filter from this cache.

## Direct vs transitive

A package is **direct** if any of:

- It appears in a [`library()`](https://rdrr.io/r/base/library.html) or
  [`require()`](https://rdrr.io/r/base/library.html) call within the
  enumerated source.
- It appears in any
  [`box::use()`](https://klmr.me/box/reference/use.html) clause within
  the enumerated source.
- It appears in the app’s `DESCRIPTION` `Imports` or `Depends` field
  (when the app is itself a packaged Shiny app).

Otherwise it is **transitive**.

When a function resolves to a transitive package — i.e., a function from
a dependency of a direct import that is called without itself being
declared — the call is flagged with **SVT-W204** (“transitive function
call”). This is informational, not an error: real-world apps do this
routinely (e.g., calling
[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)
when only `dplyr` is loaded).

## Utility / Framework / Method categories

Categories are **declarations**, not inferences. The package does not
auto-classify in v1.

Sources of category data, in priority order:

1.  Manifest `package_categories:` entry for the feature.
2.  (Reserved for future: app-level default category map.)
3.  Doc stub field populated by the developer post-generation, then read
    back on the next run.
4.  Default `unset`. Emits **SVT-W205** (“category unset”) if the
    feature is non-trivially using the package.

In the function-centric inventory, every function row carries the
category of its package within this feature. In the package view, a
package whose functions in this feature span multiple categories gets
`category = mixed`; the function rows retain their individual values.

See
[`vignette("validation-framing")`](https://appsilon.github.io/shiny.val.tools/articles/validation-framing.md)
for what U/F/M mean and why they’re not auto-classified.

## Output format: `inventory.json`

Per feature/module, written to `<out_dir>/<name>/inventory.json`:

``` json
{
  "feature": "km_plot",
  "schema_version": "1.0",
  "package_versions": {
    "survival": "3.5-7",
    "dplyr": "1.1.4"
  },
  "functions": [
    {
      "package": "survival",
      "function": "survfit",
      "fq_name": "survival::survfit",
      "call_sites": [{"file": "R/km.R", "line": 42, "col": 12}],
      "resolution": "library_walk",
      "direct": true,
      "category": "method",
      "warnings": []
    }
  ],
  "packages": [
    {
      "package": "survival",
      "functions": ["survfit", "Surv"],
      "direct": true,
      "category": "method",
      "call_count": 3
    }
  ]
}
```

### Stability contract

The schema is **stable within v1**. Downstream tooling — per-trial
source-scan workflows, custom test generators, package risk-scoring
integrations — consumes this format.

- `schema_version` lets future versions be additive without breaking
  consumers.
- Field names and types do not change within v1.
- New fields may be added; existing fields are never removed or renamed.

If the implementation must diverge, the major version (`1.0` → `2.0`)
bumps and a migration note is published. Downstream pharma tooling
relies on this contract.

## Human-readable view

The doc stub (`<name>.md`) reads `inventory.json` and renders both
tables in markdown:

``` markdown
## Functions called

| Package  | Function | Resolution    | Direct | Category | Calls | Warnings |
|----------|----------|---------------|--------|----------|-------|----------|
| survival | survfit  | library_walk  | yes    | method   | 1     |          |
| survival | Surv     | library_walk  | yes    | method   | 2     |          |
| dplyr    | filter   | box_declared  | yes    | framework| 4     |          |

## Packages used

| Package  | Functions      | Direct | Category   | Call count |
|----------|----------------|--------|------------|------------|
| dplyr    | filter         | yes    | framework  | 4          |
| survival | Surv, survfit  | yes    | method     | 3          |
```

## `renv` / `pak` integration

The package does not reimplement dependency resolution. It uses:

- **`renv::dependencies(path)`** — static dependency scan; identifies
  packages referenced in the source.
- **`pak::pkg_deps(<pkg>)`** — transitive resolution when needed.
- **[`renv::lockfile_read()`](https://rstudio.github.io/renv/reference/lockfiles.html)**
  — when `renv.lock` is present, recorded versions populate
  `package_versions` in `inventory.json`, making artifacts reproducible
  against the same lockfile.

When neither `renv.lock` nor an installed package is available,
`package_versions` is omitted for that package and **SVT-W206**
(“version unresolved”) is recorded.

`renv` is a `Suggests:` dependency. The package functions without it;
lockfile-derived versions simply do not appear in `inventory.json`.
