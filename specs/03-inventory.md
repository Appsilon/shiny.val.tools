# Per-feature inventory: functions and packages

## Purpose

Defines the function-centric inventory data model, the derivation of the package-list view, function-origin resolution rules, the direct vs. transitive distinction, and Utility/Framework/Method category support.

## Canonical model: function-centric

The canonical inventory unit is a **function call**, not a package. For each feature and each module subgraph, the package emits a table:

```
FunctionCall {
  package      : string                # e.g. "dplyr"
  function     : string                # e.g. "filter"
  fq_name      : string                # "dplyr::filter"
  call_sites   : [{file, line, col}]   # all locations within this subgraph
  resolution   : enum {explicit, box_declared, library_walk, ambiguous, unresolved}
  direct       : bool                  # true if from a direct import
  category     : enum {utility, framework, method, unset}
  warnings     : [warning_code]
}
```

The package list view is **derived**:

```
PackageUse {
  package      : string
  functions    : [string]             # union of fn names in this subgraph
  direct       : bool                 # any direct call → direct
  category     : enum {utility, framework, method, mixed, unset}
  call_count   : int                  # total call sites across all listed functions
}
```

Both views are emitted. Function-centric is canonical; package view is convenience for consumers operating at the package layer.

## Function-origin resolution

Resolution is attempted per call site in priority order. The first match wins; the `resolution` field records which rule fired.

1. **Explicit `pkg::fn` or `pkg:::fn`** — unambiguous. `:::` access additionally emits SVT-W201 ("internal function access").
2. **Box-declared, function set:** the call name appears in a `box::use(pkg[fn1, fn2])` clause for the file. Resolves to `pkg::fn`.
3. **Box-declared, qualified:** call site uses `pkg$fn` or `alias$fn` matching a `box::use(pkg)` or `box::use(alias = pkg)` clause. Resolves via alias.
4. **Library walk:** call site uses a bare name; the file has `library(pkg)` (or `require(pkg)`) and `pkg` exports a function with that name. Resolves to `pkg::fn`.
5. **Ambiguous:** multiple loaded namespaces export the same name. All candidates are recorded; SVT-W202 ("ambiguous origin") is emitted; `resolution = ambiguous`.
6. **Unresolved:** no rule applies. SVT-W203 ("unresolved function call") emitted; `package = "<unknown>"`; `resolution = unresolved`.

Resolution data is computed once at parse time and cached on the graph object. Per-feature inventories filter from this cache.

## Direct vs. transitive

A package is **direct** if any of:

- It appears in a `library()` or `require()` call within the enumerated source.
- It appears in any `box::use()` clause within the enumerated source.
- It appears in the app's `DESCRIPTION` `Imports` or `Depends` field (when the app is itself a packaged Shiny app).

Otherwise it is **transitive**. Transitive resolution uses `pak::pkg_deps()` against the resolved package set; we do not reimplement the dependency graph.

When a function resolves to a transitive package — i.e., a function from a dependency of a direct import that is called without itself being declared — the call is flagged with SVT-W204 ("transitive function call"). This is informational, not an error: real-world apps do this routinely (e.g., calling `tibble::tibble` when only `dplyr` is loaded).

## Utility / Framework / Method categories

Categories are **declarations**, not inferences. The package does not auto-classify in v1.

Sources of category data, in priority order:

1. Manifest `package_categories:` entry for the feature.
2. (Reserved for v2: app-level default category map.)
3. Doc stub field populated by the developer post-generation, then read back on the next run.
4. Default `unset`. Emits SVT-W205 ("category unset") if the feature is non-trivially using the package.

In the function-centric inventory, every function row carries the category of its package within this feature. In the package view, a package whose functions in this feature span multiple categories gets `category = mixed`; the function rows retain their individual values.

A future v2 may add a category-suggestion table (e.g. `logger` → utility, `ggplot2` → framework). v1 contains no heuristics.

## Output formats

Two formats per subgraph, written to `validation/<name>/`:

### Machine-consumable: `inventory.json`

```json
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

The schema is **stable within v1**. Downstream tooling — per-trial source-scan workflows, custom test generators, package risk-scoring integrations — consumes this format. `schema_version` lets future versions be additive without breaking consumers.

### Human-readable: rendered into `<name>.md`

The doc stub (spec 02) reads `inventory.json` and renders both tables in markdown.

## Renv / pak integration

The package does not reimplement dependency resolution. It uses:

- **`renv::dependencies(path)`** — static dependency scan; identifies packages referenced in the source.
- **`pak::pkg_deps(<pkg>)`** — transitive resolution when needed.
- **`renv::lockfile_read()`** — when `renv.lock` is present, recorded versions populate `package_versions` in `inventory.json`, making artifacts reproducible against the same lockfile.

When neither `renv.lock` nor an installed package is available, `package_versions` is omitted for that package and SVT-W206 ("version unresolved") is recorded.

## Warning codes (range: SVT-W201 to SVT-W299)

- **SVT-W201** — Internal function access (`pkg:::fn`)
- **SVT-W202** — Ambiguous function origin (multiple namespaces export the name)
- **SVT-W203** — Unresolved function call (no rule resolves the origin)
- **SVT-W204** — Transitive function call (called function from a transitive dependency)
- **SVT-W205** — Category unset for a non-trivially used package
- **SVT-W206** — Package version unresolved (no lockfile or installed copy)

## Out of scope for this spec

- AST extraction (spec 01)
- Feature slicing (spec 02)
- Rendering (spec 04)
- Package risk scoring — defer to `riskmetric` / `val.meter`
