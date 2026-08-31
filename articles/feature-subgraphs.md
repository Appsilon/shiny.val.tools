# Feature subgraphs and the manifest

A **feature subgraph** is the closed reactive subgraph rooted at one or
more outputs. It is the unit of validation. This vignette covers how
slicing works, how to author a `features.yml` manifest, and the schema
of the doc stub the package emits per feature.

## Default slicing rule

Without a manifest, the package emits **one feature per top-level output
and per top-level side-effecting observer**. The output `output$km_plot`
produces a feature named `km_plot`; an assigned download handler
`download_csv <- downloadHandler(...)` produces a feature named
`download_csv`.

Each feature’s subgraph is the transitive upstream closure of its root,
terminating at:

- Input nodes (graph leaves)
- Module instance nodes (opaque per the module-contract design)
- Top-level reactiveValues entries

Module-internal observers and reactives reachable only via a module
instance node are **not** part of the parent’s feature subgraph. They
belong to the module subgraph.

The default rule guarantees there are no unclaimed outputs — every
behavior is captured. The cost is that grouping is per-output, not
per-user-feature.

## Module subgraphs

Each Shiny module definition produces a **module subgraph** in addition
to any feature subgraphs. The module subgraph is rooted at:

- The module’s outputs (assigned in the body of `moduleServer()`)
- The module’s returned reactives (the value returned by the server
  function)

Module subgraphs are validated standalone. The doc stub for a module
mirrors the feature stub but is keyed under `modules:` in the manifest.

A module subgraph’s contract — its inputs, outputs, returned reactives —
is auto-extracted and shown in the doc stub for the developer to
confirm. Confirmation is implicit: the module appears in the manifest
with a complete `intended_use`.

A `module_instance` node referenced by a feature, but with no
corresponding `moduleServer()` definition reachable in the enumerated
source, emits **SVT-W104** (“orphan module instance”) — likely a missing
source or a misnamed call.

## The `features.yml` manifest

A developer-authored declaration of feature groupings and per-feature
metadata. Lives at the app root (configurable via the `manifest`
argument to
[`svt_validate()`](https://appsilon.github.io/shiny.val.tools/reference/svt_validate.md)).

### Schema

``` yaml
features:
  - name: km_plot
    intended_use: |
      Generate Kaplan–Meier survival curves for the trial primary
      endpoint cohort, stratified by treatment arm.
    risk_classification: high            # high | medium | low | not_required
    rationale: ~                         # required if risk_classification = not_required
    roots:
      - output: km_plot
      - output: km_summary_table
      - observer: km_export
    package_categories:
      survival: method
      dplyr:    framework
      logger:   utility
    reviewers:
      - role: developer
        name: ~
        signed_at: ~
      - role: validation_engineer
        name: ~
        signed_at: ~

modules:
  - name: filter_panel
    intended_use: |
      Provide reusable subject filters across analyses.
    risk_classification: medium
```

### Field semantics

- **`name`** — the stable identifier; used in artifact filenames.
- **`intended_use`** — free text; the validation surface declaration.
  Required.
- **`risk_classification`** — one of `high`, `medium`, `low`,
  `not_required`. Required.
- **`rationale`** — required if `risk_classification = not_required`,
  optional otherwise. Encodes the “decision not to validate is part of
  validation” principle.
- **`roots`** — list of `{output: <name>}` or `{observer: <name>}`
  entries. At least one required. For module-namespaced roots, use
  `{output: <namespace>/<name>}` form.
- **`package_categories`** — optional declaration overriding doc-stub
  category fields (utility / framework / method).
- **`reviewers`** — placeholder structure. Names and timestamps filled
  by humans during sign-off.

### Generating a starter manifest

``` r

graph <- svt_build_graph(svt_parse("path/to/app"))
svt_manifest_template(graph, out_path = "features.yml")
```

This produces one entry per default-sliced feature root with empty
fields (`~`) for the developer to fill in.

### Validating a manifest

``` r

issues <- svt_manifest_validate("features.yml", graph)
issues
#> # A tibble: 1 × 3
#>   code      name     message
#>   <chr>     <chr>    <chr>
#>   SVT-W101  km_plot  Manifest root output: km_plt not found in graph
```

An empty tibble means the manifest is ready to use.

The checks (per validate call):

- **SVT-W101** — Manifest references unknown root.
- **SVT-W102** — Two features claim the same root.
- **SVT-W105** — `risk_classification = not_required` without
  `rationale`.
- **fatal** — name collision across features+modules.

By default (`lenient = FALSE`), any of these aborts the slice. With
`lenient = TRUE` they become warnings and slicing proceeds best-effort.

## Unclaimed-output completeness check

After slicing, the package walks the global graph for every top-level
output and observer. Any that is not the root of any manifest-declared
feature emits **SVT-W103** (“unclaimed output”), returned by
[`svt_unclaimed()`](https://appsilon.github.io/shiny.val.tools/reference/svt_unclaimed.md).

This is the validation-completeness gate. An app with zero SVT-W103
warnings has every user-visible behavior claimed by some feature.

The default slicing rule (one feature per output) prevents SVT-W103 from
firing in the default case — every output gets an auto-generated feature
stub. The warning becomes meaningful only when a user-authored manifest
is present and explicit.

``` r

svt_unclaimed(features)
#> # A tibble: 1 × 5
#>   code      file       line   col message
#>   <chr>     <chr>     <int> <int> <chr>
#>   SVT-W103  server.R     48    3 Unclaimed output: output:debug_text
```

## Doc stub schema

For each feature and each module, the package emits a markdown file at
`<out_dir>/<name>.md`. Section order:

``` markdown
# Feature: km_plot

## Intended use
[from manifest, or "(not declared - fill before validation)"]

## Risk classification
[from manifest, or "(not declared)"]

## Rationale
[from manifest, optional]

## Reactive subgraph
[link to <name>.html]

## Module contract                      # module-only
[inputs / outputs / returned reactives]

## Functions called
[auto-filled from inventory; one row per pkg::fn]

## Packages used
[auto-filled; one row per package]

## Warnings
[auto-filled; one row per warning code triggered in this subgraph]

## Reviewers
- Developer: __________________ Date: __________
- Validation Engineer: __________________ Date: __________
```

The auto-filled sections are regenerated on every run. Human-authored
sections (intended use, rationale, reviewer signatures) are preserved
across regenerations by reading the existing file and merging.

## Regeneration semantics

On regeneration:

- Subgraph HTML widgets are overwritten.
- Function and package inventories are overwritten.
- Warning lists are overwritten.
- Doc stub markdown is **merged**: human-authored sections preserved,
  auto-filled sections refreshed.

Auto-filled sections are: *Intended use*, *Risk classification*,
*Rationale*, *Reactive subgraph*, *Module contract* (modules only),
*Functions called*, *Packages used*, *Warnings*. Everything else
(notably *Reviewers*) is preserved verbatim if it exists on disk.

This is the artifact-level analogue of “pull the latest, but keep my
local edits”. A developer can sign the *Reviewers* section once; the
signature survives subsequent regenerations until the developer removes
it.

## Selecting a subset of features

[`svt_validate()`](https://appsilon.github.io/shiny.val.tools/reference/svt_validate.md)
accepts `features` and `modules` character vectors to limit the
artifacts produced — useful in CI for re-rendering only what changed.

``` r

svt_validate(
  app_path = "path/to/app",
  features = c("km_plot", "km_summary_table"),
  modules  = "filter_panel"
)
```

Slicing still happens for the whole app (so unclaimed-output detection
remains correct), but only the named features and modules get rendered.
