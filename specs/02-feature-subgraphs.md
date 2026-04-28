# Feature subgraphs and the manifest

## Purpose

Defines how the global graph is sliced into feature subgraphs and module subgraphs, the `features.yml` manifest schema, the doc stub schema, and the unclaimed-output completeness check.

## Default slicing rule

Without a manifest, the package emits **one feature per top-level output and per top-level side-effecting observer**. The output `output$km_plot` produces a feature named `km_plot`; an assigned download handler `download_csv <- downloadHandler(...)` produces a feature named `download_csv`.

Each feature's subgraph is the transitive upstream closure of its root, terminating at:

- Input nodes (graph leaves)
- Module instance nodes (opaque per the module-contract design)
- Top-level reactiveValues entries

Module-internal observers and reactives reachable only via a module instance node are **not** part of the parent's feature subgraph. They belong to the module subgraph.

## Manifest: `features.yml`

A developer-authored declaration of feature groupings and per-feature metadata. Lives at the app root (configurable). Schema:

```yaml
features:
  - name: km_plot
    intended_use: |
      Generate Kaplan–Meier survival curves for the trial primary endpoint
      cohort, stratified by treatment arm.
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

Field semantics:

- `name` — the stable identifier; used in artifact filenames.
- `intended_use` — free text; the validation surface declaration. Required.
- `risk_classification` — one of `high`, `medium`, `low`, `not_required`. Required.
- `rationale` — required if `risk_classification = not_required`, optional otherwise. Encodes the "decision not to validate is part of validation" principle.
- `roots` — list of `{output: <name>}` or `{observer: <name>}` entries. At least one required. For module roots, use `{output: <namespace>/<name>}` form.
- `package_categories` — optional declaration overriding doc-stub category fields (see spec 03).
- `reviewers` — placeholder structure. Names and timestamps filled by humans during sign-off.

Multiple manifest files can be combined (e.g., one per module owner). Combination order: app root first, then per-folder manifests, with later entries overriding earlier ones on name collision.

## Manifest validation

When loading the manifest, the package checks:

- Every `roots` entry resolves to a node in the graph. Unknown root → SVT-W101, fatal unless `lenient = TRUE`.
- No two features claim the same root. Conflict → SVT-W102, fatal unless `lenient = TRUE`.
- Every `name` is unique across `features:` and across `modules:`. Collision → fatal.
- `risk_classification = not_required` without a `rationale` → SVT-W105, fatal.

## Module subgraphs

Each module definition produces a **module subgraph** in addition to any feature subgraphs. The module subgraph is rooted at:

- The module's outputs (assigned in the body of `moduleServer()`)
- The module's returned reactives (the value returned by the server function)

Module subgraphs are validated standalone. The doc stub for a module mirrors the feature stub but is keyed under `modules:` in the manifest.

A module subgraph's contract is auto-extracted (inputs, outputs, returned reactives) and shown in the doc stub for the developer to confirm. Confirmation is implicit: the module appears in the manifest with a complete `intended_use`.

A `module_instance` node referenced by a feature, but with no corresponding `moduleServer()` definition reachable in the enumerated source, emits SVT-W104 ("orphan module instance") — likely a missing source or a misnamed call.

## Doc stub schema

For each feature and each module, the package emits a markdown file at `validation/<name>.md`:

```markdown
# Feature: km_plot

## Intended use
[from manifest, or "(not declared — fill before validation)"]

## Risk classification
[from manifest, or "(not declared)"]

## Rationale
[from manifest, optional]

## Reactive subgraph
[link to validation/<name>.html]

## Functions called (function-centric inventory)
[auto-filled from spec 03; one row per pkg::fn with category and direct/transitive flags]

## Packages used (derived view)
[auto-filled; one row per package, with direct/transitive, category, call count]

## Warnings
[auto-filled; one row per warning code triggered in this subgraph]

## Reviewers
- Developer: __________________ Date: __________
- Validation Engineer: __________________ Date: __________
```

The auto-filled sections are regenerated on every run. Human-authored sections (intended use, rationale, reviewer signatures) are preserved across regenerations by reading the existing file and merging.

## Unclaimed-output completeness check

After slicing, the package walks the global graph for every top-level output and observer. Any that is not the root of any manifest-declared feature emits SVT-W103 ("unclaimed output"), surfaced in the per-app summary.

This is the validation-completeness gate. An app with zero SVT-W103 warnings has every user-visible behavior claimed by some feature, satisfying the overview's "validated subgraph = positive declaration" principle.

The default slicing rule (one feature per output) prevents SVT-W103 from firing in the default case — every output gets an auto-generated feature stub. The warning becomes meaningful only when a user-authored manifest is present and explicit.

## Regeneration semantics

On regeneration:

- Subgraph HTML widgets are overwritten.
- Function and package inventories are overwritten.
- Warning lists are overwritten.
- Doc stub markdown is **merged**: human-authored sections preserved, auto-filled sections refreshed.

When the auto-filled function or package list changes between runs, a diff marker is appended to the doc stub footer ("inventory changed since last regeneration"). This is the signal that re-validation may be required, and is the artifact-level analogue of a code change triggering re-review.

## Warning codes (range: SVT-W101 to SVT-W199)

- **SVT-W101** — Manifest references unknown node
- **SVT-W102** — Root claimed by multiple features
- **SVT-W103** — Unclaimed output (no manifest-declared feature owns this output)
- **SVT-W104** — Orphan module instance (no module definition found in enumerated source)
- **SVT-W105** — `risk_classification = not_required` without `rationale`

## Out of scope for this spec

- The function/package inventory (spec 03)
- Rendering of the subgraph (spec 04)
- The end-to-end CLI workflow (spec 05)
