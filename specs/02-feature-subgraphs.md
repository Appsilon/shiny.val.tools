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
      app:      method                   # reserved key: this feature's own helpers
    verification: required               # required (default) | not_required
    rationale_verification: ~            # required if verification = not_required
    tests:                               # optional extra evidence links (spec 06)
      - file: tests/cypress/e2e/km.cy.js
        note: e2e — arm switch and plot render
      - external: UAT-014
        note: manual acceptance record
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
- `package_categories` — optional declaration overriding doc-stub category fields (see spec 03). The reserved key `app` declares the category of the feature's **own** source, which is what a generated helper stub carries (spec 06). It is reserved: a package genuinely named `app` cannot be categorised through this block.
- `verification` — `required` (default) or `not_required`. `not_required` sets the feature's coverage status to `waived` and suppresses verification warnings. Same principle as `risk_classification = not_required`: documenting the decision not to test is part of validation.
- `rationale_verification` — required when `verification = not_required`; missing → SVT-W311 (spec 06), fatal unless `lenient = TRUE`.
- `tests` — optional links to verification evidence the static test scan cannot see: Cypress specs, manual UAT records, `valtools` test-case ids. In-test `@covers` annotations remain the primary link; this is the escape hatch. Semantics in spec 06.
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

### Module identity

A module is detected by the presence of a `moduleServer()` call (the legacy `function(input, output, session)` form is also recognised). Identity — the stable name used in artifact filenames, manifests, and the `namespace` field of module-internal nodes — is derived from the file containing the `moduleServer()` call site, not from the wrapper-function binding name:

- **Default:** the relative file path with its extension stripped — e.g. `app/view/mod_card.R` → `app/view/mod_card`, `server.R` → `server`. This is the natural identity in rhino apps where every module file conventionally exports `server`, and it matches how a developer locates the module on disk.
- **Multi-module-per-file:** when a single file contains more than one `moduleServer()` call, identities are disambiguated as `<path>::<binding>`, where `<binding>` is the wrapper-function's bound name — e.g. `server::inner_server`, `server::outer_server` for two modules in the same `server.R`.

Artifact filenames preserve the path: `validation/app/view/mod_card.md`, `validation/app/view/mod_card.html`, `validation/app/view/mod_card/inventory.json`. Intermediate directories are created as needed.

For module roots in `features.yml`, the `<namespace>/<name>` form uses this identity — e.g. `app/view/mod_card/echo` references the `echo` output inside the `app/view/mod_card` module.

The wrapper-function binding name (the local function in user code, e.g. `counter_server` or `server`) is preserved on `module_instance` nodes as a display label (`fq_name = counter_server[c1]`) but does not appear in module identity.

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

## Test surface
[auto-filled from spec 06; selected harness plus one row per stimulus, observable,
intermediate, terminal, and app-defined helper. Omitted entirely when the testing
layer is switched off]

## Test coverage
[auto-filled from spec 06; coverage status, the tests mapped to this feature with
their harness, link mechanism and last known result, and what is not exercised]

## Reviewers
- Developer: __________________ Date: __________
- Validation Engineer: __________________ Date: __________
```

`## Test surface` and `## Test coverage` are auto-filled like the rest; they are omitted (not rendered empty) when `svt_render()` receives no surface/coverage records.

The auto-filled sections are regenerated on every run. Human-authored sections (intended use, rationale, reviewer signatures) are preserved across regenerations by reading the existing file and merging.

## Unclaimed-output completeness check

After slicing, the package walks the global graph for every top-level output and observer. Any that is not the root of any manifest-declared feature emits SVT-W103 ("unclaimed output"), surfaced in the per-app summary.

This is the validation-completeness gate. An app with zero SVT-W103 warnings has every user-visible behavior claimed by some feature, satisfying the overview's "validated subgraph = positive declaration" principle.

The default slicing rule (one feature per output) prevents SVT-W103 from firing in the default case — every output gets an auto-generated feature stub. The warning becomes meaningful only when a user-authored manifest is present and explicit.

### The dual gate

This check has a verification counterpart in spec 06, and the pair is what the index reports:

```
SVT-W103  behaviour a user can reach that no feature claims   → validation gap
SVT-W301  a claimed feature that no test exercises            → verification gap
```

Neither number means much alone. Zero of both is a validated surface that is fully claimed and fully exercised.

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
- Test surface derivation, coverage classification, and the `verification` / `tests` manifest semantics (spec 06)

## Package targets: exported modules as the unit

A target directory carrying both `DESCRIPTION` and `NAMESPACE` is a **package
target**. `DESCRIPTION` alone is not sufficient — it also appears in project
layouts that are not packages.

Such a package has no `app.R` / `ui.R` / `server.R` entrypoint and therefore no
top-level outputs, so `default_feature_roots()` yields nothing and the target
produces **zero features**. This is correct, not a degenerate case: a module
library has no app-level behaviour to slice. Its modules are the deliverable.

### Which modules get a record

Only modules whose server function is **exported** by `NAMESPACE`. An
unexported module is implementation detail: it gets no record of its own, but
it still appears as a `module_instance` terminal inside every exported module
that instantiates it, exactly as a child module does in an app target.

A module qualifies when the function wrapping its `moduleServer()` call is
exported. That wrapper name is already captured as `wrapper_binding` on the
`module_server` definition row.

### Naming and UI pairing

The record is named by the **exported server function** (`counter_server`),
because that is the name the package's consumers call. Node `namespace` values
keep the graph's file-derived module identity (`R/counter_mod`), which stays
the internal coordinate — the two are related on the record via `module`.

The paired UI function is matched by stem: `<stem>_server` → `<stem>_ui`. This
is the dominant convention for packaged Shiny modules. When no such export
exists, `ui_fn` is `NA` and the module's UI-declared inputs are attributed only
where the server body references them.

### Qualified `moduleServer()`

Packaged modules commonly call `shiny::moduleServer(...)` rather than the bare
form, since a package imports rather than attaches Shiny. Both forms must
establish the module namespace in every walker. In the references walker the
module-server check therefore runs **before** the `pkg::fn()` branch, which
would otherwise consume the qualified call as a generic package reference and
walk the module body without its namespace — producing a module with no edges.
