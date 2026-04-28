# Progress

Status snapshot of `shiny.val.tools` against the five specs in `specs/`.
Update on significant change; stale by design otherwise.

## Snapshot

- **Tests:** 449 passing, 0 failing
- **R CMD check:** clean (only the environmental clock-verification NOTE)
- **Specs touched:** 01 nearly complete (structural core + module instances + 8 of 10 warning codes; References table now also tracks `:::` access and `alias$fn` calls for spec 03); 02 fully implemented (slicer + manifest + module subgraphs + doc stub merge + SVT-W101..W105) and the doc stub now embeds the spec-03 inventory tables; 03 first slice in (function-origin resolver + direct/transitive classification + per-feature inventory + `inventory.json` v1.0 emission + SVT-W201..W206); 04 first slice in (visNetwork widget per feature/module + node styling + tooltips + warning badges + hierarchical / forceAtlas2 fallback heuristic); 05 first slice in (full `svt_*` public surface + `svt_validate()` orchestrator + S3 print/summary methods)
- **Fixtures:** `traditional_basic`, `traditional_with_source`, `cycle_app`, `box_in_R`, `rhino_basic`

## Spec 01 — Graph model and AST extraction

The five intermediate tables: **Files**, **Imports**, **Sources**, **Definitions**, **References** → Nodes + Edges. Definitions and References are the structural core; until they exist, no graph.

### File enumeration

- [x] Starting set (`app.R`, `ui.R`, `server.R`, `global.R`, `R/**/*.R`)
- [x] `source(path)` following (relative-to-calling-file, cycle prevention)
- [x] `box::use(./...)` / `box::use(../...)` following
- [x] `box::use(seg/seg/...)` absolute paths via `options(box.path = ...)`
- [x] Rhino bootstrap: `.Rprofile` and `app/main.R` added to starting set when present
- [x] `discover_box_path()` parses `.Rprofile` for `options(box.path = getwd())`
- [x] `chdir = TRUE` argument to `source()` honored — default (FALSE) resolves relative to app root, TRUE relative to file dir

### Imports table

- [x] `library()` / `require()` detection with `inside_function` flag
- [x] `box::use()` clauses — all 7 forms in spec 01 table
- [x] Trailing commas tolerated; `./mod[fn]` precedence; `pkg[...]` whole-namespace flag
- [x] Conditional context flag (foundation for SVT-W004)
- [x] Absolute flag (TRUE for `app/view/home`, FALSE for `./mod`/`../mod`, NA for package imports)
- [ ] `#' @export` extraction (foundation for module contracts)

### Sources table

- [x] Static `source()` calls collected, paths resolved, conditional flag set

### Definitions table

- [x] `output$x <- ...` and `output[["x"]] <- ...` assignments
- [x] Named reactive bindings (`name <- reactive(...)`, `eventReactive`, `bindEvent(reactive(...), ...)`)
- [x] Named observer assignments (`observe(...)`, `observeEvent(...)`, `downloadHandler(...)`, `bindEvent(observe(...), ...)`)
- [x] `reactiveValues(name = ...)` declarations and subsequent named writes (`rv$name <-`, `rv[["name"]] <-`)
- [x] `moduleServer(id, ...)` definition sites — both wrapper-function form and legacy `function(input, output, session)` form
- [x] Output reassignment → SVT-W010

### References table

- [x] `input$x` and `input[["x"]]` reads
- [x] Bare-name calls — foundation for named-reactive / observer reads (`total()` etc.)
- [x] `rv$name` reads (best-effort, only when `rv` is a known reactiveValues container; foundation for SVT-W003)
- [x] Function calls resolvable to `pkg::fn` (foundation for spec 03 inventory)
- [x] `pkg:::fn` access tracked via the `internal` column (foundation for SVT-W201)
- [x] `alias$fn` calls captured with the alias in `container` (foundation for `box::use(alias = pkg)` resolution)

### Nodes and Edges

- [x] Node construction from Definitions (output / reactive / observer / value)
- [x] Input nodes from References (no syntactic Definition for inputs)
- [x] Edge construction from References × Nodes, scoped via `in_def_*` (the enclosing definition that owns each read)
- [x] Stable, content-addressed node IDs (`<type>:<namespace>:<container>:<name>`)
- [x] `source_loc` (`file`, `line`, `col`) on every node and edge
- [x] Multiple references collapse into one edge; first occurrence wins for `source_loc`
- [x] SVT-W010 attached to the canonical output node (warnings list-column)

### Module namespacing

- [x] `moduleServer(id, function(input, output, session) ...)` walk with namespaced inputs / outputs / reactives
- [x] Legacy `name <- function(input, output, session, ...) {...}` form recognized as a module
- [x] One `module_instance` node per call site (parent-side instantiation, with concrete ns_id)
- [ ] `NS(id)` / `session$ns()` recognized at the syntactic level (refines namespace; current detection is structural)

### Hybrid-app overlap rules — **not started**

- [ ] In-file `library(pkg)` + `box::use(pkg[fn])` overlap → SVT-W006
- [ ] `source("foo.R")` + `box::use(./foo)` reaching the same file → SVT-W007

### Warning codes — emission status

- [x] SVT-W001 — Dynamic input ID (`input[[expr]]` with non-literal key)
- [ ] SVT-W002 — `renderUI` / `insertUI` source for input definition
- [x] SVT-W003 — Named access into `reactiveValues()` (one warning per `rv$x` read)
- [x] SVT-W004 — Conditional `source()` / `box::use()` inclusion
- [x] SVT-W005 — Whole-namespace box import (`pkg[...]`)
- [ ] SVT-W006 — `library`/`box::use` overlap with non-imported reference (per-call-site; needs inventory pass)
- [x] SVT-W007 — Duplicate inclusion via `source()` and `box::use()`
- [x] SVT-W008 — `library()` / `require()` inside a function body
- [x] SVT-W009 — Metaprogramming construct (`do.call`, `eval`, `evalq`, `parse`, `Recall`, `match.call`, `sys.call`)
- [x] SVT-W010 — Output reassigned

## Spec 02 — Feature subgraphs and the manifest

- [x] Default slicing rule (one feature per top-level output / side-effecting observer) — `default_slice()` in `R/slicing.R`
- [x] Transitive upstream closure (`upstream_closure()`); inputs / values / module instances are natural terminals because they have no outgoing edges in the current model
- [x] `features.yml` schema (load + parse) — `read_manifest()` in `R/manifest.R`, depends on `yaml` (now in Imports)
- [x] Manifest validation — `validate_manifest()` covers unknown root (W101), conflicting roots (W102), name uniqueness across features+modules (fatal collision row), `not_required` without `rationale` (W105)
- [x] Module subgraphs rooted at namespaced outputs/observers + returned reactives — `module_slice()` in `R/modules.R`
- [x] Module-contract auto-extraction (inputs / outputs / returned reactives); returned reactives detected via the value-position of the moduleServer body (named `list(...)` and bare-name returns; anonymous returns deferred)
- [x] Doc stub markdown emission with merged human / auto-filled sections — `render_doc_stub()`, `merge_doc_stub()`, `write_doc_stub()` in `R/doc_stub.R`
- [x] Unclaimed-output completeness check — `detect_unclaimed_outputs()` in `R/manifest.R`, fires only when an explicit manifest is supplied per spec
- [x] Regeneration semantics — auto-filled sections refreshed, human-authored sections (Reviewers, plus any unknown sections) preserved verbatim
- [x] SVT-W101..W105 messages registered in `warning_message()`. SVT-W104 detector wired (`detect_orphan_module_instances()`); orphans are impossible by construction today since `module_instance` nodes are only emitted for known wrappers, but the detector exists for symmetry with the spec
- [x] End-to-end emission orchestrator — `svt_validate()` (spec 05) composes `apply_manifest()` + `module_slice()` + `write_doc_stub()` + `write_inventory_json()`
- [x] Doc-stub Functions called / Packages used sections render the spec-03 inventory as markdown tables (placeholder text removed)
- [ ] **Open:** Inventory diff marker (footer "inventory changed since last regeneration") — `inventory.json` is now emittable, so this becomes a comparison against the previously written file.

## Spec 03 — Per-feature inventory

- [x] FunctionCall model + per-feature emission — `build_inventory()` + `build_feature_inventory()` in `R/inventory.R`; per-site rows are collated into one row per `(package, fn)` with a `call_sites` list-column per spec 03 schema
- [x] Derived PackageUse view — `derive_package_view()` (mixed when categories differ)
- [x] Function-origin resolution priority chain — `resolve_function_origins()`: explicit `pkg::fn`/`pkg:::fn`, box-declared aliased (`alias$fn`), box-declared function set (`box::use(pkg[fn])`), library walk via `getNamespaceExports()`, ambiguous, unresolved
- [x] Direct vs transitive flag — `direct_packages()` covers library/require/box::use plus DESCRIPTION Imports/Depends; transitive = anything else; SVT-W204 fires per call when the resolved package isn't direct (no `pak::pkg_deps` walk needed for the warning, deferred until a stronger transitive-set use case lands)
- [x] Category support — manifest `package_categories` flows in via `feature$package_categories`; default `unset`; SVT-W205 fires for any non-trivially used package without a category. Doc-stub readback (priority 3 per spec) deferred — manifest is the only declared source today
- [x] `inventory.json` v1 emission — `render_inventory_json()` + `write_inventory_json()`; schema_version 1.0; deterministic key ordering; hand-rolled serializer (no jsonlite dep)
- [x] `renv::lockfile_read()` integration — `read_lockfile_versions()`; `renv` is in Suggests; missing lockfile or installed copy emits SVT-W206
- [x] SVT-W201..W206 messages registered in `warning_message()`; W201/W202/W203/W204 emitted per call site, W205 per package, W206 per missing version

### Open spec 03 items

- [ ] `pak::pkg_deps()` integration to enumerate the transitive dependency closure when callers want it as a discrete set (the W204 warning works without it, but spec mentions pak as the reference)
- [ ] Doc-stub readback for category data (priority 3) — round-trips developer-authored categories from a previous regeneration

## Spec 04 — visNetwork rendering

- [x] Per-feature / per-module HTML widget at `validation/<name>.html` — `write_feature_html()` in `R/render.R`; uses `visNetwork::visSave(..., selfcontained = TRUE)`
- [x] Node styling table (shapes/colors per spec) — `node_style_table()`; six types covered
- [x] Tooltip with FQ name, source location (`file:line`), type, warning codes — assembled in `build_vis_nodes()` with `htmltools::htmlEscape()`
- [x] Hierarchical LR layout (`visHierarchicalLayout(direction = "LR", sortMethod = "directed")`); `forceAtlas2Based` fallback via `choose_solver()` (density heuristic since vis.js's overlap is a runtime concept). Solver and reason logged in the widget footer
- [x] Module-instance single-node rendering with click-through — `module_instance` rows carry a `url` field; `visEvents(selectNode)` opens `<name>.html` in a new tab
- [x] Warning badges on nodes — appended to label as `(⚠N)` when the node carries warnings
- [x] Header (name, risk, truncated intended use) and footer (layout solver + reason, total warning count, doc + inventory links, optional short commit hash)
- [ ] Determinism (seeded layout from content-addressed IDs) — node/edge data is sorted, no timestamps emitted, but vis.js's stabilization step runs in JS at render time. Byte-identical output is not yet asserted by a test
- [ ] `cli` messaging — `R/api.R` still uses base `message()` / `warning()` (spec 05 follow-up)

## Spec 05 — Public API

- [x] `svt_validate()` top-level — composes parse / graph / slice / inventory / render
- [x] Step-by-step: `svt_parse`, `svt_build_graph`, `svt_slice`, `svt_inventory`, `svt_render`
- [x] Inspection: `svt_summary`, `svt_warnings`, `svt_unclaimed`, `svt_inspect` (lobstr fall-through to base print)
- [x] Manifest helpers: `svt_manifest_template`, `svt_manifest_validate`
- [x] Return classes (`svt_parsed`, `svt_graph`, `svt_features`, `svt_inventory`, `svt_validation`) with `print` / `summary` methods
- [x] `lenient = TRUE` downgrades manifest abort to a warning per spec
- [x] `app_path` accepts a `.zip` archive (auto-extracted to a session tempdir)
- [x] `features = …` / `modules = …` selectors filter the rendered set
- [ ] `cli` messaging (info / warn / error) — currently uses base `message()` / `warning()`; promote when adding `cli` to Imports
- [ ] Determinism contract test (same input → byte-identical output) — needs spec 04 visNetwork artifact to be byte-stable end-to-end

## Stability contracts — status

- [x] `inventory.json` schema_version 1.0 — emitted by `render_inventory_json()`; deterministic key ordering; the hand-rolled serializer is the only writer so the on-disk shape is fully under our control
- [x] Warning code ranges — SVT-W001..W010 (graph), SVT-W101..W105 (manifest), SVT-W201..W206 (inventory) all wired through `warning_message()`; W002 and W006 still pending detectors
- [ ] Determinism test in suite — needs spec 04/05 artifact pipeline to compare byte-identical outputs end-to-end

## Suggested next slice

All five specs have a first-slice implementation; what remains is hardening and the spec-04/spec-05 follow-ups.

1. **Determinism contract test.** Write a test that runs `svt_validate()` twice on a fixture (and on a fresh tempdir) and compares each emitted file byte-for-byte. The hand-rolled JSON serializer and sorted doc stub are already byte-stable. The visNetwork HTML is the open piece — `htmlwidgets::saveWidget()` may embed timestamps and tempfile paths. If the diff is non-empty, decide whether to post-process the widget output, pin a fixed seed, or relax the contract to "all artifacts except the widget".
2. **Promote messaging to `cli`.** Add `cli` to Imports and replace the `message()` / `warning()` calls in `R/api.R` with `cli_inform()` / `cli_warn()`. Small, mechanical.
3. **"Inventory changed since last regeneration" diff marker.** Compare the freshly rendered `inventory.json` against the previously written file; on a delta, append a footer line to the doc stub. The diff need not be granular — a simple "inventory changed" boolean is what spec 02 calls for.
4. **Spec 01 catch-up codes.** SVT-W002 (renderUI/insertUI source for input definition), SVT-W006 (library/box::use overlap with non-imported reference), `NS(id)` / `session$ns()` recognition, and `#' @export` extraction for richer module contracts.

Remaining spec 01 items can land opportunistically:

- `#' @export` extraction (foundation for richer module contracts)
- SVT-W002 (renderUI / insertUI source)
- SVT-W006 (library / box::use overlap with non-imported reference)
- `NS(id)` / `session$ns()` recognition

Spec 02 follow-ups:

- "Inventory changed since last regeneration" diff marker — `inventory.json` is now emittable, so this becomes a comparison against the previously written file.

Spec 03 follow-ups:

- `pak::pkg_deps()` for an explicit transitive-set view (W204 already works without it).
- Doc-stub readback as a category source (priority 3 per spec).

Spec 05 follow-ups:

- Determinism contract test — see item 2 above.
- `cli` messaging — see item 3 above.
