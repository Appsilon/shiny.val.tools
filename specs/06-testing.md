# Test surface, scaffolding, and verification traceability

## Purpose

Defines how the package helps developers write tests for a Shiny app, and how the resulting tests become traceable verification evidence attached to each validated feature.

Resolves the overview open question "how does the package address the cost of writing tests" and defines three new artifacts, one new intermediate table, the `SVT-W301..W399` warning range, and four new public functions.

## The problem this solves

The feedback that motivated this spec: _producing validation artifacts is hard, and writing proper tests is just as hard._ Broken down, the difficulty is five distinct things:

1. **The blank page.** Given a feature, what is its test surface? Which inputs drive it, which outputs and reactives are the observable results, where does it stop?
2. **Harness boilerplate.** `testServer()` shape, module namespacing, `args = list(id = ...)`, sourcing the right files in the right order.
3. **Opaque results.** Some render functions produce a value that `testServer()` hands back verbatim but that nothing can usefully be compared against — `renderPlot` returns a display list, `renderUI` an HTML tag structure, `downloadHandler` a writer function. A developer discovers this only after writing the test.
4. **No completion signal.** Which features have tests? Which requirement does this test satisfy? An auditor asks for a traceability matrix; nobody has one.
5. **Silent drift.** The app grows a new input; the test still passes; it now covers less than it did. Nothing says so.

Every one of these is answerable from data the package already computes. The reactive graph knows the drivers and the results (1). The module contract and the app's import style know the harness shape (2). The subgraph knows which nodes are intermediate (3). The manifest holds `intended_use` and `risk_classification`, so a matrix needs only the test side (4). Node sets are content-addressed, so drift is a hash comparison (5).

## Position: the package does not write tests

The overview places **verification** out of scope and that stays true in the sense that matters: we do not assert behaviour. Refined position:

- **In scope:** deriving the test surface, generating harness scaffolds with the plumbing wired, discovering and mapping the tests that exist, and emitting verification-traceability evidence per feature.
- **Out of scope:** writing assertions, choosing expected values, running the app, running the tests, measuring line coverage.

This is the same split the package already applies to documentation: we emit a doc stub, a human writes the intended use. Here we emit a test stub, a human writes the expectation. The overview's rule that we emit **functional** evidence (this code is reachable from these controls) and never **numerical** evidence (given A and B, expect X) is unchanged — an expected value is a human judgment about the analysis, and a generated one would be worse than no test at all.

Consequently, **coverage in this spec means "exercised", never "correct"**. Every artifact says so in its header. A feature reported `covered` has tests that reach its roots; whether those tests assert the right thing is the reviewer's call.

## Three deliverables

| Deliverable          | Artifact                                    | Answers                                     |
| -------------------- | ------------------------------------------- | ------------------------------------------- |
| **Test surface**     | `<slug>/test_surface.json`, doc-stub section | "What is there to test in this feature?"     |
| **Scaffold**         | `<out_dir>/tests/test-svt-<slug>.R`          | "Where do I start?"                          |
| **Traceability**     | `traceability.{json,md}`, index section      | "What is covered, by which test, with what result?" |

Each is independently useful and independently switchable (`tests =` argument, spec 05). A team that already has a mature test suite wants only the third.

## Test surface model

One record per feature subgraph and per module subgraph, derived entirely from the slice:

```
TestSurface {
  name          : string                # feature or module identity
  kind          : enum {feature, module}
  surface_hash  : string                # md5 of the canonical surface serialization
  harness       : enum {testserver, unit}
  harness_reason: string                # why this harness was selected
  stimuli       : [Stimulus]            # what a test must set
  observables   : [Observable]          # what a test can assert on directly
  internals     : [Internal]            # intermediate nodes, observability annotated
  terminals     : [Terminal]            # trusted boundaries — module instances, rv entries
  helpers       : [Helper]              # app-defined functions called inside the subgraph
  blockers      : [warning_code]        # static-analysis limits that make the surface partial
}
```

Derivation, from the subgraph node set (spec 02) and nothing else:

- **stimuli** — `input` nodes in the closure. For a module surface these are namespace-local ids, set with `session$setInputs()`; for a feature they are top-level ids, set with `app$set_inputs()`. Each stimulus carries the roots it reaches (a one-line "drives: km_plot, km_table"), which is the single most useful thing the graph can tell a test author.
- **observables** — the subgraph roots (`output`, `observer`), plus, for a module, the contract's returned reactives (asserted via `session$returned()`). Each carries `def_call` — the render function (`renderPlot`, `renderText`, `renderDT`, `downloadHandler`, …), which drives the assertion hint and the opacity annotation below.
- **internals** — `reactive` and `value` nodes strictly between stimuli and roots. Each carries `observable_via`:
  - `direct` — inside `testServer()` a named reactive is callable by name (`my_reactive()`), so internals need no plumbing at all. Every internal in a `testserver` surface is `direct`; this is why the package generates nothing else.
  - `opaque` — the node's value exists under `testServer()` but is not a useful comparison target (see the opacity rule below). Emits SVT-W312.
- **terminals** — `module_instance` nodes (link to the already-validated module artifact per layered trust; a feature test does not re-test them) and top-level `reactiveValues` entries (a test must seed them, or drive them through the input that writes them).
- **helpers** — functions defined in the app's own enumerated source and called inside the subgraph, with `{file, line}` and a category (see "Helper categories" below). These are where analytical risk concentrates: a `method`-category helper is the highest-value unit test in the app and the cheapest to write, because it needs no Shiny harness at all.
- **blockers** — any of `SVT-W001` (dynamic input id), `SVT-W002` (`renderUI`-created input), `SVT-W003` (dynamic `reactiveValues` access), `SVT-W009` (metaprogramming) attached to a node in the closure. Their presence means the stimulus list is incomplete and a test built from it can pass while missing a code path. Surfaced as SVT-W308 on the surface record, not silently swallowed.

`surface_hash` is `md5()` of the canonical serialization: sorted stimuli ids, sorted observable names with their `def_call`, sorted internal names, sorted helper `file:name`, sorted blocker codes. Node ids are content-addressed (spec 01), so the hash changes exactly when the testable surface changes — not when a line moves.

## Harness selection

The package targets **`testthat` only**. Two harness values exist and a browser
harness is not one of them:

| Harness      | Emitted for                                  | Rationale                                                                         |
| ------------ | -------------------------------------------- | --------------------------------------------------------------------------------- |
| `testserver` | every feature and module surface             | `shiny::testServer()` drives the server function directly; internals are readable by name |
| `unit`       | every helper, independent of its surface     | An app-defined function needs no Shiny harness at all                              |

`harness_reason` records which of the two rules applied and, for a `testserver`
surface, whether any observable is `opaque`, so a reviewer can see at a glance
where a `testServer()` assertion will be structural rather than semantic.

### Why not `shinytest2`

The scope of this layer is **unit testing the server logic**: given these
inputs, does this reactive chain compute what it should. `shiny::testServer()`
answers exactly that question, in-process, with no browser, no Node, no
Chromote, and no screenshot baselines — which is what makes it runnable in a
locked-down GxP CI and reproducible years later when the validation package is
re-reviewed.

`shinytest2` answers a different question: does the assembled application still
*look and behave* as it did before. That is regression testing of the whole
app, its verification value rests on stored snapshots that are sensitive to
browser and font versions, and its failures are frequently not defects. Mixing
it into a generated scaffold would push teams toward brittle evidence for a
question this package never claimed to answer.

Consequently the package **generates** no `shinytest2` code and adds no
dependency on it. Existing `shinytest2` tests in an app are still discovered
and still map to features (spec 06 phase 3) — an app that has them gets credit
for them; we simply do not write new ones.

### The opacity rule

Some observables are readable under `testServer()` but not usefully
comparable. When a root's `def_call` is `renderPlot`, `renderImage`,
`renderUI`, `renderCachedPlot`, an htmlwidget renderer, or `downloadHandler`,
the observable is annotated `observable_via: opaque` and the surface emits
SVT-W312.

`opaque` is an annotation, never a harness switch. It tells the test author
what the honest assertion is: check that the value is produced and has the
expected class or structure, and move the semantic assertion down into the
helper that computed the data — which is exactly where the `unit` stubs
already point. A plot's pixels are not evidence about the analysis; the
`survfit` object behind it is.

## Scaffolding

`svt_scaffold_tests()` writes one file per surface plus one file per helper group. The generated code is a **harness with the plumbing filled and the assertions left blank**.

### The skip marker

Every generated `test_that()` block opens with:

```r
skip("SVT scaffold - assertions not written (SVT-W303)")
```

The marker is plain ASCII: generated files land in a package's test tree, where a non-ASCII string literal is a `R CMD check` finding the developer did not ask for. The coverage classifier keys on the `SVT scaffold` substring.

The alternatives are both worse: an empty block passes and manufactures false confidence; a failing placeholder blocks CI on day one and gets deleted. A skip is visible in `testthat` output, countable, non-blocking, and detectable by the coverage classifier — an unfilled scaffold is classified `scaffold`, never `covered`, so it cannot game the gate. Removing the `skip()` line is the developer's explicit act of taking ownership.

### Module scaffold — rhino flavor

```r
# Generated by shiny.val.tools 0.1.0 - scaffold; fill in the assertions.
# Module: app/view/mod_card    surface_hash: 4f1c9a2b
# @covers module: app/view/mod_card

box::use(
  shiny[testServer],
  testthat[test_that, expect_equal, skip],
)
box::use(app/view/mod_card)

test_that("app/view/mod_card — outputs respond to record_id", {
  skip("SVT scaffold - assertions not written (SVT-W303)")

  testServer(mod_card$server, args = list(id = "test", record = NULL), {
    # stimuli
    session$setInputs(
      record_id = NULL,   # drives: title, body
      expanded  = NULL    # drives: body
    )

    # observables — outputs
    # expect_equal(output$title, ...)   # renderText
    # expect_equal(output$body,  ...)   # renderText

    # internals — readable by name under testServer
    # expect_equal(selected_record(), ...)

    # returned reactives
    # expect_equal(session$returned(), ...)

    # terminals — trusted, see app--logic--fetch.md
  })
})
```

### The `args = list(...)` block

`args` names **every formal of the module's wrapper function**, taken from the Definitions table's `wrapper_formals` column: `id` seeded with a test namespace, the rest `NULL` for the developer to fill. A module whose server is `function(id, selected)` — one consuming another module's returned reactive — does not run from a scaffold that names only `id`, and getting that list right by hand is precisely the boilerplate this layer exists to remove.

### Module scaffold — traditional flavor

Same body; the header attaches the packages the module file's body uses (the app's `global.R` attaches them, and a directly-sourced file has no such preamble), then sources the defining file's dependencies before the file itself:

```r
library(shiny)
app_root <- testthat::test_path("..", "..")
source(file.path(app_root, "R/utils.R"))     # defines a helper this module calls
source(file.path(app_root, "R/mod_card.R"))
```

The dependency list comes from two places: the files the surface's **helpers** are defined in — a more precise answer than the Sources table alone, because the surface already knows which app-defined functions the subgraph calls — plus whatever the Sources table records the defining file as sourcing. It is best-effort and labelled as such in a generated comment; the developer completes it if the module reaches further. Getting this list right by hand is one of the concrete frictions in the feedback, and the graph already holds most of the answer.

### Package-target flavor

When the target is a source package that exports its modules (spec 03), neither `source()` nor `box::use()` applies: the header is `library(<Package>)` and the server expression is the exported wrapper function.

### Feature scaffold — `testServer` flavor

A feature is rooted in the top-level server, so the scaffold drives the app's
own server function. `shiny::testServer()` accepts an app directory as well as
a function, which is what makes a whole-app unit test possible without a
browser:

```r
# @covers feature: km_plot
test_that("feature km_plot — km_fit responds to arm and cutoff", {
  skip("SVT scaffold - assertions not written (SVT-W303)")

  shiny::testServer(testthat::test_path("..", ".."), {
    # stimuli
    session$setInputs(
      arm    = NULL,   # drives: km_plot
      cutoff = NULL    # drives: km_plot
    )

    # internals — readable by name under testServer
    # expect_equal(km_fit(), ...)

    # observables
    # output$km_plot is renderPlot — opaque (SVT-W312).
    # Assert that it is produced and check its structure; assert the
    # analysis itself on km_fit() above, or in the helper unit stub.
    # expect_s3_class(km_fit(), "survfit")
  })
})
```

The generated comment on an opaque observable always names where the real
assertion belongs. That redirection is the point: it moves the expectation off
the pixels and onto the computed object, which is both testable and the thing
the validation actually cares about.

### Helper scaffold

```r
# @covers fn: compute_km_fit
test_that("compute_km_fit", {
  skip("SVT scaffold - assertions not written (SVT-W303)")
  # Defined at R/km.R:10.
  # Category: method - declared in features.yml.
  # expect_equal(compute_km_fit(...), ...)
})
```

Helper stubs are ordered `method` first, then `framework`, then `utility`, then `unset`, then by name within a category so the order is deterministic. The ordering is the package's only opinion about what to test first, and it follows directly from the category model in spec 00.

### Helper categories

The category model in spec 00 is declared per *package*, and an app-defined
helper belongs to no package. The manifest's existing `package_categories`
block therefore accepts the reserved key `app`, which declares the category of
the feature's own source:

```yaml
features:
  - name: km_plot
    package_categories:
      survival: method
      app: method          # this feature's own helpers
```

Every helper of that feature takes the declared category; with no `app` key the
category is `unset` and the helper sorts last. `app` is reserved: a real
package named `app` would collide, and the manifest validator rejects it as a
package name for that reason.

A per-function override is deliberately not offered in v1. Categories exist to
express *how much validation burden this code carries*, and that judgment is
made at the feature level — the level at which intended use and risk are
already declared.

### Placement and overwrite rules

- **Default** `target = "staging"` — files go to `<out_dir>/tests/`, inside the lifecycle-managed tree (spec 05). Pristine files refresh on re-render; edited files are never rewritten. "Pristine" means the file's MD5 still matches the artifact manifest's record, so a scaffold the manifest does not know about is left alone too — we overwrite only what we can prove we generated. When a run skips an edited scaffold it re-records the **original** MD5, not the current one; recording the current one would make the file look pristine next run and the edit would be lost. The developer copies what they want into the app.
- **Opt-in** `target = "app"` — files go to the app's own test directory (`tests/testthat/`), and an existing path is **never** written: it is skipped with SVT-W309. Writing into a validated repository's test tree is not something to do by default.
- Filenames: `test-svt-<slug>.R` for surfaces (slug rule per spec 04), `test-svt-helpers-<slug>.R` for helper groups. The `svt-` infix keeps generated files greppable and separable from hand-written ones.

## Discovering the tests that exist

A sixth intermediate table, `tests`, built by `build_tests_table()` and memoized under the run-scoped cache like every other builder (spec 05).

Enumeration roots, none of which are part of the app graph (test files must never contribute nodes, edges, or inventory rows — they are not the application):

- `tests/testthat/**/*.R`
- `tests/**/*.R` (`setup-*.R`, `helper-*.R` included, flagged as support files)
- `tests/cypress/e2e/**/*.cy.js` — **comment scan only**, no JS parsing (the R-boundary non-goal stands)

Each `test_that()` call becomes one row: `{file, line, desc, harness, is_support, filled, targets, touched_inputs, touched_outputs, called_functions}`.

- `harness` is inferred from the calls in the block: `testServer` → `testserver`; `AppDriver$new` → `shinytest2`; neither → `unit`. The `shinytest2` value exists only for *discovered* tests — we never generate one — so an app that already has browser tests still gets verification credit for them.
- `filled` is `FALSE` while the block still contains an `SVT scaffold` skip marker.
- `touched_inputs` / `touched_outputs` come from `set_inputs()` / `setInputs()` argument names and `get_values()` / `get_value()` / `output$x` references — the same reference-walking machinery spec 01 already uses.

### Linking tests to the graph

Three mechanisms, highest priority first:

1. **Annotation.** A comment in the block's preceding comment run, or anywhere in a file's leading comment block (file-level annotations apply to every test in the file):

   ```r
   # @covers feature: km_plot, km_summary
   # @covers module: app/view/mod_card
   # @covers fn: compute_km_fit
   ```

   A comment, not a function call: it works in any framework, adds no runtime dependency on this package, is invisible to CI, and matches established practice — `valtools` links test code to test cases with a roxygen `@coverage` tag, `srr` links code to standards the same way. Unknown ids emit SVT-W305.

2. **Harness-target inference.** `testServer(mod_card$server, ...)` or `testServer(mod_card_server, ...)` resolves through the imports/definitions tables to a module identity. `AppDriver` blocks resolve to features by intersecting `touched_outputs` with feature roots. Helper stubs resolve to features by the functions they call.

3. **Unmatched.** The test maps to nothing → SVT-W304. Loop-generated tests, tests behind helper wrappers, and tests of code outside the graph all land here; the remediation named in the warning is a `@covers` annotation.

Inference is a convenience, annotation is the contract. The traceability artifact records which mechanism produced each link (`link: annotation | inference`) so a reviewer can see how much of the matrix is inferred.

### Manifest-side links

Evidence that lives outside the R test tree attaches through the manifest instead (see "Manifest extension"): a Cypress spec, a `valtools` test case id, a manual UAT record.

## Coverage classification

Per feature and per module, exactly one status:

| Status     | Definition                                                                                              |
| ---------- | ------------------------------------------------------------------------------------------------------- |
| `covered`  | Every observable is asserted or read by at least one filled mapped test, and at least one stimulus is set |
| `partial`  | Some observables or stimuli are exercised, not all. Unexercised members are listed                       |
| `scaffold` | Mapped tests exist but all are unfilled scaffolds                                                        |
| `uncovered`| No mapped test                                                                                          |
| `waived`   | Manifest declares `verification: not_required` with a rationale                                          |

Warnings: `uncovered` → SVT-W301, `partial` → SVT-W302, `scaffold` → SVT-W303. `waived` and `covered` emit nothing.

### Risk interaction

`risk_classification` (spec 02) modulates severity, not status:

- `high` + `uncovered` → SVT-W301 is emitted at **error** severity when `strict_verification = TRUE`, otherwise warn. This is the one place the package takes a position: a high-risk feature with no test is the defect worth failing a build over.
- `not_required` → no verification warning regardless of coverage. The rationale is already recorded; demanding tests for a feature the team documented as out of validation scope would be noise.

The `waived` status reuses the existing "documenting the decision not to validate is part of validation" principle from spec 00, applied to verification. A waiver without a rationale is fatal (SVT-W311), mirroring SVT-W105.

### Relationship to the unclaimed-output gate

The two completeness gates are duals and both belong in the index summary:

```
SVT-W103  behaviour the user can reach that no feature claims   → validation gap
SVT-W301  a claimed feature that no test exercises              → verification gap
```

An app with zero of both has a validated surface that is fully claimed and fully exercised. That pair of numbers is the headline an auditor wants and neither is meaningful alone.

## Staleness detection

`traceability.json` records each covering test's `md5` and each surface's `surface_hash`. On re-run, if a surface's hash changed while every covering test file's md5 did not, emit SVT-W307 ("test surface changed since covering test last modified") and list what changed (added stimuli, added or removed observables). This is the verification analogue of the "inventory changed since last regeneration" marker in spec 02, and it is the only automated answer to difficulty (5).

The check is deliberately conservative: an md5 change is not evidence the test was updated for the right reason, only that somebody touched it. The warning clears on any edit, and says so.

## Test result ingestion

Optional. `svt_test_coverage(results = <path>)` reads a test report the consumer's CI already produced — `testthat::JunitReporter()` XML, or a `testthat::ListReporter` RDS — and stamps `result` (`pass` / `fail` / `skip` / `missing`) onto each mapped test row. A mapped test with no entry in the report is `missing`; a `fail` or a `missing` emits SVT-W310.

We ingest, we never execute. Running the suite needs the app's data, credentials, and package library; that is CI's job, and staying out of it keeps the package's "static analysis only" contract intact. The result field is what turns a traceability matrix into verification *evidence* rather than a plan, so it is worth the one file-format dependency.

No wall-clock value is read from the environment — only fields present in the report — so the determinism contract holds.

## Artifacts

### `<slug>/test_surface.json`

Sibling of `inventory.json` in the per-subgraph directory. `schema_version: "1.0"`, stable within v1 under the same additive-only rule as `inventory.json`.

```json
{
  "feature": "km_plot",
  "kind": "feature",
  "schema_version": "1.0",
  "surface_hash": "4f1c9a2b...",
  "harness": "testserver",
  "harness_reason": "feature surface driven through shiny::testServer(); 1 observable is opaque",
  "stimuli": [
    {"name": "arm", "node_id": "...", "drives": ["km_plot"],
     "source_loc": {"file": "ui.R", "line": 18, "col": 5}, "warnings": []}
  ],
  "observables": [
    {"name": "km_plot", "kind": "output", "def_call": "renderPlot",
     "observable_via": "opaque", "source_loc": {"file": "R/km.R", "line": 40, "col": 3}}
  ],
  "internals": [
    {"name": "km_fit", "kind": "reactive", "observable_via": "direct",
     "source_loc": {"file": "R/km.R", "line": 30, "col": 1}}
  ],
  "terminals": [
    {"name": "app/view/filter_panel", "kind": "module_instance",
     "artifact": "app--view--filter_panel.md"}
  ],
  "helpers": [
    {"function": "compute_km_fit", "file": "R/km.R", "line": 10, "category": "method"}
  ],
  "blockers": ["SVT-W001"]
}
```

### `traceability.{json,md}`

One pair at the root of `out_dir`. `traceability.json`, `schema_version: "1.0"`:

```json
{
  "schema_version": "1.0",
  "inputs": {"test_path": "tests", "results": "ci/junit.xml"},
  "entries": [
    {
      "name": "km_plot", "kind": "feature",
      "intended_use_declared": true, "risk_classification": "high",
      "surface_hash": "4f1c9a2b...", "status": "partial",
      "tests": [
        {"file": "tests/testthat/test-km.R", "line": 12,
         "desc": "km plot renders for the ITT cohort", "harness": "testserver",
         "link": "annotation", "filled": true, "md5": "...", "result": "pass"}
      ],
      "unexercised": {"observables": ["km_summary_table"], "stimuli": ["cutoff"]},
      "warnings": ["SVT-W302"]
    }
  ],
  "orphan_tests": [
    {"file": "tests/testthat/test-misc.R", "line": 4, "desc": "...", "warnings": ["SVT-W304"]}
  ],
  "summary": {"covered": 3, "partial": 1, "scaffold": 0, "uncovered": 2, "waived": 1,
              "orphan_tests": 1}
}
```

`traceability.md` renders the same data as the auditor-facing matrix:

1. **Title + the exercised-not-correct disclaimer.**
2. **Summary** — the status counts, plus the W103/W301 gate pair.
3. **Matrix** — `| Feature | Intended use | Risk | Status | Tests | Result | Doc |`, one row per feature and module, sorted by name. Intended use is truncated; the Doc column links the per-feature stub.
4. **Gaps** — one row per `uncovered` / `partial` entry with what is missing, sorted by risk descending. This is the work list.
5. **Orphan tests** — the reverse view: tests that map to nothing.
6. **Reviewers** — non-auto sign-off block, merged across regenerations like every other stub.

Only `## Reviewers` is non-auto.

### Doc-stub sections

Per-feature and per-module stubs (spec 02) gain two auto-filled sections, placed after `## Warnings`:

```markdown
## Test surface
Harness: testserver (shiny::testServer(); 1 observable is opaque)

| Role       | Name          | Detail                        |
| ---------- | ------------- | ----------------------------- |
| stimulus   | arm           | drives km_plot                |
| observable | km_plot       | renderPlot — opaque           |
| internal   | km_fit        | readable by name              |
| terminal   | filter_panel  | validated separately          |
| helper     | compute_km_fit| method — R/km.R:10            |

## Test coverage
Status: partial. Tests: tests/testthat/test-km.R:12 (testserver, annotation, pass).
Not exercised: observable km_summary_table, stimulus cutoff.
```

### Index section

`index.md` (spec 04) gains **Verification coverage** after "Aggregate warnings": the status counts, the W103/W301 gate pair, and a link to `traceability.md`. `index.html` is unchanged — the architecture diagram stays about architecture.

## Manifest extension

Optional per-feature and per-module block in `features.yml`:

```yaml
features:
  - name: km_plot
    verification: required          # required (default) | not_required
    rationale_verification: ~       # required when verification = not_required
    tests:
      - file: tests/cypress/e2e/km.cy.js
        note: e2e — arm switch and plot render
      - external: UAT-014
        note: manual acceptance record, filed in the validation binder
      - external: valtools:Test_case_003
```

Semantics:

- `verification: not_required` + `rationale_verification` → status `waived`. Missing rationale → SVT-W311, fatal unless `lenient = TRUE`.
- `tests:` entries are additional links for evidence the R test scan cannot see. `file` entries are checked for existence (missing → SVT-W305); `external` entries are recorded verbatim and never checked.

In-test `@covers` annotations remain the primary link because they live next to the thing they describe. Manifest `tests:` is the escape hatch, not the main road.

## Public API

Added to spec 05's surface:

```r
surface  <- svt_test_surface(features, inventory)
coverage <- svt_test_coverage(surface, test_path = "tests", results = NULL)
paths    <- svt_scaffold_tests(surface, out_dir = "validation",
                               target = c("staging", "app"),
                               features = NULL)
svt_traceability(features, coverage, out_dir = "validation")
```

Pipeline position — between inventory and render, because the renderer consumes both new records:

```r
parsed    <- svt_parse(app_path)
graph     <- svt_build_graph(parsed)
features  <- svt_slice(graph, manifest = manifest)
inventory <- svt_inventory(graph, features)
surface   <- svt_test_surface(features, inventory)
coverage  <- svt_test_coverage(surface, test_path = "tests")
artifacts <- svt_render(features, inventory, out_dir = out_dir,
                        surface = surface, coverage = coverage)
```

`svt_validate()` gains:

- `tests = c("surface", "coverage", "off")` — how far to take the testing layer. Default `"coverage"` when a `tests/` directory exists, `"surface"` otherwise.
- `test_path = NULL` — where the app's tests live. `NULL` = `<app_path>/tests`.
- `test_results = NULL` — path to a CI test report to ingest.
- `scaffold = FALSE` — also write scaffolds. Off by default: generating files into a validated repo is opt-in, always.
- `strict_verification = FALSE` — escalate SVT-W301 on `high`-risk features to an error.

New classes `svt_test_surface` and `svt_test_coverage`, both with `print` and `summary` methods, both list-of-tibbles like their siblings. `svt_render(surface = NULL, coverage = NULL)` keeps the existing signature working: with both `NULL` the new doc-stub and index sections are omitted entirely rather than rendered empty.

## Required changes to earlier specs

- **Spec 01** — the Definitions table gains a `def_call` column (the RHS call name: `renderPlot`, `eventReactive`, `downloadHandler`, …; `classify_rhs()` already computes the kind from it) and `kind = "function"` rows for top-level function definitions, which is where `helpers` comes from. A sixth intermediate table, `tests`, is added; it is built from the test tree and never contributes to nodes, edges, or inventory.
- **Spec 02** — manifest gains `verification`, `rationale_verification`, `tests`; doc stub gains the two auto sections; the coverage gate is documented alongside the unclaimed-output gate.
- **Spec 03** — the function inventory's category data feeds helper-stub ordering. No change to `inventory.json`.
- **Spec 04** — `index.md` gains the Verification coverage section; `traceability.{json,md}` join the artifact set.
- **Spec 05** — the four new functions, the new `svt_validate()` arguments, and the lifecycle treatment of `<out_dir>/tests/` and `<slug>/test_surface.json` as tracked artifacts.

## Determinism

Same source + same manifest + same test tree + same results file → byte-identical artifacts. Surfaces are sorted by name; within a surface, every list is sorted; `surface_hash` is content-derived; scaffold test names are derived from the feature name and a positional index, never a timestamp; ingested results contribute only fields read from the report. No wall-clock value enters any testing artifact.

## Declared limitations

Each is surfaced in the artifact that could be misread without it:

- **Coverage means exercised, not correct.** Stated in every artifact header.
- **Inference is heuristic.** Loop-generated tests, wrapper helpers, and parameterised suites will not map without a `@covers` annotation. SVT-W304 counts them.
- **The stimulus list inherits every graph limitation.** Dynamic ids, `renderUI`-created inputs, and metaprogramming (SVT-W001/W002/W009) mean a surface can be incomplete; SVT-W308 says so per surface.
- **No JS parsing.** Cypress specs contribute only through `@covers` comments or manifest entries.
- **No line coverage.** `covr` measures lines; we map tests to features. The two are complementary and we do not attempt the former.
- **Opaque observables are not asserted for us.** A `renderPlot` output can be checked for existence and class under `testServer()`; its appearance cannot. SVT-W312 says so per observable and points at the helper where the real assertion belongs. Teams that want appearance regression evidence need a browser tool; this package does not generate one.
- **Scaffolds are starting points.** A generated harness compiles and runs; it asserts nothing until a human fills it in, and the `skip()` marker makes that state visible rather than comfortable.

## Interoperability

- **`valtools`** — the PHUSE validation framework organises requirements, test cases, and test code with roxygen tags, and scrapes a coverage matrix from `@coverage`. The mapping is direct: our `intended_use` is their requirement, our test surface is the input to their test case, our `@covers` id is their `@coverage` target. `traceability.json` is the interchange point; a `tests: - external: valtools:Test_case_003` entry links the two by id. We do not reimplement their report.
- **`rhino`** — generated scaffolds land where `rhino::test_r()` already looks (`tests/testthat/`) and use `box::use()` headers in box-based apps. Cypress specs under `tests/cypress/` link by comment only.
- **`shiny::testServer`** — the only harness we generate, and `testthat` the only framework. We add no runtime dependency: generated files import `shiny` and `testthat` directly and both stay in `Suggests:`. We take no dependency on `shinytest2` at all; see "Why not `shinytest2`".
- **`riskmetric` / `val.meter`** — unchanged. Package risk stays out of scope; the new artifacts describe the app's own verification, not its dependencies'.

## Warning codes (range: SVT-W301 to SVT-W399)

- **SVT-W301** — Feature or module has no mapped test (verification gap)
- **SVT-W302** — Partial coverage: some observables or stimuli unexercised
- **SVT-W303** — Scaffold present but not filled in (skip marker still there)
- **SVT-W304** — Orphan test: maps to no feature, module, or helper
- **SVT-W305** — `@covers` annotation or manifest `tests:` entry references an unknown target
- **SVT-W306** — *(retired, never emitted)* Intermediate reactive not observable under the selected browser harness. Allocated when the design still generated `shinytest2` scaffolds. The code is retired rather than reused: per the stability contract a retired identifier is never given a new meaning. Its replacement is SVT-W312.
- **SVT-W307** — Test surface changed since the covering test was last modified (possibly stale)
- **SVT-W308** — Test surface incomplete: static-analysis blockers in the subgraph
- **SVT-W309** — Scaffold target already exists in the app test tree; skipped, not overwritten
- **SVT-W310** — Mapped test failed or is absent from the ingested result report
- **SVT-W311** — `verification: not_required` without `rationale_verification`
- **SVT-W312** — Observable is opaque under `testServer()` (`renderPlot`, `renderUI`, `downloadHandler`, …); assert its structure and move the semantic assertion into the helper that computed it

## Implementation order

Each phase is independently shippable and testable, in the TDD order the project uses.

1. **Surface derivation** (read-only) — *implemented*. `def_call` + `kind = "function"` rows in the Definitions table, `TestSurface` construction, harness selection, `surface_hash`, `test_surface.json`, the `## Test surface` doc-stub section. Warnings: SVT-W308, SVT-W312.
2. **Scaffolding** — *implemented*. Templates per flavor (box, traditional, package target), staging placement, never-overwrite. Warnings: SVT-W303, SVT-W309.
3. **Discovery and coverage.** `build_tests_table()`, annotation parsing, inference, classification, `traceability.{json,md}`, the index section, manifest extension. Warnings: SVT-W301, W302, W304, W305, W311.
4. **Results and staleness.** JUnit ingestion, md5 recording, drift detection, `strict_verification`. Warnings: SVT-W307, SVT-W310.

### Fixtures

Per the project's fixture-before-test rule, added under `inst/extdata/`:

- `traditional_tested/` — `source()`-based app with `tests/testthat/` containing one annotated test, one inferrable test, one orphan test, and one unfilled scaffold.
- `rhino_tested/` — box-based app with a module test using `testServer(mod$server)` and a Cypress spec carrying a `@covers` comment.
- `untested_app/` — reuse of `traditional_basic` with no `tests/` directory, for the `uncovered` path and the `tests = "surface"` default.

Determinism tests: two runs over `rhino_tested/` produce identical `test_surface.json`, `traceability.json`, and scaffolds; a stimulus added to a fixture module changes exactly that module's `surface_hash`.

## Out of scope for this spec

- Generating assertions or expected values — permanently out, per the overview.
- Running the app or the test suite.
- Line or expression coverage (`covr`'s job).
- Generating `shinytest2` / browser / snapshot tests of any kind.
- Property-based or fuzz input generation.
- Generating Cypress specs, or parsing JS beyond `@covers` comments.
- Reconciling coverage across app versions — the no-diff non-goal stands.
