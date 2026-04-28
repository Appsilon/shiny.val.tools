# shiny.val.tools — Overview

## What this package is

`shiny.val.tools` is an R package that takes a Shiny app (a path or a zip), parses its R source statically, and produces **per-feature validation artifacts** for use in pharma/GxP validation packages.

The unit of validation is a **feature subgraph**: the closed reactive subgraph rooted at one or more outputs. For each feature subgraph, the package emits:

1. The subgraph itself (interactive `visNetwork` HTML widget).
2. The list of R packages used inside that subgraph.
3. The list of functions/methods called inside that subgraph (with package origin).
4. A documentation stub for the developer to fill in (intended use, risk assessment).

These artifacts are inputs to a human validation process. **The package does not validate the app. It produces evidence for someone else to validate.**

## Why this exists when `reactlog` exists

`reactlog` instruments a _running_ Shiny app and records the reactive graph at runtime. It is excellent for debugging and is too granular for documented validation:

- It requires running the app — credentials, data, side effects, infrastructure.
- It only captures code paths that fired during the trace, missing branches.
- Its graph is a flat, global, low-level view — not feature-scoped.
- Output is debugging-oriented, not auditor-oriented.

`shiny.val.tools` is **static, output-rooted, and feature-scoped**. It runs at parse time (no app launch), produces narrow subgraphs centered on each documentable feature, and emits artifacts in a form auditors can attach to a validation package.

## Why this exists when `riskmetric` / `val.meter` exist

Those packages assess **R package** quality and risk. They are out of scope and complementary. `shiny.val.tools` produces a package list per feature; the consumer feeds that list into `riskmetric` / `val.meter` separately.

We do not validate packages, score functions, or recommend alternatives. We list and trace.

## Core concepts

### Reactive graph

A directed graph derived from the app's R source. Node types:

- **input** — a reference to `input$x` (or `ns("x")` inside a module).
- **output** — an assignment to `output$y`.
- **reactive** — a named binding to `reactive(...)`, `eventReactive(...)`, or `bindEvent(reactive(...), ...)`.
- **observer** — `observe(...)`, `observeEvent(...)`, or any call producing side effects (e.g. `downloadHandler`, writes).
- **value** — entries in `reactiveValues(...)`, where statically resolvable.
- **module instance** — a `moduleServer(...)` instantiation in a parent (treated as a black-box node carrying its I/O contract; see below).

Edges are **depends-on** relationships: an edge from A to B means A reads B inside its body.

### Feature

A **feature** is a named, closed subgraph rooted at one or more outputs (and/or side-effecting observers). The default is one-output-per-feature. Developers may declare groupings explicitly via a manifest file (see [02-feature-subgraphs.md](02-feature-subgraphs.md)).

A feature subgraph is the transitive upstream closure of its roots, terminating at:

- Inputs (graph leaves on the input side)
- Module instance boundaries (the module is opaque from the parent's view)
- Top-level reactive values

### Module contract

A module's validation surface is its **contract**, not its internals:

- The set of UI input IDs it owns (under its namespace)
- The set of `output$x` it owns
- The set of reactive values it returns from its server function

In a parent feature subgraph, a module instance is a single node carrying that contract. The module's own internals are validated separately as a **module subgraph** (treated like a feature, but rooted at the module's outputs and return values).

A module validated once is trusted everywhere it is instantiated.

### Source and import resolution

Shiny apps bring code into scope in two distinct ways, and the package handles both — including hybrid apps that mix them, which are common in real codebases.

**Traditional Shiny:** `library(pkg)` brings a package into the global namespace; `source("path/to/file.R")` pulls in another R file.

**Rhino (`box`-based):** `box::use(...)` is per-file and replaces both. It declares package imports with explicit function-set narrowing, and resolves local files by relative path:

```r
box::use(
  shiny[...],              # whole-namespace
  dplyr[filter, mutate],   # explicit function set — tightest call surface
  d = dplyr,               # aliased; later calls look like d$something()
  ./modules/foo,           # local module via relative path
  ./utils[helper_fn],
)
```

Box matters for two reasons beyond simply supporting rhino:

1. **Tighter, more honest inventory.** With `library(dplyr)`, the static call surface is everything in `dplyr`; we narrow only by walking call sites. With `box::use(dplyr[filter, mutate])`, the import statement _declares_ the call surface — no narrowing required. The function-centric inventory becomes more precise wherever box is used.
2. **Per-file dependency declarations.** Every R file that uses box owns its own `box::use()` block, giving us per-file dependency boundaries — a stronger signal than a single global `library()` at the top of the app.

A **box module** (an R file with `#' @export` declarations) is not the same as a **Shiny module** (a `moduleServer()` UI/server pair). Rhino apps usually colocate one of each per file, but they are distinct concepts and the graph model tracks them as such even when they coincide. Box's `#' @export` declarations are parsed directly to give us module-contract data without inference.

## Validation framing

These definitions are used throughout the specs and are drawn from the conceptual sources listed in [References](#references). Pharma teams routinely conflate these terms; the specs do not.

- **Verification** — "did we build it correctly?" Adherence to the SDLC: unit tests, integration tests, code review, `R CMD check`. The technical proof that the code matches its specifications.
- **Validation** — "did we build the right thing?" Evidence that the tool works for its **intended use**, traceable to user-facing requirements.
- **Testing** — the method used to demonstrate either of the above.

`shiny.val.tools` supports **validation**. It produces traceable evidence that links:

```
intended use → feature → reactive subgraph → packages + functions used
```

Verification is **out of scope**. The developer's existing unit tests, `R CMD check`, `shinytest2`, and the `riskmetric` / `val.meter` ecosystem cover that surface. We add the layer above.

### Layered trust

Validation works in layers; each layer trusts the layers below and validates only what is built on top:

```
1. R + base packages           ← trusted (we don't validate that median() calculates the median)
2. declared dependencies       ← validated by riskmetric / val.meter (out of scope here)
3. Shiny modules (reusable)    ← validated once, contracts trusted everywhere (this package)
4. features (output-rooted)    ← validated against intended use (this package)
5. the app                     ← a composition of validated features
```

A module validated standalone is trusted everywhere it is instantiated. A package vetted by `riskmetric` is trusted by every feature subgraph that uses it. **Validation does not propagate downward** — each layer is responsible only for what it adds.

This principle is the justification for the module-contract design and for why we do not re-validate `dplyr` every time a feature uses `dplyr::filter`.

### Intended use determines risk, not the function

The same function used in two different features can carry different risk depending on what each feature is **for**. `dplyr::filter` used to "explore raw data" is low risk; `dplyr::filter` used to "compute the primary endpoint cohort" is high risk. The risk lives in the intended use of the surrounding feature, not in the function itself.

The package therefore associates **functions and packages with the feature subgraph that uses them**, not with the app globally. Each feature's intended-use declaration localizes the risk of every function it transitively calls. This is the principal reason a per-feature inventory matters more than an app-level one — and the reason this package exists as something distinct from `riskmetric`.

### Validated subgraph = positive declaration of validated behavior

A feature subgraph is an **explicit declaration** of what behavior is validated. The union of all declared feature subgraphs is the entire validated surface of the app. Anything a user can do in the app that does not appear in any declared subgraph is, by construction, **not validated behavior** — it may still work, but it carries no validation evidence and should not be relied on for regulated decisions.

Auditors can therefore inspect the manifest and the rendered subgraphs and answer: _what is the validated surface of this app, and what is outside it?_

### The function call surface is the validation surface, not the package list

A package list says "this app loads `dplyr`." It does not say which functions of `dplyr` were called, or whether `dplyr::filter` was the entry point or `dplyr::group_by`, or whether the call came via a direct import or because `dplyr` was pulled in as a transitive dependency and a developer (or AI tool) reached for it anyway.

Pharma practice has historically validated against the package list. In real usage, "Imports" are called directly — the binary "Intended for Use vs Imports" classification leaks. The validation surface that matters is the **set of functions actually invoked**, not the set of packages declared.

The package therefore models the inventory as **function-centric**: the per-feature inventory is the set of `pkg::fn` calls reachable from the feature's roots. The package list is _derived_ from this function set, not the other way around. Consumers who want to operate at the package layer can still do so (we provide both views), but the function-level data is the canonical artifact.

### Direct vs transitive dependencies

The function-centric model still benefits from a pragmatic filter: pharma teams often choose to focus validation effort on the app's **direct imports**, trusting that if a direct import functions correctly, its dependencies are absorbed into that validation. The package distinguishes direct from transitive in every per-feature inventory, but it does not collapse the data — the consumer chooses how deep to go.

This composes cleanly with the function-centric model: every reported function carries both its package origin and a flag indicating whether that package was a direct or transitive import of the app.

### Package categories (Utility / Framework / Method)

Beyond direct/transitive, packages and functions can be classified by what kind of role they play, which determines how much validation rigor is appropriate:

- **Utility** — convenience or infrastructure (loggers, formatters, plumbing). User-visible output; if wrong, user notices and works around. Low validation burden.
- **Framework** — requires direct user input to function (e.g., `dplyr` — the user supplies the verbs). User can correct through input. Low-to-medium burden.
- **Method** — implements analytical methods with limited or no user input (e.g., a specific survival model fit). User cannot easily correct the output. **High validation burden** — this is where extensive testing actually matters.

The category depends on the **use context**, not the package itself. The same package can sit in different categories in different features. The package therefore does not auto-classify; it provides a field in the per-feature doc stub for the developer to declare. Classification is a human judgment call, but the doc stub structures the conversation.

## Pipeline

```
1. Ingest          path or zip → unzipped app root
                   → enumerate R files (R/, server.R, ui.R, app.R)
                   → follow source() and box::use(./...) to reach all
                     transitively included files

2. Parse           AST per file
                   → resolve library() calls, source() targets, and
                     box::use() blocks (packages, function sets, aliases,
                     local module paths, #' @export declarations)
                   → resolve Shiny module definitions and namespaces

3. Build graph     walk ASTs → emit nodes and edges
                   → flag unresolved references as warnings

4. Slice           apply features.yml manifest (if present) or default
                   one-feature-per-output rule
                   → produce one feature subgraph per declared feature
                   → produce one module subgraph per module definition

5. Inventory       per subgraph: collect functions called and packages used
                   → where box::use() is present, use its declared imports
                     as ground truth for the call surface
                   → where library() is used, narrow via AST call-site walk
                   (defer package risk scoring to riskmetric/val.meter)

6. Render          per subgraph: emit visNetwork HTML widget
                   → emit doc stub (markdown) listing packages, functions,
                     warnings, and a placeholder for intended use
```

Each step is exposed as a public function so a user can drive the pipeline programmatically or end-to-end.

## What is in v1

- Static parsing of `app.R`, `ui.R`, `server.R`, and `R/*.R`.
- **Traditional Shiny support (first-class):** `library()` and `require()` calls, plus `source()` resolution — relative paths, `chdir = TRUE`, and recursive following. Call surface determined by AST call-site walk across loaded namespaces.
- **Rhino / `box` support (first-class):** full `box::use(...)` parsing including package imports, explicit function-set narrowing, aliases, local module paths (`./...`, `../...`), and `#' @export` declaration extraction. Where the function set is explicitly declared, it is used as the call surface directly.
- **Hybrid apps** mixing `library()`, `source()`, and `box::use()` are supported. Neither style is a fallback for the other — both are first-class throughout the pipeline.
- Reactive graph construction with the node/edge types listed above.
- Shiny module support (`moduleServer`, `callModule`, `NS()`), including module contract extraction. Where a Shiny module lives in a box module file, the `#' @export` data refines the contract directly.
- Default one-output-per-feature slicing.
- Optional `features.yml` manifest for grouped features.
- Per-feature package inventory (via `renv` / `pak` / `attachment` machinery, not reimplemented).
- Per-feature function inventory (AST walk, `pkg::fn` resolution where possible).
- Per-feature `visNetwork` rendering.
- Per-feature markdown documentation stub.
- Honest warning emission for static-analysis limits (see below).

## Non-goals (explicit)

- **Package validation / risk scoring** — defer to `riskmetric`, `val.meter`.
- **Runtime / `reactlog` integration** — possible future work; not v1.
- **Custom JS message handlers, htmlwidgets internals, www/ asset tracing** — stops at the R boundary.
- **Multi-app / portfolio analysis** — single app per invocation.
- **Graph diffing across app versions** — useful, deferred.
- **Auto-generated narrative documentation** — we emit stubs; humans write intended use.
- **A monolithic report / dashboard** — artifacts first, reporting layer later, per user direction.
- **Validation of the app itself** — we produce evidence; humans validate.

## Declared static-analysis limitations

These are not bugs. They are documented in every report and surfaced as warnings on the relevant nodes:

- **Dynamic IDs** — `input[[paste0("x", i)]]`, programmatically constructed IDs.
- **`renderUI` / `insertUI` inputs** — inputs created at runtime in UI rendering.
- **Named access into `reactiveValues()`** — best-effort by name, no guarantee of completeness.
- **Metaprogramming** — `do.call`, `eval(parse(...))`, dynamic dispatch.
- **Cross-package re-exports** — function origin may resolve to the re-exporter rather than the source.
- **Conditional `source()` / `box::use()`** — files imported inside conditionals are best-effort.
- **Whole-namespace box imports** — `box::use(pkg[...])` does not declare a function set, so the call surface is determined by AST walk (same as `library(pkg)`); we do not get the tighter surface that explicit `box::use(pkg[fn1, fn2])` provides.

Each limitation has a stable warning code (e.g. `SVT-W001`) so reports are diffable and auditors can build expectations against the warning surface.

## Validation philosophy

Responsibilities, owners, and where each one sits in the V/V landscape:

| Responsibility                              | Owner                      | Layer        |
| ------------------------------------------- | -------------------------- | ------------ |
| Produce per-feature validation artifacts    | `shiny.val.tools`          | validation   |
| Write intended-use declarations             | Developer                  | validation   |
| Sign off / approve validation evidence      | Validation engineer / QA   | validation   |
| Score package risk                          | `riskmetric` / `val.meter` | dependency   |
| Verify code correctness (unit, integration) | App's own test suite       | verification |
| Verify behavior end-to-end                  | `shinytest2`, manual UAT   | verification |
| Run analysis-driven custom tests            | Developer / consumer       | verification |

We do exactly the first thing. We make the others cheaper to do — particularly the bottom row, which depends on knowing which functions and packages a feature actually uses (the inventory we emit).

### Position relative to the QC-risk model

Modern pharma validation increasingly frames risk as "the risk that a function produces an incorrect result _that QC processes cannot identify and correct_" rather than "the risk that a package contains a bug." Static risk metrics (`riskmetric`, etc.) score packages in isolation; they do not know how a package is used in situ or what QC processes wrap that usage.

`shiny.val.tools` provides the in-situ usage context that QC-risk-based validation depends on: per-feature, function-level inventories tied to declared intended use. We are an **input** to QC-risk assessment, not an implementation of it.

Two artifact-design consequences follow:

- The doc stub permits **"validation not required, with rationale"** as a valid declared state for a feature, package, or function. Documenting the decision not to validate is itself part of the validation process.
- Functional evidence (the function was called, with these inputs reachable from these UI controls) is what we emit. We do not emit numerical validation evidence (given inputs A and B, expect X) — that lives in the consumer's QC and test layers.

## Audience

- Primary: developers in pharma/GxP Shiny projects who must produce validation evidence.
- Secondary: validation engineers reviewing that evidence.
- Tertiary: auditors reading the final validation package.

API design prioritizes the primary audience. Artifact design prioritizes the secondary and tertiary.

## Glossary

- **Feature** — a named closed subgraph rooted at one or more outputs/observers.
- **Shiny module** — a `moduleServer()` (or legacy `callModule()`) UI/server pair with namespaced inputs/outputs.
- **Module subgraph** — the internal subgraph of a Shiny module, validated standalone.
- **Module contract** — the I/O surface of a Shiny module: its inputs, outputs, and returned reactives.
- **Box module** — an R file with `#' @export` declarations, importable via `box::use(./path)`. Distinct from a Shiny module, though the two often coincide in rhino apps.
- **Artifact** — one file emitted by the package per feature or module (HTML widget, markdown stub, package list, function list).
- **Manifest** — `features.yml`, an optional developer-authored declaration of feature groupings.
- **Warning code** — stable identifier (e.g. `SVT-W001`) for a class of static-analysis limitation.

## Open questions for follow-up specs

These are flagged here so they don't get lost; each is resolved in its target spec:

- Exact `features.yml` schema → [02-feature-subgraphs.md](02-feature-subgraphs.md)
- AST walk strategy: hand-rolled vs `CodeDepends` vs `lintr` AST → [01-graph-model.md](01-graph-model.md)
- `box::use()` parsing details: alias resolution, whole-namespace imports, recursive `./` path resolution, `#' @export` tag extraction → [01-graph-model.md](01-graph-model.md)
- Hybrid-app handling rules when `library()`, `source()`, and `box::use()` overlap on the same package or file → [01-graph-model.md](01-graph-model.md)
- Function-origin resolution when a function exists in multiple loaded namespaces → [03-inventory.md](03-inventory.md)
- Direct vs transitive package emission format → [03-inventory.md](03-inventory.md)
- Function-centric inventory schema (canonical) and derived package-list view → [03-inventory.md](03-inventory.md)
- Utility / Framework / Method category support: doc-stub field only, or also auto-suggest defaults → [03-inventory.md](03-inventory.md)
- Doc stub schema, including "validation not required + rationale" as a valid state → [02-feature-subgraphs.md](02-feature-subgraphs.md)
- visNetwork color/shape conventions, layout algorithm → [04-rendering.md](04-rendering.md)
- Public function naming and end-to-end workflow signature → [05-public-api.md](05-public-api.md)
