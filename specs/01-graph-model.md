# Reactive graph model and AST extraction

## Purpose

Defines the data model for the reactive graph and the strategy used to extract it from source code. Resolves overview open questions: AST walk strategy, `box::use()` parsing details, hybrid-app overlap rules.

## Node model

```
Node {
  id           : string             # stable, content-addressed
  type         : enum {input, output, reactive, observer, value, module_instance}
  name         : string             # bare name (e.g. "x" for input$x)
  namespace    : string?            # module identity (file-path-derived) for module-internal nodes; null at top level
  fq_name      : string             # namespace + "/" + name (or just name)
  source_loc   : {file, line, col}  # where the node is defined or first referenced
  warnings     : [warning_code]     # any SVT-W*** codes attached
}
```

Node IDs are stable across runs given the same source. Basis: `(namespace, type, name)` with a content hash suffix only if needed for disambiguation. Stability matters for diffing and for `features.yml` references.

### Node type semantics

- **input** — emitted on the **first** static reference to `input$x` (or `ns("x")` inside a module). One node per `(namespace, name)` regardless of how many references exist.
- **output** — emitted on assignment to `output$y`. One node per `(namespace, name)`. Repeated assignment to the same output emits SVT-W010 ("output reassigned") on the second and subsequent occurrences.
- **reactive** — emitted on a named binding `name <- reactive(...)`, `name <- eventReactive(...)`, or `name <- bindEvent(reactive(...), ...)`. Anonymous reactives passed inline are not emitted as nodes; their dependencies are attributed to the consuming node.
- **observer** — emitted on `observe(...)`, `observeEvent(...)`, `bindEvent(observe(...), ...)`, `downloadHandler(...)`, and assigned variants. Side-effecting; no outgoing edges to consumers.
- **value** — emitted on entries in `reactiveValues(name = ...)` and on subsequent named writes (`rv$name <- ...`). Best-effort by name; flagged with SVT-W003 when access is dynamic.
- **module_instance** — emitted on every parent-side call to a known module wrapper. Represents a black-box reference to a module subgraph; carries the module contract. The node's `name` is the target module's identity (file-path-derived; see spec 02 "Module identity") so the rendered artifact link points at the right `<module-id>.html`. Three call shapes are recognised:
  1. **Bare-name call** — `wrapper(id, ...)` where `wrapper` is a function in the file whose definition contains a `moduleServer()` call. Traditional Shiny pattern.
  2. **Box-aliased server call** — `<alias>$server(id, ...)` where `<alias>` is bound by a `box::use(<path/to/mod>)` or `box::use(<alias> = <path/to/mod>)` clause whose target resolves to a module identity in the slice. Rhino server-side pattern.
  3. **Box-aliased UI call** — `<alias>$ui(id, ...)` same alias rules. Rhino UI-side mounting; included so the architecture diagram surfaces "main uses module X" even when the relationship is established only through the UI tree.

  At the architecture level (index artifact), parent→child edges are de-duplicated so a parent that calls both `<alias>$ui(...)` and `<alias>$server(...)` produces a single edge.

## Edge model

```
Edge {
  source     : node_id              # the dependent (reads the target)
  target     : node_id              # the dependency
  source_loc : {file, line, col}    # where the reference appears
}
```

There is one edge type, `depends_on`. Multiple references collapse into a single edge; `source_loc` is the first occurrence.

## AST extraction strategy

### Choice: base R parse tree, hand-rolled visitor

We use base R's `parse()` to produce a parse tree, walk it recursively with our own visitor, and use `getSrcref()` to attach source locations.

We do **not** use:

- `CodeDepends` — too opinionated about its own model, sparsely maintained.
- `lintr` / `xmlparsedata` — heavyweight XML representation, awkward for our patterns.

Rationale: the constructs we recognize are Shiny-specific and box-specific. None of the existing tools handle them natively, and the visitor needs only ~10–20 patterns. A hand-rolled walker over the base parse tree is more direct than wrapping a tool that doesn't fit.

### Diagnostics: `lobstr` as a `Suggests:` dependency

For human-readable AST inspection — during development and in test failure messages — we use `lobstr::ast()`. `lobstr` is **not** a parser substitute; it is a pretty-printer over `parse()` results, designed for visual inspection rather than programmatic walking. The production pipeline does not depend on it.

`lobstr` lives in `Suggests:` rather than `Imports:`. The package functions without it; diagnostic helpers gracefully degrade to base `print()` when it is absent. A public helper `svt_inspect()` (spec 05) exposes the pretty-printed AST for one file, intended for users debugging why their app produced an unexpected graph.

### Visitor passes and intermediate tables

The walk produces six intermediate tables before assembly:

1. **Files table** — one row per enumerated R file, with parse status and metadata.
2. **Imports table** — one row per `library()`, `require()`, `box::use()` clause. Captures package, alias, function set, local path, file containing the call.
3. **Sources table** — one row per `source()` call. Captures path, calling file, conditional flag.
4. **Definitions table** — one row per `output$x`, named reactive/observer assignment, `reactiveValues()` declaration, `moduleServer()` definition site, and top-level function definition (`kind = "function"`). Carries `wrapper_formals` for `module_server` rows (the comma-joined formal names of the wrapper function, which is what fills a generated `testServer()` scaffold's `args = list(...)`) and `def_call`: the name of the call on the right-hand side (`renderPlot`, `renderText`, `reactive`, `eventReactive`, `observeEvent`, `downloadHandler`, …). `classify_rhs()` already derives the node kind from this call; recording the call itself costs nothing and is what tells spec 06 whether an observable is opaque under `testServer()`.
5. **References table** — one row per read of `input$x`, a named reactive, `rv$name`, or a function call resolvable to `pkg::fn`.
6. **Tests table** — one row per `test_that()` block found in the app's **test tree**, with the harness it uses, the inputs and outputs it touches, the functions it calls, and any `@covers` annotations. Built by `build_tests_table()`; consumed only by spec 06.

Nodes are constructed from the Definitions table. Two kinds are deliberately excluded: `module_server` (the instance nodes come from the wrapper call sites instead) and `function` (a top-level function definition is not a reactive-graph node — the rows exist so spec 06 can tell an app-defined helper apart from an unresolved package call, and so the inventory can name where a helper is defined). Edges are constructed by joining References to Definitions within the appropriate scope (function body, module server body).

### Source location

Every node and edge carries `{file, line, col}` from `getSrcref()`. Locations survive in artifacts and are clickable in rendered HTML widgets where possible.

## File enumeration

Test files are **not** part of the app graph. The enumeration below covers application source only; `tests/**` is enumerated separately by `build_tests_table()` (spec 06) and never contributes a node, an edge, an import row, or an inventory row. A test that calls `dplyr::filter` is not the app calling `dplyr::filter`, and letting test code leak into a feature's inventory would corrupt the validation surface.

Starting set:

- `app.R` (if present)
- `ui.R` and `server.R` (if present)
- All files matching `R/**/*.R`
- `global.R` (if present, evaluated first by Shiny convention)
- `.Rprofile` (if present) — runs at R startup; in rhino apps it sets `options(box.path = ...)`. Parsed for the same `box::use()` / `library()` / `source()` patterns as any other file.
- `app/main.R` (if present) — the rhino entry point. Rhino's `app.R` is opaque (`rhino::app()`); it boots `app/main.R` via `box::use(app/main)`. We can't follow that statically because `app.R` calls into a package, so we add `app/main.R` directly when it exists.

Follow recursively:

- `source(path)` — resolve `path` relative to the calling file (respecting `chdir = TRUE`).
- `box::use(./...)` and `box::use(../...)` — resolve relative to the calling file.
- `box::use(seg/seg/...)` (no leading `.` / `..`) — resolve as an absolute box path; see "Absolute box paths" below.
- Cycle prevention via tracking the enumerated set.

Files reached only through conditional `source()` or conditional `box::use()` are still enumerated, but every node and edge sourced from them carries SVT-W004 ("conditional inclusion"). Files not reached through any path are not enumerated.

## Box::use parsing details

`box::use()` is parsed as a syntactic form, never evaluated. The argument list is walked clause-by-clause.

| Clause syntax       | Meaning                                                        |
| ------------------- | -------------------------------------------------------------- |
| `pkg`               | package import; calls written as `pkg$fn`                      |
| `pkg[fn1, fn2]`     | function-set import; calls written as `fn1(...)`               |
| `pkg[...]`          | whole-namespace; SVT-W005 emitted on the import row            |
| `alias = pkg`       | aliased package; calls written as `alias$fn`                   |
| `alias = pkg[fn1]`  | aliased function set                                           |
| `./mod`, `../mod`   | local box module (path relative to the calling file)           |
| `./mod[fn]`         | local box module with explicit function set                    |
| `seg/seg/...`       | absolute local box module — see "Absolute box paths" below     |

Each clause produces one row in the Imports table. Aliases are resolved during the Reference pass: `alias$fn(...)` resolves to `pkg::fn`.

### Absolute box paths

A box clause whose path has multiple segments and does **not** start with `.` or `..` is an absolute box path — `box::use(app/view/home)` is the rhino convention. At runtime box resolves these against `getOption("box.path")`. Rhino's `.Rprofile` sets `options(box.path = getwd())`, making absolute paths resolve relative to the project root.

Statically, we mirror that:

- We scan `.Rprofile` (and any other enumerated file) for `options(box.path = expr)`. The expression is captured but not evaluated.
- The expression `getwd()` is treated as the app root. A string literal is currently not honored — we have no reliable way to relate an absolute filesystem path to our app-relative model. (Future: accept string literals that match the app root.)
- If no setting is found we fall back to the app root anyway. Rhino's convention is so universal that allowing absolute resolution by default is the pragmatic choice.

Each absolute clause produces an Imports row with `local_path` set to the as-written path and a separate `absolute = TRUE` flag.

### `#' @export` extraction

Each enumerated R file is scanned for roxygen-style `#' @export` comments preceding function or assignment definitions. Extracted exports populate the box module's contract. Functions without `#' @export` are private to their box module.

When a Shiny module's `moduleServer()` lives in a box module file, the box exports refine the Shiny module contract: a function that is `#' @export`'d and returns or contains a `moduleServer()` is a Shiny module that the box module exports.

## Module namespacing

Shiny modules use `NS(id)` or `session$ns()` to construct namespaced IDs. Inside `moduleServer(id, function(input, output, session) {...})`, references to `input$x` and `output$y` are namespaced under `id`.

The walker distinguishes:

- **Module definition** — the function expression containing `moduleServer(...)`. Inputs and outputs collected here are namespaced by the module's namespace symbol (a placeholder until instantiation).
- **Module instantiation** — the call site invoking the module function with a concrete `id`. The placeholder namespace is bound to that ID.

Modules instantiated multiple times produce one `module_instance` node per call site, each with its own namespace.

Legacy `callModule()` is supported with the same model. The module function signature is `function(input, output, session, ...)` and the wrapper is `callModule(modFn, "id", ...)`.

## Hybrid-app overlap rules

When `library(pkg)` and `box::use(pkg[fn1])` both reference the same package:

- **In the file containing `box::use(pkg[fn1])`:** the box clause wins. The call surface for that file is `{fn1}` only. Other `pkg` references in that file resolve as `pkg::other` but emit SVT-W006 ("library/box overlap — unexpected call surface").
- **In files without `box::use(pkg)`:** `library(pkg)` applies; the call surface is determined by AST walk.

Detection scope for SVT-W006: `library()` is treated as app-global (see below), so the overlap holds wherever in the source the `library()` call sits. Only clauses with an **explicit function set** participate — a whole-namespace `box::use(pkg[...])` declares no surface to exceed, and is SVT-W005's business instead. Confirming that a bare name belongs to `pkg` requires the package to be installed and loadable; when it is not, no candidate is confirmed and the call falls through to SVT-W203 rather than being reported here.

When `source("foo.R")` and `box::use(./foo)` both reach the same file:

- The file is enumerated once.
- Both inclusions are recorded in their respective tables.
- SVT-W007 ("duplicate inclusion via source() and box::use()") is emitted on both call sites.

When `library()` is called inside a function body:

- Treated as a global library() call (Shiny apps load on startup; the function may be the server function).
- SVT-W008 ("library() inside a function") emitted as a code-quality flag.

## Warning codes (range: SVT-W001 to SVT-W099)

- **SVT-W001** — Dynamic input ID (`input[[expr]]`)
- **SVT-W002** — `renderUI` / `insertUI` source for input definition. Emitted when an input-creating UI call (`*Input(...)`, `actionButton`, `actionLink`, `selectizeInput`, …) with a **literal** id appears inside a `renderUI()` / `insertUI()` body. The id exists only once that UI renders, so the input node it creates has no static definition site and any test that sets it must render the UI first. Detection is deliberately narrow: a non-literal id inside `renderUI` is already SVT-W001 territory, and a UI call outside a render context is a normal static input
- **SVT-W003** — Named access into `reactiveValues()` resolved by name only
- **SVT-W004** — Conditional `source()` or `box::use()` inclusion
- **SVT-W005** — Whole-namespace box import (`pkg[...]`)
- **SVT-W006** — `library` / `box::use` overlap with non-imported reference
- **SVT-W007** — Duplicate file inclusion via `source()` and `box::use()`
- **SVT-W008** — `library()` inside a function body
- **SVT-W009** — Metaprogramming construct (`do.call`, `eval(parse(...))`) blocks resolution
- **SVT-W010** — Output reassigned (multiple `output$x <- ...` for the same name)

## Internal data shape

The graph is held internally as a list of tibbles:

```r
graph <- list(
  files     = tibble::tibble(...),
  imports   = tibble::tibble(...),
  sources   = tibble::tibble(...),
  nodes     = tibble::tibble(...),
  edges     = tibble::tibble(...),
  warnings  = tibble::tibble(...)
)
```

This shape is the input to spec 02 (slicing) and spec 03 (inventory). The shape is stable in v1.

## Out of scope for this spec

- Slicing into feature subgraphs (spec 02)
- Function-origin resolution beyond `pkg::fn` (spec 03)
- Rendering (spec 04)
