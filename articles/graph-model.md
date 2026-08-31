# The reactive graph model

This vignette describes the reactive graph that
[`svt_build_graph()`](https://appsilon.github.io/shiny.val.tools/reference/svt_build_graph.md)
extracts from an app’s R source — the data model, the AST extraction
strategy, and the rules used to handle traditional Shiny, rhino
([`box::use`](https://klmr.me/box/reference/use.html)), and hybrid apps.

## What the graph contains

[`svt_build_graph()`](https://appsilon.github.io/shiny.val.tools/reference/svt_build_graph.md)
returns an `svt_graph` object — a list of tibbles:

``` r

graph$files     # one row per enumerated R file
graph$imports   # one row per library/require/box::use clause
graph$sources   # one row per source() call
graph$nodes     # one row per graph node
graph$edges     # one row per depends_on edge
graph$warnings  # one row per warning hit
```

`nodes` and `edges` are the structural core. The other tibbles are the
*intermediate tables* the walkers produce — useful for inspection and
auditing how a given node came to exist.

## Node model

Each node carries:

    Node {
      id           : string             # stable, content-addressed
      type         : enum {input, output, reactive, observer,
                           value, module_instance}
      name         : string             # bare name (e.g. "x" for input$x)
      namespace    : string?            # module namespace; NA for top-level
      fq_name      : string             # namespace + "/" + name (or just name)
      file, line, col                   # source location
      warnings     : list of warning codes attached to this node
    }

Node IDs are stable across runs given the same source. The basis is
`(type, namespace, container, name)`, structured as a colon-separated
string. Stability matters for diffing artifacts and for `features.yml`
references.

### Node types

- **input** — emitted on the **first** static reference to `input$x` (or
  `ns("x")` inside a module). One node per `(namespace, name)`
  regardless of how many references exist.
- **output** — emitted on assignment to `output$y`. One node per
  `(namespace, name)`. Repeated assignment to the same output emits
  **SVT-W010** (“output reassigned”) on the second and subsequent
  occurrences.
- **reactive** — emitted on a named binding `name <- reactive(...)`,
  `name <- eventReactive(...)`, or
  `name <- bindEvent(reactive(...), ...)`. Anonymous reactives passed
  inline are not emitted as nodes; their dependencies are attributed to
  the consuming node.
- **observer** — emitted on `observe(...)`, `observeEvent(...)`,
  `bindEvent(observe(...), ...)`, `downloadHandler(...)`, and assigned
  variants. Side-effecting; no outgoing edges to consumers.
- **value** — emitted on entries in `reactiveValues(name = ...)` and on
  subsequent named writes (`rv$name <- ...`). Best-effort by name; every
  such read carries **SVT-W003** because access is by name only.
- **module_instance** — emitted on each call site of a module wrapper
  function with a literal namespace id (e.g. `counter_server("c1")`).
  Represents a black-box reference to a module subgraph; carries the
  module contract.

## Edge model

    Edge {
      source     : node_id              # the dependent (reads the target)
      target     : node_id              # the dependency
      file, line, col                   # where the reference appears
    }

There is one edge type, `depends_on`. Multiple references collapse into
a single edge; `source_loc` is the first occurrence.

The rendered widget flips edges so the visual arrow flows *input →
reactive → output* — see
[`vignette("rendering")`](https://appsilon.github.io/shiny.val.tools/articles/rendering.md).

## File enumeration

The starting set, per Shiny convention:

- `app.R` (if present)
- `ui.R` and `server.R` (if present)
- `global.R` (if present, evaluated first by Shiny convention)
- All files matching `R/**/*.R`
- `.Rprofile` (if present) — runs at R startup; in rhino apps it sets
  `options(box.path = ...)`. Parsed for the same patterns as any other
  file.
- `app/main.R` (if present) — the rhino entry point. Rhino’s `app.R` is
  opaque
  ([`rhino::app()`](https://appsilon.github.io/rhino/reference/app.html));
  it boots `app/main.R` via `box::use(app/main)`. Since we can’t follow
  that statically, we add `app/main.R` directly when it exists.

From the starting set, the enumerator walks:

- `source(path)` — resolve `path` relative to the calling file
  (respecting `chdir = TRUE`).
- `box::use(./...)` and `box::use(../...)` — resolve relative to the
  calling file.
- `box::use(seg/seg/...)` (no leading `.` / `..`) — resolve as an
  absolute box path; see “Absolute box paths” below.

Files reached only through conditional
[`source()`](https://rdrr.io/r/base/source.html) or conditional
[`box::use()`](https://klmr.me/box/reference/use.html) are still
enumerated, but every node and edge sourced from them carries
**SVT-W004** (“conditional inclusion”). Files not reached through any
path are not enumerated.

## AST extraction strategy

We use base R’s [`parse()`](https://rdrr.io/r/base/parse.html) to
produce a parse tree, walk it recursively with our own visitor, and use
[`getSrcref()`](https://rdrr.io/r/utils/sourceutils.html) to attach
source locations.

We do **not** use:

- `CodeDepends` — too opinionated about its own model, sparsely
  maintained.
- `lintr` / `xmlparsedata` — heavyweight XML representation, awkward for
  our patterns.

The constructs we recognize are Shiny-specific and box-specific. None of
the existing tools handle them natively. A hand-rolled walker over the
base parse tree is more direct than wrapping a tool that doesn’t fit.

### Diagnostic helper: `svt_inspect()`

For human-readable AST inspection during development and in test failure
messages, the package uses
[`lobstr::ast()`](https://lobstr.r-lib.org/reference/ast.html). `lobstr`
lives in `Suggests:` rather than `Imports:`; the package functions
without it and
[`svt_inspect()`](https://appsilon.github.io/shiny.val.tools/reference/svt_inspect.md)
falls back to base [`print()`](https://rdrr.io/r/base/print.html) when
`lobstr` is absent.

``` r

svt_inspect("R/server.R")
```

## The five intermediate tables

The walk produces five tables before assembly:

1.  **Files** — one row per enumerated R file.
2.  **Imports** — one row per
    [`library()`](https://rdrr.io/r/base/library.html),
    [`require()`](https://rdrr.io/r/base/library.html), or
    [`box::use()`](https://klmr.me/box/reference/use.html) clause.
    Captures package, alias, function set, local path, file containing
    the call.
3.  **Sources** — one row per
    [`source()`](https://rdrr.io/r/base/source.html) call. Captures
    path, calling file, conditional flag.
4.  **Definitions** — one row per `output$x`, named reactive/observer
    assignment, `reactiveValues()` declaration, and `moduleServer()`
    definition site.
5.  **References** — one row per read of `input$x`, a named reactive,
    `rv$name`, or a function call resolvable to `pkg::fn`.

Nodes are constructed from the Definitions table (plus inputs from
References, since inputs have no syntactic definition site). Edges are
constructed by joining References to Definitions within the appropriate
scope (function body, module server body).

## Source and import resolution

Shiny apps bring code into scope in two distinct ways. The package
handles both — including hybrid apps that mix them, which are common in
real codebases.

**Traditional Shiny** uses
[`library(pkg)`](https://rdrr.io/r/base/library.html) and
`source("path/to/file.R")`.

**Rhino / `box`** uses per-file `box::use(...)`:

``` r

box::use(
  shiny[...],              # whole-namespace
  dplyr[filter, mutate],   # explicit function set
  d = dplyr,               # aliased; calls look like d$something()
  ./modules/foo,           # local module via relative path
  ./utils[helper_fn],
)
```

Box matters for two reasons:

1.  **Tighter, more honest inventory.** With
    [`library(dplyr)`](https://dplyr.tidyverse.org), the static call
    surface is everything in `dplyr`; we narrow only by walking call
    sites. With `box::use(dplyr[filter, mutate])`, the import statement
    *declares* the call surface — no narrowing required.
2.  **Per-file dependency declarations.** Every R file that uses box
    owns its own [`box::use()`](https://klmr.me/box/reference/use.html)
    block.

A **box module** (an R file with `#' @export` declarations) is not the
same as a **Shiny module** (a `moduleServer()` UI/server pair). Rhino
apps usually colocate one of each per file, but they are distinct
concepts.

### `box::use()` clauses

| Clause syntax | Meaning |
|----|----|
| `pkg` | package import; calls written as `pkg$fn` |
| `pkg[fn1, fn2]` | function-set import; calls written as `fn1(...)` |
| `pkg[...]` | whole-namespace; **SVT-W005** emitted on the import row |
| `alias = pkg` | aliased package; calls written as `alias$fn` |
| `alias = pkg[fn1]` | aliased function set |
| `./mod`, `../mod` | local box module (path relative to the calling file) |
| `seg/seg/...` | absolute local box module — see below |

### Absolute box paths

A box clause whose path has multiple segments and does **not** start
with `.` or `..` is an absolute box path — `box::use(app/view/home)` is
the rhino convention. At runtime, box resolves these against
`getOption("box.path")`. Rhino’s `.Rprofile` typically sets
`options(box.path = getwd())`, making absolute paths resolve relative to
the project root.

Statically, we mirror that:

- `.Rprofile` (and any other enumerated file) is scanned for
  `options(box.path = expr)`.
- [`getwd()`](https://rdrr.io/r/base/getwd.html) is treated as the app
  root. A string literal is currently not honored — we have no reliable
  way to relate an absolute filesystem path to our app-relative model.
- If no setting is found we fall back to the app root anyway. Rhino’s
  convention is universal enough that allowing absolute resolution by
  default is the pragmatic choice.

## Module namespacing

Shiny modules use `NS(id)` or `session$ns()` to construct namespaced
IDs. Inside `moduleServer(id, function(input, output, session) {...})`,
references to `input$x` and `output$y` are namespaced under `id`.

The walker distinguishes:

- **Module definition** — the function expression containing
  `moduleServer(...)`. Inputs and outputs collected here are namespaced
  by the module’s namespace symbol (a placeholder until instantiation).
- **Module instantiation** — the call site invoking the module function
  with a concrete `id`. The placeholder namespace is bound to that ID.

Modules instantiated multiple times produce one `module_instance` node
per call site, each with its own namespace.

Legacy `callModule()` is supported with the same model.

## Hybrid-app overlap rules

When [`library(pkg)`](https://rdrr.io/r/base/library.html) and
`box::use(pkg[fn1])` both reference the same package:

- **In the file containing `box::use(pkg[fn1])`:** the box clause wins.
  The call surface for that file is `{fn1}` only. Other `pkg` references
  in that file resolve as `pkg::other` but emit **SVT-W006**
  (“library/box overlap”).
- **In files without `box::use(pkg)`:**
  [`library(pkg)`](https://rdrr.io/r/base/library.html) applies; the
  call surface is determined by AST walk.

When `source("foo.R")` and `box::use(./foo)` both reach the same file:

- The file is enumerated once.
- Both inclusions are recorded in their respective tables.
- **SVT-W007** (“duplicate inclusion”) is emitted on both call sites.

When [`library()`](https://rdrr.io/r/base/library.html) is called inside
a function body:

- Treated as a global library() call (Shiny apps load on startup; the
  function may be the server function itself).
- **SVT-W008** (“library() inside a function”) emitted as a code-quality
  flag.

## Declared static-analysis limitations

These are not bugs. They are documented in every report and surfaced as
warnings on the relevant nodes:

- **Dynamic IDs** — `input[[paste0("x", i)]]`, programmatically
  constructed IDs (**SVT-W001**).
- **`renderUI` / `insertUI` inputs** — inputs created at runtime in UI
  rendering (**SVT-W002**).
- **Named access into `reactiveValues()`** — best-effort by name
  (**SVT-W003**).
- **Conditional [`source()`](https://rdrr.io/r/base/source.html) /
  [`box::use()`](https://klmr.me/box/reference/use.html)** — files
  imported inside conditionals are best-effort (**SVT-W004**).
- **Whole-namespace box imports** — `box::use(pkg[...])` does not
  declare a function set, so the call surface is determined by AST walk
  (same as [`library(pkg)`](https://rdrr.io/r/base/library.html)); we do
  not get the tighter surface that explicit `box::use(pkg[fn1, fn2])`
  provides (**SVT-W005**).
- **`library` / [`box::use`](https://klmr.me/box/reference/use.html)
  overlap** with non-imported references (**SVT-W006**).
- **Duplicate file inclusion** via
  [`source()`](https://rdrr.io/r/base/source.html) and
  [`box::use()`](https://klmr.me/box/reference/use.html) (**SVT-W007**).
- **[`library()`](https://rdrr.io/r/base/library.html) inside a
  function** body (**SVT-W008**).
- **Metaprogramming** — `do.call`, `eval(parse(...))`, dynamic dispatch
  (**SVT-W009**).
- **Output reassigned** — multiple `output$x <- ...` for the same name
  (**SVT-W010**).

Each limitation has a stable warning code so reports are diffable and
auditors can build expectations against the warning surface. The full
list is at
[`vignette("warning-codes")`](https://appsilon.github.io/shiny.val.tools/articles/warning-codes.md).

## Determinism

Same source produces byte-identical output. Node IDs are
content-addressed; node and edge tables are sorted before serialization.
This contract underwrites the diff-based regeneration semantics of doc
stubs (see
[`vignette("feature-subgraphs")`](https://appsilon.github.io/shiny.val.tools/articles/feature-subgraphs.md)).
