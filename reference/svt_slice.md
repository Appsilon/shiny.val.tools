# Slice a graph into feature and module subgraphs.

Without a manifest, the default rule applies (one feature per top-level
output / observer; one module subgraph per `moduleServer` definition).
With a manifest, manifest-declared features supersede default features
at the same root, and module subgraphs are still produced for every
module definition.

## Usage

``` r
svt_slice(graph, manifest = NULL, lenient = FALSE)
```

## Arguments

- graph:

  An `svt_graph` from
  [`svt_build_graph()`](https://appsilon.github.io/shiny.val.tools/reference/svt_build_graph.md).

- manifest:

  Path to `features.yml`, an in-memory manifest list, or `NULL` for
  default slicing.

- lenient:

  If `TRUE`, manifest issues become warnings rather than aborts.

## Value

An `svt_features` object — a list of feature/module records with
`manifest_supplied`, `manifest_issues`, `unclaimed`, and `app_path`
carried alongside.

## Details

Manifest validation is run before application; with `lenient = FALSE`
(the default) any SVT-W101/W102/W105 issue or a name collision aborts.
With `lenient = TRUE` the issues are reported via `cli` warnings and
slicing proceeds best-effort.
