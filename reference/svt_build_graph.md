# Build the reactive graph from a parsed app.

Returns the five intermediate tables (Files, Imports, Sources,
Definitions, References) plus the assembled Nodes, Edges, and Warnings
tables as an `svt_graph` object. The original `app_path` is carried
through so downstream steps (slicing, inventory, render) can re-resolve
files.

## Usage

``` r
svt_build_graph(parsed)
```

## Arguments

- parsed:

  An `svt_parsed` object from
  [`svt_parse()`](https://appsilon.github.io/shiny.val.tools/reference/svt_parse.md).

## Value

An `svt_graph` object — the graph tibbles plus `app_path`.
