# Derive the test surface for every feature and module.

The test surface answers "what is there to test in this feature": the
inputs a test must set (stimuli), the outputs and returned reactives it
can assert on (observables), the intermediates between them, the trusted
terminals, and the app-defined helpers it calls. Everything is derived
from the subgraph — the app is never run.

## Usage

``` r
svt_test_surface(features, inventory = NULL)
```

## Arguments

- features:

  An `svt_features` object.

- inventory:

  An `svt_inventory` object, or `NULL`. Supplies the called-function set
  from which helpers are derived.

## Value

An `svt_test_surface` object — a named list of per-subgraph surface
records.

## Details

The package targets `testthat` only: every feature and module surface is
driven through
[`shiny::testServer()`](https://rdrr.io/pkg/shiny/man/testServer.html),
and each helper gets a plain `testthat` unit stub. Observables whose
render function produces a value that cannot usefully be compared
(`renderPlot`, `renderUI`, `downloadHandler`, ...) are annotated
`opaque` and flagged SVT-W312, which points the assertion at the helper
that computed the data.

Coverage in this layer means *exercised*, never *correct*.
