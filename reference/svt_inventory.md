# Build per-feature inventories.

For every feature/module in `features`, resolves call sites against the
file's imports, classifies direct vs transitive, and emits the
function-centric FunctionCall and derived PackageUse views.

## Usage

``` r
svt_inventory(graph, features)
```

## Arguments

- graph:

  An `svt_graph`.

- features:

  An `svt_features` object.

## Value

An `svt_inventory` object — a named list keyed by feature name, each
entry the per-feature inventory record.
