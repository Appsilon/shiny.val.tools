# All warnings, as a flat tibble.

For an `svt_graph`, returns the global warnings table. For an
`svt_features`, prepends the manifest issues. For an `svt_inventory`,
returns the union of every feature's inventory warnings tagged with the
owning feature name.

## Usage

``` r
svt_warnings(x)
```

## Arguments

- x:

  An `svt_graph`, `svt_features`, or `svt_inventory` object.
