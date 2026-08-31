# Validate a manifest against a graph.

Returns a tibble of issues; an empty tibble means "ready to use". Wraps
`validate_manifest()` so callers can inspect issues without triggering
the slice-time abort.

## Usage

``` r
svt_manifest_validate(manifest, graph)
```

## Arguments

- manifest:

  Path to `features.yml` or an in-memory manifest list.

- graph:

  An `svt_graph`.
