# Generate a starter `features.yml` for a graph.

Writes the YAML to `out_path` (or returns it as a string when `out_path`
is NULL). Each top-level output/observer becomes one entry with
`intended_use`/`risk_classification`/`rationale` left as `~` for the
developer to fill.

## Usage

``` r
svt_manifest_template(graph, out_path = "features.yml")
```

## Arguments

- graph:

  An `svt_graph`.

- out_path:

  Path to write the YAML to. `NULL` returns the YAML as a string instead
  of writing.
