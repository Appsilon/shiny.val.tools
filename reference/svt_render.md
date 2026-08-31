# Render artifacts for every feature/module.

Per feature/module: writes `<out_dir>/<name>.md` (the doc stub, with
Functions called and Packages used populated from the inventory),
`<out_dir>/<name>/inventory.json`, and `<out_dir>/<name>.html` (the
interactive visNetwork widget).

## Usage

``` r
svt_render(
  features,
  inventory,
  out_dir = "validation",
  surface = NULL,
  coverage = NULL,
  scaffold = FALSE
)
```

## Arguments

- features:

  An `svt_features` object.

- inventory:

  An `svt_inventory` object.

- out_dir:

  Directory to write artifacts to. Created if missing.

- surface:

  An `svt_test_surface` object, or `NULL`. When `NULL` the
  `## Test surface` doc-stub sections and the per-feature
  `test_surface.json` artifacts are omitted entirely rather than
  rendered empty.

- coverage:

  An `svt_test_coverage` object, or `NULL`. When supplied, the
  `## Test coverage` doc-stub sections, the index's Verification
  coverage section and `traceability.{json,md}` are written; when `NULL`
  they are omitted entirely rather than rendered empty.

- scaffold:

  If `TRUE`, also write test scaffolds into `<out_dir>/tests/` as
  lifecycle-tracked artifacts. Requires `surface`. Off by default:
  generating files into a validated repository is opt-in, always.

## Value

An `svt_validation` object — paths and counts.
