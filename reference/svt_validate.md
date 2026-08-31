# End-to-end: parse, slice, inventory, render.

End-to-end: parse, slice, inventory, render.

## Usage

``` r
svt_validate(
  app_path,
  manifest = NULL,
  out_dir = "validation",
  features = NULL,
  modules = NULL,
  lenient = FALSE,
  tests = NULL,
  test_path = NULL,
  test_results = NULL,
  strict_verification = FALSE,
  scaffold = FALSE
)
```

## Arguments

- app_path:

  Path to a Shiny app root or a `.zip` archive.

- manifest:

  Path to `features.yml`, an in-memory manifest list, or `NULL`. When
  `NULL`, `app_path/features.yml` is used if present.

- out_dir:

  Where artifacts are written. Created if missing.

- features:

  Character vector of feature names to include (`NULL` = all).

- modules:

  Character vector of module names to include (`NULL` = all).

- lenient:

  If `TRUE`, manifest issues become warnings.

- tests:

  How far to take the testing layer (spec 06): `"coverage"` also
  discovers the app's existing tests, maps them to features, and writes
  `traceability.{json,md}`; `"surface"` derives each subgraph's test
  surface and writes the `test_surface.json` artifacts and doc-stub
  sections; `"off"` skips the layer entirely. `NULL` — the default —
  resolves to `"coverage"` when the test tree exists and `"surface"`
  otherwise.

- test_path:

  Where the app's tests live. `NULL` = `<app>/tests`.

- test_results:

  Path to a
  [`testthat::JunitReporter()`](https://testthat.r-lib.org/reference/JunitReporter.html)
  XML report to ingest, or `NULL`. We ingest; we never execute.

- strict_verification:

  If `TRUE`, a `high`-risk feature with no mapped test (SVT-W301) aborts
  the run rather than warning. The intended CI gate, and the one place
  the package takes a position.

- scaffold:

  If `TRUE`, also write test scaffolds into `<out_dir>/tests/`. Off by
  default: generating files into a validated repository is opt-in,
  always.

## Value

An `svt_validation` object.
