# Map an app's existing tests onto its derived test surfaces.

Discovers every `test_that()` block under `test_path`, links each to the
features, modules and helpers it exercises, and classifies each surface
as `covered`, `partial`, `scaffold`, `uncovered` or `waived`.

## Usage

``` r
svt_test_coverage(surface, features = NULL, test_path = NULL, results = NULL)
```

## Arguments

- surface:

  An `svt_test_surface` object.

- features:

  An `svt_features` object, or `NULL`. Supplies the manifest
  (`verification`, `rationale_verification`, `tests:`) and each record's
  declared risk and intended use.

- test_path:

  Where the app's tests live. `NULL` = `<app>/tests`.

- results:

  Path to a
  [`testthat::JunitReporter()`](https://testthat.r-lib.org/reference/JunitReporter.html)
  XML report to ingest, or `NULL`.

## Value

An `svt_test_coverage` object.

## Details

Linking has two mechanisms. A `# @covers feature: km_plot` comment in
the test is the contract; harness-target inference is the convenience
that fills in the rest (a `testServer(mod$server, ...)` block resolves
to that module, a `testServer()` block over the app directory to the
features whose outputs it reads, a plain block to the helpers it calls).
Which mechanism produced each link is recorded, so a reviewer can see
how much of the matrix is inferred. A test that maps to nothing is an
orphan (SVT-W304); the remedy is an annotation.

Coverage here means **exercised**, never **correct**: a feature reported
`covered` has tests that reach its observables; whether those tests
assert the right thing is a reviewer's judgment. Tests are discovered
and mapped, never executed — with `results` you can ingest a
[`testthat::JunitReporter()`](https://testthat.r-lib.org/reference/JunitReporter.html)
XML report your CI already produced, and each mapped test is stamped
`pass` / `fail` / `skip` / `missing`.
