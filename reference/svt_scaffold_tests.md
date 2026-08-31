# Write test scaffolds for a derived test surface.

One file per surface plus one per helper group. The generated code is a
harness with the plumbing filled in and the assertions left blank: every
`test_that()` block opens with a `skip()` marker, which is visible in
`testthat` output, countable, non-blocking, and detectable by the
coverage classifier. Removing that line is the developer's explicit act
of taking ownership of the test.

## Usage

``` r
svt_scaffold_tests(
  surface,
  out_dir = "validation",
  target = c("staging", "app"),
  features = NULL
)
```

## Arguments

- surface:

  An `svt_test_surface` object.

- out_dir:

  The validation directory. Scaffolds are written to `<out_dir>/tests/`
  under `target = "staging"`.

- target:

  `"staging"` (default) or `"app"`.

- features:

  Character vector of surface names to scaffold (`NULL` = all).

## Value

A tibble with one row per scaffold: `name`, `kind` (`surface` /
`helpers`), `path`, `status` (`written`, `unchanged`, `skipped_edited`,
`skipped_exists`) and `warnings`.

## Details

Assertions and expected values are never generated — an expected value
is a human judgment about the analysis, and a generated one would be
worse than no test at all.

Overwrite rules. With `target = "staging"` files land in
`<out_dir>/tests/`: a scaffold nobody touched refreshes, and one whose
contents have moved away from the artifact manifest's record is a test
somebody wrote and is left alone. With `target = "app"` files land in
the app's own `tests/testthat/` and an existing path is **never**
written — it is skipped with SVT-W309.
