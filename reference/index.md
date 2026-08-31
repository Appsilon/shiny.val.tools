# Package index

## End-to-end

Run the full pipeline from a Shiny app path to artifacts on disk.

- [`svt_validate()`](https://appsilon.github.io/shiny.val.tools/reference/svt_validate.md)
  : End-to-end: parse, slice, inventory, render.

## Pipeline steps

The composable steps invoked by
[`svt_validate()`](https://appsilon.github.io/shiny.val.tools/reference/svt_validate.md).

- [`svt_parse()`](https://appsilon.github.io/shiny.val.tools/reference/svt_parse.md)
  : Parse a Shiny app's source.
- [`svt_build_graph()`](https://appsilon.github.io/shiny.val.tools/reference/svt_build_graph.md)
  : Build the reactive graph from a parsed app.
- [`svt_slice()`](https://appsilon.github.io/shiny.val.tools/reference/svt_slice.md)
  : Slice a graph into feature and module subgraphs.
- [`svt_inventory()`](https://appsilon.github.io/shiny.val.tools/reference/svt_inventory.md)
  : Build per-feature inventories.
- [`svt_render()`](https://appsilon.github.io/shiny.val.tools/reference/svt_render.md)
  : Render artifacts for every feature/module.

## Testing layer

Derive each subgraph’s test surface, scaffold the harness, map the app’s
existing tests onto it, and emit the traceability matrix. The package
targets `testthat` only; it writes no assertions and runs nothing.
Coverage means exercised, never correct.

- [`svt_test_surface()`](https://appsilon.github.io/shiny.val.tools/reference/svt_test_surface.md)
  : Derive the test surface for every feature and module.
- [`svt_scaffold_tests()`](https://appsilon.github.io/shiny.val.tools/reference/svt_scaffold_tests.md)
  : Write test scaffolds for a derived test surface.
- [`svt_test_coverage()`](https://appsilon.github.io/shiny.val.tools/reference/svt_test_coverage.md)
  : Map an app's existing tests onto its derived test surfaces.
- [`svt_traceability()`](https://appsilon.github.io/shiny.val.tools/reference/svt_traceability.md)
  : Write the verification traceability matrix.

## Manifest helpers

Author and validate `features.yml`.

- [`svt_manifest_template()`](https://appsilon.github.io/shiny.val.tools/reference/svt_manifest_template.md)
  :

  Generate a starter `features.yml` for a graph.

- [`svt_manifest_validate()`](https://appsilon.github.io/shiny.val.tools/reference/svt_manifest_validate.md)
  : Validate a manifest against a graph.

## Inspection helpers

Look at intermediate state without re-running earlier steps.

- [`svt_summary()`](https://appsilon.github.io/shiny.val.tools/reference/svt_summary.md)
  : Per-feature node/edge/warning counts.
- [`svt_warnings()`](https://appsilon.github.io/shiny.val.tools/reference/svt_warnings.md)
  : All warnings, as a flat tibble.
- [`svt_unclaimed()`](https://appsilon.github.io/shiny.val.tools/reference/svt_unclaimed.md)
  : Outputs/observers not claimed by any manifest-declared feature.
- [`svt_inspect()`](https://appsilon.github.io/shiny.val.tools/reference/svt_inspect.md)
  : Pretty-print a single file's AST.

## Example apps

Run the Shiny apps bundled with the package.

- [`svt_run_example()`](https://appsilon.github.io/shiny.val.tools/reference/svt_run_example.md)
  : Run one of the bundled example Shiny apps
