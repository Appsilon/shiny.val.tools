# Run one of the bundled example Shiny apps

The package ships small Shiny apps under `inst/extdata/`, each
exercising a specific source pattern. This runs one of them so you can
see the app the analysis artifacts describe.

## Usage

``` r
svt_run_example(
  app = c("traditional_basic", "traditional_with_source", "rhino_basic",
    "rhino_multi_module"),
  ...
)
```

## Arguments

- app:

  Name of the example app to run. One of `"traditional_basic"`,
  `"traditional_with_source"`, `"rhino_basic"` or
  `"rhino_multi_module"`.

- ...:

  Passed on to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Value

Whatever [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html)
returns, invisibly for its side effect of running the app.

## Details

Only the apps that actually start are offered. `cycle_app` (deliberately
circular [`source()`](https://rdrr.io/r/base/source.html)) and
`box_in_R` (no `shinyApp()` entrypoint) exist to exercise static
analysis and are not runnable.

The app is copied to a temporary directory before running, so edits made
while exploring never touch the installed package.

## Examples

``` r
if (interactive()) {
  svt_run_example("rhino_multi_module")
}
```
