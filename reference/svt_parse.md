# Parse a Shiny app's source.

Enumerates every R file reachable from the app root (the conventional
starting set plus transitive
[`source()`](https://rdrr.io/r/base/source.html) and
[`box::use()`](https://klmr.me/box/reference/use.html) follow), parses
each retaining srcrefs, and returns the parsed bundle as an `svt_parsed`
object.

## Usage

``` r
svt_parse(app_path)
```

## Arguments

- app_path:

  Path to a Shiny app root or a `.zip` archive.

## Value

An `svt_parsed` object — a list with `app_path`, `files` (relative
paths), and `asts` (named list, parse() results keyed by relpath).

## Details

`app_path` may be a directory or a `.zip` archive; archives are
extracted to a session-scoped tempdir and the extracted root is used for
all subsequent steps.
