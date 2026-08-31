# Pretty-print a single file's AST.

Uses [`lobstr::ast()`](https://lobstr.r-lib.org/reference/ast.html) when
available; falls back to base
[`print()`](https://rdrr.io/r/base/print.html) on the parsed expression
otherwise.

## Usage

``` r
svt_inspect(file_path)
```

## Arguments

- file_path:

  Path to the R file to inspect.
