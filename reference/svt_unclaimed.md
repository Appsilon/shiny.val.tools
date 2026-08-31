# Outputs/observers not claimed by any manifest-declared feature.

Returns the tibble computed at slice time (SVT-W103). Only meaningful
when an explicit manifest was supplied — otherwise the default rule
trivially claims every root and the table is empty.

## Usage

``` r
svt_unclaimed(features)
```

## Arguments

- features:

  An `svt_features` object.
