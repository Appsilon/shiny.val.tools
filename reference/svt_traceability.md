# Write the verification traceability matrix.

Emits `traceability.json` (`schema_version` 1.0) and `traceability.md` —
the auditor-facing matrix — into `out_dir`. Only the `## Reviewers`
section of the markdown is human-authored; it is preserved across
regenerations like every other stub.

## Usage

``` r
svt_traceability(features, coverage, out_dir = "validation")
```

## Arguments

- features:

  An `svt_features` object.

- coverage:

  An `svt_test_coverage` object.

- out_dir:

  The validation directory.

## Value

The two paths written, invisibly.

## Details

Staleness (SVT-W307) is detected here, because this is the step that
both reads the previous `traceability.json` and replaces it: a surface
whose `surface_hash` moved while every covering test file's md5 stayed
put is flagged as possibly stale. The check is deliberately conservative
— an md5 change is not evidence the test was updated for the right
reason, only that somebody touched it — so the warning clears on any
edit.
