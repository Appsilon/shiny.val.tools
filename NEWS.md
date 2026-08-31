# shiny.val.tools 0.0.1

First tagged version, marking the commitment to open-sourcing the package.
No functional change from the preceding development state.

* Per-feature validation artifacts from static analysis of a Shiny app:
  interactive `visNetwork` subgraph widgets, function-centric inventories,
  derived package lists and documentation stubs.
* Support for traditional Shiny (`library()` + `source()`), rhino / `box::use()`
  apps, and hybrids of the two.
* Testing layer: derived test surfaces, optional `shiny::testServer()`
  scaffolds, and a verification-traceability matrix mapping existing tests onto
  features.
* Signable validation summary report and a whole-app index artifact.
