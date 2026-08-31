Static-analysis tooling that produces per-feature validation artifacts
for Shiny apps in pharma/GxP contexts. Slices a Shiny app's reactive
graph into output-rooted feature subgraphs and emits, for each feature,
an interactive subgraph widget, a function-centric inventory, a derived
package list, and a documentation stub. Supports traditional Shiny
('library' + 'source') and rhino ('box::use') patterns.
