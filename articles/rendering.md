# Rendering: visNetwork artifacts

For each feature subgraph and each module subgraph,
[`svt_render()`](https://appsilon.github.io/shiny.val.tools/reference/svt_render.md)
emits one self-contained HTML widget at `<out_dir>/<name>.html`. The
widget opens in any browser without a server. This vignette covers the
visual conventions and what an auditor sees when they open one.

## Goals

The widget is intentionally minimal. An auditor or reviewer should be
able to load it cold and understand the surface in **under 30 seconds**.
Every visual choice — colors, shapes, layout, hover behavior — exists to
support that.

## Node styling

| Type            | Shape        | Color    |
|-----------------|--------------|----------|
| input           | ellipse      | \#4C9BE8 |
| output          | box          | \#5BB85B |
| reactive        | diamond      | \#F0AD4E |
| observer        | triangle     | \#D9534F |
| value           | dot          | \#777777 |
| module_instance | doubleCircle | \#9265DA |

Each node carries a tooltip showing:

- Fully qualified name (`namespace/name`)
- Source location (`file:line`)
- Type
- Any warning codes attached

A legend (built via `visGroups` + `visLegend`) anchors to the right
edge, mapping each node type to its shape and color. The legend is part
of the deliverable, not optional chrome — auditors who open the widget
cold need the type key visible.

## Edge styling

All edges share the same arrow style. Edge thickness is uniform; v1 does
not encode call frequency or any quantitative metric.

The visual arrow points along **data-flow direction** (input → reactive
→ output), opposite to the underlying `depends_on` orientation in the
graph model (where `source` reads `target`). Auditors read the rendered
widget left-to-right as “this input feeds this output”, which matches
their mental model better than “this output depends on this input”. The
data model itself is unchanged — only the rendered arrow is inverted.

Edges render with a `cubicBezier` smooth curve
(`forceDirection = "horizontal"`, `roundness = 0.4`). Curved edges keep
parallel paths visually distinct in the LR layout and are easier to
follow than straight lines when many edges fan out from a single
reactive.

## Layout

Hierarchical, left-to-right:

- Inputs anchor on the left.
- Outputs and observers anchor on the right.
- Reactives flow between in topological order.
- Module instances appear at the layer determined by their I/O position.

Configuration: `solver = "hierarchicalRepulsion"`, `direction = "LR"`.

For graphs with strong cycles or dense connectivity that would produce
overlapping nodes (\>5% node overlap by area), the renderer falls back
to `solver = "forceAtlas2Based"`. The fallback decision is logged in the
widget footer.

We approximate the overlap criterion statically with a density heuristic
— when the edge-to-node ratio exceeds 1.5, the hierarchical layout
begins to crowd at typical canvas sizes, so the fallback fires.

## Module instance rendering

Module instances render as **single nodes** by default — consistent with
the layered-trust principle from
[`vignette("validation-framing")`](https://appsilon.github.io/shiny.val.tools/articles/validation-framing.md).
The tooltip exposes the contract (inputs, outputs, returned reactives).
Clicking the node opens the corresponding module subgraph artifact
(`<module_name>.html`) in a new tab.

v1 does not produce expanded-inline module rendering. Module subgraphs
are first-class artifacts in their own right.

## Hover and selection behavior

The widget enables `highlightNearest` with `algorithm = "hierarchical"`
and an effectively unbounded degree (`from = 50, to = 50`) on hover and
on click. Hovering a node fades all unrelated nodes/edges and highlights
the **complete** transitive upstream chain (everything that feeds this
node) and downstream chain (everything this node feeds). Single-step
neighborhood highlighting hides the cross-feature dependency surface
that auditors most need to see; full-chain highlighting is the explicit
choice.

`nodesIdSelection = TRUE` is also enabled, exposing a node-picker
dropdown so reviewers can select a node by name without scanning the
canvas.

## Warning rendering

Nodes with attached warnings show a small ⚠ badge with the warning count
appended to the label. Hovering reveals the warning codes in the
tooltip. Edges do not carry warnings; warnings live on nodes.

## Widget structure

Each HTML widget contains:

1.  **Header** — feature name, truncated intended use, risk
    classification, regeneration timestamp.
2.  **The visNetwork itself** — the subgraph.
3.  **Footer** — source code commit hash (if `.git` is present), warning
    summary count, layout solver used, link to the corresponding doc
    stub (`<name>.md`) and to the `inventory.json`.

## Determinism

Same inputs → byte-identical output. Node positions are seeded from node
IDs (which are content-addressed). Colors and shapes are fixed by type.
Determinism underwrites the diff-based regeneration semantics described
in
[`vignette("feature-subgraphs")`](https://appsilon.github.io/shiny.val.tools/articles/feature-subgraphs.md).

## What the widget does *not* do

- **Static (PNG/SVG) snapshots** — possibly added later if auditor
  workflows demand it.
- **Inline module expansion** — single-node module rendering is
  sufficient for v1.
- **Diff visualization across versions** — deferred.
- **Theming or per-org customization** — v1 ships one fixed style.
