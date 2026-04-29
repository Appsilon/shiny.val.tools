# Rendering: visNetwork artifacts

## Purpose

Defines the visNetwork output format, color and shape conventions, layout strategy, module instance rendering, and embedded metadata.

## Output

For each feature subgraph and each module subgraph, one self-contained HTML widget at `validation/<slug>.html`. The widget opens in any browser without a server.

In addition, the validation directory contains exactly one **index** artifact pair (`validation/index.md` + `validation/index.html`) that summarises the whole app and links every module/feature artifact together. See "Index artifact" below.

### Filename slug rule

Module identities use forward slashes (e.g. `app/view/mod_card`). On disk we slugify by replacing `/` with `--`:

```
slug(name) = gsub("/", "--", name, fixed = TRUE)
```

So `app/view/mod_card` → `validation/app--view--mod_card.{md,html}` and the inventory lives at `validation/app--view--mod_card/inventory.json`. Feature names without slashes pass through unchanged. The slug is purely a filesystem-level transformation:

- Doc stub `# Module:` headings still print the original identity (`Module: app/view/mod_card`).
- Widget `main` text still uses the original identity.
- `inventory.json` `feature` field still uses the original identity.
- Internal links inside the doc stub and widget click-through `url`s use the slugged form (since they are filesystem references).

`--` is the chosen separator because module names already use `_` for word separators (`mod_card`); `__` would visually conflict with that and `.` collides with file extensions.

### Self-contained HTML guarantee

Every `<slug>.html` is a single file with all dependencies (vis.js, htmlwidgets, fonts, CSS) inlined via base64. No sibling `<slug>_files/` directory may remain after `svt_render()` returns successfully. `htmlwidgets::saveWidget(selfcontained = TRUE)` (htmlwidgets ≥ 1.6) does the inlining; the renderer additionally `unlink()`s any sibling `<slug>_files/` directory that htmlwidgets leaves behind as a build straggler. This contract underwrites distribution: a single `.html` per artifact is what auditors archive and email.

## Node styling

| Type            | Shape        | Color   |
| --------------- | ------------ | ------- |
| input           | ellipse      | #4C9BE8 |
| output          | box          | #5BB85B |
| reactive        | diamond      | #F0AD4E |
| observer        | triangle     | #D9534F |
| value           | dot          | #777777 |
| module_instance | doubleCircle | #9265DA |

Each node carries a tooltip showing:

- Fully qualified name (`namespace/name`)
- Source location (`file:line`)
- Type
- Any warning codes attached

## Edge styling

All edges share the same arrow style. Edge thickness is uniform; v1 does not encode call frequency or any quantitative metric.

The visual arrow points along **data-flow direction** (input → reactive → output), opposite to the underlying `depends_on` orientation in the graph model (spec 01, where `source` reads `target`). Auditors read the rendered widget left-to-right as "this input feeds this output," which matches their mental model better than "this output depends on this input." The data model itself is unchanged — only the rendered arrow is inverted.

Edges render with a `cubicBezier` smooth curve (`forceDirection = "horizontal"`, `roundness = 0.4`). Curved edges keep parallel paths visually distinct in the LR layout and are easier to follow than straight lines when many edges fan out from a single reactive.

## Layout

Hierarchical, left-to-right:

- Inputs anchor on the left.
- Outputs and observers anchor on the right.
- Reactives flow between in topological order.
- Module instances appear at the layer determined by their I/O position.

Configuration: `solver = "hierarchicalRepulsion"`, `direction = "LR"`. For graphs with strong cycles or dense connectivity that produce overlapping nodes (>5% node overlap by area), fall back to `solver = "forceAtlas2Based"`. The fallback decision is logged in the widget footer.

## Module instance rendering

Module instances render as **single nodes** by default — consistent with the layered-trust principle. The tooltip exposes the contract (inputs, outputs, returned reactives). Clicking the node opens the corresponding module subgraph artifact (`validation/<slug>.html`, where `<slug>` is the module identity slugified per the filename slug rule above) in a new tab.

v1 does not produce expanded-inline module rendering. Module subgraphs are first-class artifacts in their own right.

## Hover and selection behavior

The widget enables `highlightNearest` with `algorithm = "hierarchical"` and an effectively unbounded degree (`from = 50, to = 50`) on hover and on click. Hovering a node fades all unrelated nodes/edges and highlights the **complete** transitive upstream chain (everything that feeds this node) and downstream chain (everything this node feeds). Single-step neighborhood highlighting hides the cross-feature dependency surface that auditors most need to see; full-chain highlighting is the explicit choice.

`nodesIdSelection = TRUE` is also enabled, exposing a node-picker dropdown so reviewers can select a node by name without scanning the canvas.

A legend (built via `visGroups` + `visLegend`) anchors to the right edge, mapping each node type to its shape and color per the table above. The legend is part of the deliverable, not optional chrome — auditors who open the widget cold need the type key visible.

## Warning rendering

Nodes with attached warnings show a small badge in the upper right with the warning count. Hovering reveals the codes. Edges do not carry warnings; warnings live on nodes.

## Widget structure

Each HTML widget contains:

1. A header — feature name, truncated intended use, risk classification, regeneration timestamp.
2. The visNetwork itself.
3. A footer — source code commit hash (if `.git` is present), warning summary count, layout solver used, link to the corresponding doc stub (`<name>.md`) and to the `inventory.json`.

The widget is intentionally minimal. An auditor or reviewer should be able to load it cold and understand the surface in under 30 seconds.

## Index artifact

`svt_render()` always emits one `index.md` and one `index.html` at the root of `out_dir`. The index is the navigation hub for the validation packet: it summarises the whole app and links every per-feature/per-module artifact together. Auditors open the index first; per-feature artifacts are the depth.

### `index.md` sections

Section order (mirrors per-feature stubs where it makes sense):

1. **Title** — `# <app_name> Validation`, where `<app_name>` is `basename(app_path)`.
2. **Summary** — one row per metric: features, modules, files parsed, packages used, total warnings. Plus the commit hash from `git_commit_short()` when available.
3. **App overview** — `[Architecture diagram](index.html)` link.
4. **Features** — markdown table: `| Name | Intended use | Risk | Nodes | Warnings | Doc | Widget |`. Each name links to the feature's `<slug>.md`; Doc and Widget are explicit relative links to `<slug>.md` and `<slug>.html`.
5. **Modules** — markdown table: `| Name | Inputs | Outputs | Returned | Warnings | Doc | Widget |`. Inputs/Outputs/Returned are counts, not full lists (the per-module stub holds those).
6. **Module relationships** — bullet list of "`<parent>` instantiates `<child>`" lines, derived from `module_instance` nodes whose `name` field matches a module identity. Empty if the app has no inter-module instantiation.
7. **Aggregate warnings** — markdown table: `| Code | Count | Description |`, sorted by code.
8. **Reviewers** — whole-app sign-off block, identical placeholder shape to per-feature stubs (`- Developer: __________________ Date: __________` etc.). Preserved across regeneration via the same merge mechanism per-feature stubs use.

The auto-fill / non-auto split mirrors per-feature stubs: only `## Reviewers` is non-auto. All other sections refresh on every render.

### `index.html` — architecture overview

A visNetwork that renders **only the architecture**, not the full reactive graph union. Nodes:

- One node per **module** (using the `module_instance` shape/colour — `doubleCircle`, `#9265DA`).
- One node per **top-level feature** (using the `output` shape/colour — `box`, `#5BB85B`).

Edges:

- **Parent module → child module** when the parent's subgraph contains a `module_instance` node whose `name` resolves to a module identity in the slice.
- **Feature → module** when the feature's subgraph node set intersects a module's namespace.

Click handler is the same `selectNode` JS used in per-feature widgets: clicking a node opens the corresponding `<slug>.html` in a new tab.

The architecture-only view is deliberate. The full union of every reactive across the app would be unreadable for non-trivial apps and would duplicate content the per-module widgets already render. The index is the navigation layer; depth lives in the per-module artifacts.

Layout, hover behaviour, legend, and footer follow the same conventions as feature widgets (LR hierarchical with `forceAtlas2Based` fallback on density >1.5; `highlightNearest` with full-chain hierarchical algorithm; legend on the right; footer with commit hash and warning count).

## Determinism

Same inputs → byte-identical output. Node positions are seeded from node IDs (which are content-addressed per spec 01). Colors and shapes are fixed by type. Determinism underwrites the diff-based regeneration semantics in spec 02. The index artifact is also deterministic — its tables are sorted by name and its architecture graph nodes/edges are sorted before being passed to visNetwork.

## Out of scope for this spec

- Static (PNG/SVG) snapshot rendering — possibly added later if auditor workflows demand it.
- Inline module expansion — single-node module rendering is sufficient for v1.
- Diff visualization across versions — deferred per overview non-goals.
- Theming or per-org customization — v1 ships one fixed style.
