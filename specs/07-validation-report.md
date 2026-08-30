# Validation Summary Report

## Purpose

`index.md` (spec 04) is a navigation hub: it links the packet together so an
auditor can reach the depth. It is not a document anyone signs.

The **validation summary report** is the single artifact a sponsor signs. It
identifies what was analysed, states the method and its limits, rolls up the
declared risk classification, reports what the analysis found, and carries the
approval block. The per-feature stubs remain the detailed traceability and
risk-assessment record underneath it — the report references them, it does not
replace them.

One file: `validation/validation-report.md`. No HTML pair — this is a document
to be printed, circulated and signed; the interactive view is the index.

## What the report must never do

- **Claim validation.** The tool produces evidence. The conclusion is
  human-authored and is never generated.
- **Narrate.** Per spec 00's non-goals, we do not auto-generate prose. Auto
  sections are tables and enumerated facts.
- **Suppress findings.** Limitations and analysis findings are auto sections. A
  sponsor configuration can add sections; it cannot remove or override these.
  A report that can hide `SVT-W001` is not evidence.

## Determinism

The report carries **no generation timestamp**. Same source + manifest +
package version must yield a byte-identical report, per the determinism
contract; a clock reading would break that on every run.

Provenance is established instead by the source revision (git short SHA when
the target is a repository), the analysing package version, and the scope
counts. Effective dates and signature dates belong to the human-authored
Approval section, where they are preserved across regeneration.

## Sections

| # | Section                            | Source                        | Regen |
|---|------------------------------------|-------------------------------|-------|
| 1 | Title                              | system name or target basename| auto  |
| 2 | Document control                   | manifest `report:` block      | auto  |
| 3 | System identification              | target, revision, tool version| auto  |
| 4 | Scope of analysis                  | graph + slice counts          | auto  |
| 5 | Method and limitations             | fixed text (spec 00)          | auto  |
| 6 | Features and risk classification   | manifest + slice              | auto  |
| 7 | Verification status                | testing layer (spec 06)       | auto  |
| 8 | Analysis findings                  | aggregated warning codes      | auto  |
| 9 | Conclusion                         | human                         | preserved |
| 10| Approval                           | human, seeded from `report:`  | preserved |

Sections 1–8 are `auto_keys` and are refreshed on every run. Sections 9 and 10
are preserved verbatim once written, by the same `merge_doc_stub()` mechanism
the per-feature stubs use.

### Verification status while spec 06 is unimplemented

The testing layer is specified but not built. The section states that
verification was **not assessed by this tool** rather than being omitted — an
absent section reads as "nothing to report", which would be misleading in a
signed document. When spec 06 lands, this section carries the coverage
classification and links the traceability matrix.

## Sponsor configuration

Sponsors differ in document control fields, not in what the analysis found. The
manifest therefore gains an optional top-level `report:` block:

```yaml
report:
  title: Validation Summary Report
  document_id: VAL-2026-014
  sponsor: Example Sponsor Ltd
  system_name: Example System
  system_version: 1.2.0
  sop_reference: SOP-CSV-011
  approvers:
    - role: System Owner
      name: A. Person
    - role: Quality Assurance
```

Every field is optional. Absent fields render as `(not declared)` so the gap is
visible on the signed page rather than silently missing. `approvers` seeds the
Approval table's rows on first write; thereafter the section is human-owned.

This is deliberately data, not a template. A template file would have to define
which sections are auto — breaking the merge contract — and would make every
sponsor responsible for tracking sections we add later. Sponsors that need a
different document format entirely should render from `inventory.json`, which
is schema-pinned for exactly that purpose.

## Manifest handling

`read_manifest()` and `normalize_manifest()` preserve the `report:` block
alongside `features` and `modules`. It takes no part in manifest validation
against the graph — it is document metadata, not a slicing instruction, so a
malformed or unknown field never blocks a run.

## Warning codes

None. The report aggregates existing codes; it introduces no new ones.
