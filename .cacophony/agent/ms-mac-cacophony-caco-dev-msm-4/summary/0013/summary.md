# Slice 11 — bd-1afb2e: cross-surface Beads UX/content polish contract

## Goal

Define a shared design/copy contract so caco-web, TUI, and Android Beads surfaces converge on consistent UX instead of fixing symptoms independently.

## Bead(s)

- **bd-1afb2e** (task, P2) — Define cross-surface Beads UX/content polish contract.

## Before state

- Three surfaces (web/TUI/Android) each rendering beads with different label ordering, chip styles, confirmation copy, and metadata layouts.
- Awkward copy like bare "Close bead bd-XXXXXX?" without title or consequence.
- Provenance labels (`discovered-via-agent:*`, `test-user`) dominating primary label display.
- No shared checklist for implementation beads to validate against.

## After state

- `docs/design/beads-ux-contract.md` — 9-section contract covering:
  - §1 Status display labels (never raw enum variants)
  - §2 Priority badge colours
  - §3 Label hierarchy (primary/secondary/provenance), chip styling, ordering
  - §4 Confirmation copy replacements (close, claim, unclaim, dispatch)
  - §5 Empty states (no beads, no match, no assignee, no description)
  - §6 Detail view metadata order and actions grouping
  - §7 List view column order and row density
  - §8 Implementation checklist (14 items)
  - §9 Surface-specific notes (web CSS vars, TUI Ratatui spans, Android Material chips)

## Diff summary

```
 docs/design/beads-ux-contract.md | 180+
 1 file changed
```

## Operator-takeaway

The contract at `docs/design/beads-ux-contract.md` is ready for bd-5c0367 (web), bd-23ecea (TUI), and bd-cee357 (Android) implementers to reference. Key decision: provenance labels hidden by default, confirmation copy includes bead title + consequence, actions grouped at bottom of detail.
