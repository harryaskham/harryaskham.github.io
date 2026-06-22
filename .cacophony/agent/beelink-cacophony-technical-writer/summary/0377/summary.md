# Technical-writer summary — document the zero-Rust-surface reintegration-gate auto-skip

## Goal

Fill a real docs gap found via a drift-check of today's landed beads: bd-45114c
(closed 2026-06-22) made the reintegration gate auto-skip for zero-Rust-surface
lands (docs/version-bump-only), but AGENTS.md / reintegration-policy.md / README did
not mention it at all. Document the new behavior accurately in the canonical gate
description.

## Bead(s)

- No implementation bead — technical-writer drift-catch (the implementing bead
  bd-45114c didn't update the reintegration-gate docs). Routine docs-sync.

## Before state

- AGENTS.md's gate paragraph (line 275) described the merge-queue gating runner and
  the manual `--skip-hooks` override, but said nothing about the bd-45114c automatic
  zero-Rust-surface gate skip. Grep across AGENTS.md/README/reintegration-policy.md
  for the behavior returned nothing — fully undocumented.

## After state

- AGENTS.md's gate paragraph now documents the auto-skip: a reintegration whose diff
  touches only non-source files (docs/**, *.md, CHANGELOG, images) plus
  version-string-only Cargo.toml/Cargo.lock bumps enqueues no gate test job and lands
  fast; detection fails safe (any .rs change, non-version Cargo edits, build scripts,
  or any ambiguity force the gate); the auto-skip still records the --skip-hooks audit
  event with the zero-Rust-surface justification; manual --skip-hooks still works.
  (This also explains why docs-only reintegrations — like the technical-writer's own —
  land without running the cargo gate.)

## Diff summary

- Files touched: AGENTS.md (one sentence added to the gate paragraph; no HTML
  sibling — the gate is documented only in AGENTS.md, not reintegration-policy.md).
- Tests: n/a (docs-only). No AUTOGEN/previous-summary churn.
- Behavioural delta: documentation only.

## Operator-takeaway

The reintegration-gate docs now reflect the bd-45114c zero-Rust-surface auto-skip,
closing a drift gap (the implementing bead didn't document it). Surfaced by a
periodic drift-check of today's closed beads — the kind of catch the technical-writer
role exists for.
