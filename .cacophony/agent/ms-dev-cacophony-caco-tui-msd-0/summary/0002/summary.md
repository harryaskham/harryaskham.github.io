# Session summary — SPEC/README/AGENTS docs for TUI Suggestions generate

## Goal

Close the documentation-contract gap that my bd-2ae6f0 landing left open: the
TUI Suggestions generate-from-prompt affordance was shipped without updating the
normative docs. A peer (caco-tui-md2-1) caught it and filed bd-3dfcb4.

## Bead(s)

- `bd-3dfcb4` — SPEC 25.6 not updated for TUI Suggestions generate-from-prompt
  (landed in bd-2ae6f0) (task, P3, docs).
- (related: `bd-2ae6f0` — the implementation; parent epic `bd-a84d20`)

## Before state

- SPEC 25.6 TUI bullet, README Suggestions section, and the AGENTS.md suggest
  note all described the Suggestions browser as read-only with no mention of the
  generate-from-prompt action shipped in bd-2ae6f0.

## After state

- SPEC 25.6, README Suggestions section, and the AGENTS.md suggest-subsystem note
  now document the in-TUI `g` generate-from-prompt overlay (POSTs the node-local
  generate endpoint, prepends + selects the new set, only proposes and never
  runs, node-local with no forward-if-not-primary). The three are consistent per
  the SPEC 23.5 documentation contract.

## Diff summary

- Code/content commit: created at reintegration; final landed squash SHA from
  the reintegration receipt.
- Files touched: `SPEC.md` (25.6 TUI bullet), `README.md` (Suggestions section),
  `AGENTS.md` (suggest-subsystem note). Docs-only; no code/tests.
- Behavioural delta: none (documentation only).

## Operator-takeaway

A surface change that ships without its SPEC/README update is an easy miss under
reintegration-storm pressure; the peer-review catch (bd-3dfcb4) is the safety
net working. The deeper fix would be a lint/test that flags suggest-surface code
changes lacking a SPEC §25 / README diff, but that is out of scope here.
