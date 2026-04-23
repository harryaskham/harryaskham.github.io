# Session summary 0000 — bd-0977f7 slice 1: single-owner protocol doc

## Goal

Land a documented tie-break protocol so agents seeing
competing `[SOLE-OWNER]` broadcasts can decide locally whether
to claim or stand down — preventing the three-way race that
required operator intervention during tonight's bead-loss
incident.

## Bead(s)

- `bd-0977f7` slice 1 — docs only. Slices 2-5
  (`--sole-owner-claim` flag, daemon auto-resolve, profile
  mixin, regression test) deferred to follow-ons.

## Before state

- No documented protocol.
- Three agents declared sole-ownership of the bead-store
  incident within five minutes; resolution required operator
  + cluster-ctrl handoff + node-ctrl-hel relay.

## After state

- New `docs/protocols/single-owner-incidents.md` (~95 lines).
- Five-rule tie-break order: operator > home-node controller
  > controller-role-over-worker > most-recent-claim >
  lexicographic.
- "Stand down" obligations enumerated: ack publicly, stop
  touching subsystem, continue unrelated queue work, forward
  analysis as input.
- Out-of-scope follow-on slices listed explicitly so future
  claimants know what's left.

## Diff summary

- Files (1): `docs/protocols/single-owner-incidents.md` (new,
  +95 lines).
- No code changes; no build/clippy needed.

## Operator-takeaway

Next time an "ONLY ONE AGENT" directive is issued, agents
have a written rule to apply locally. If three controllers
race again, rule 4 (most-recent claim wins) gives a
deterministic resolution without operator intervention. Slice
2 (machine-parseable `[SOLE-OWNER]` broadcast format) +
slice 3 (daemon-side auto-resolve) will close the loop fully.
