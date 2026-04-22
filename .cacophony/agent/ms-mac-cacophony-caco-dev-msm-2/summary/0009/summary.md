# Session summary — declare test-user-hel persistent (bd-95ed8d)

## Goal

Promote the ad-hoc test-user spawn on helsinki (agent `7wtvqfrmubjddjdq`) into declarative state so the next node-ctrl reconcile sweep doesn't prune it.

## Bead(s)

- `bd-95ed8d` — Add test-user persistent agent on helsinki targeting caco-web + android — declarative config wiring. Promoted draft → open before claim.

## Before state

- `caco agent new --profile test-user --node helsinki` produced agent `7wtvqfrmubjddjdq` covering bd-a7168d, bd-8c9869, bd-1311fa, bd-78bf58, bd-c69e23, bd-1c0bdd.
- One-shot; not in `.cacophony/agents/cacophony_persistent.yaml`; reconcile sweep would kill it.

## After state

- New `test-user-hel` entry in `.cacophony/agents/cacophony_persistent.yaml` composing `test-user + reflect-session + merge-queue` profiles on `helsinki` with a goal that codifies the operator brief (cap 5 beads/hr, ~30 min speak cadence, reflect at session-end).
- `caco config validate --project-config-dir .cacophony` → `config valid` (8 nodes, 7 projects).

## Diff summary

- Commit: `1e28f97a`
- Files touched: `.cacophony/agents/cacophony_persistent.yaml` (+20 lines, no removals).
- Tests: 0 (declarative config; covered by `caco config validate`).

## Out of scope

- Generalising to per-project test-user dispatch (bd-cc441e tracks broader declarative-state work).
- Sister entries on ms-mac / ms-dev — operator chose helsinki because that's where the surfaces under test live.

## Operator-takeaway

After this lands and node-ctrl reconciles, helsinki will keep a test-user persistent alive across daemon restarts without further ad-hoc spawns. The reflect-session mixin means session-ends leave a trail of UX friction drafts that feeds the polish beads.
