# Session summary — Smoke test for fast-test-gate stale-base detection (bd-093b0a)

## Goal

Pin bd-e5eec5's behavior with an automated test.

## Bead(s)

- `bd-093b0a` — Smoke test for fast-test-gate.sh step 6 (P3 task)

## Before state

- bd-e5eec5 added a final stale-base re-check to the gate.
- No automated test verified the behavior.

## After state

- `tests/test-fast-test-gate-stale-base.sh`: bash test that
  spins up a bare upstream + agent + peer clone, has peer land
  a commit, runs the gate in the agent clone, and asserts the
  gate exits non-zero with a stale-base / rebase-needed message.
- Verified passing locally: `PASS: fast-test-gate detects stale-base (rc=1)`.

## Diff summary

- Files touched (+~115 / 0):
  - `tests/test-fast-test-gate-stale-base.sh`: new.

## Verification

- `bash tests/test-fast-test-gate-stale-base.sh` → PASS.

## Operator-takeaway

Smoke test pins the stale-base detection contract. Any future
regression (e.g. someone removing step 6 or breaking the
ancestry check) will be caught.
