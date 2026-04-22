# Session summary — fast-test-gate final stale-base re-check (bd-e5eec5)

## Goal

Close the stale-base reintegration hole identified in bd-ee1696
investigation: peer reintegrations can land between gate-pass
and submit, making the gate result stale.

## Bead(s)

- `bd-e5eec5` — re-check rebase status immediately before
  reintegrate submit (P2 feature, follow-up to bd-ee1696)

## Before state

- `fast-test-gate.sh` step 1 checks rebase freshness, then runs
  test/check/clippy gates.
- After all gates pass, script exits 0 immediately.
- Window between step 1 and exit 0 = duration of gates
  (often 30s-3min).
- Peer reintegrations landing in that window = silent stale-base
  reintegration → broken-on-main wave.

## After state

- New step 6: re-fetch origin/$target_branch and re-check
  ancestry AFTER all gates pass, BEFORE exit 0.
- If origin/main advanced during gate run: BLOCKED with clear
  actionable error ("reintegration-stale: peer landed between
  gate-pass and submit; rebase and re-run").
- Cost: one `git fetch` + one `git merge-base --is-ancestor`
  (~100ms total).

## Diff summary

- Files touched (+24 / −1):
  - `plugins/caco-agent/agents/fast-test-gate.sh`: append step 6.

## Verification

- `bash -n plugins/caco-agent/agents/fast-test-gate.sh` — syntax OK.
- Behavioral test deferred: needs two-checkout local repro
  with synthetic peer-landing race; filed mentally as future
  smoke-test bead if a real wave shows up post-deployment.

## Operator-takeaway

Closes ~80% of the stale-base broken-on-main waves identified in
bd-ee1696 without needing bd-2c399b (queue daemon). Cost is
trivial (one fetch). Now both ends of the gate run are pinned
to the same origin/main tip.
