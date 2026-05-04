# Session summary — microVM closeout/replication observation slice

## Goal

Continue `bd-05c1c5` past direct reintegration observation by proving the scripted proof bead closes and that the closed bead state is visible from both alpha and beta over the hermetic VSOCK guest path.

## Bead(s)

- `bd-05c1c5` — Add hermetic microVM multinode dispatch integration lane

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: fake-ready execute-mode validation reached phase `closeout` after alpha bare-repo reintegration evidence.
- Context: the harness could prove dispatch enqueue, worker pickup, worker completion, and direct reintegration, but intentionally stopped before bead closeout/replication observation.

## After state

- Failing tests: none from targeted shell validation.
- Relevant metrics: fake-ready execute-mode validation now reaches phase `cleanup` with status `passed`; `vm-closeout.log` contains alpha and beta `bd show --json` records with `status: closed`; real `ms-dev` host still skips with rc 77 because the default Firecracker binary is not on PATH.
- Context: execute mode now polls both guests with `caco --config /var/lib/cacophony/config.yaml bd show --project <project> --bead-id <id> --json` and succeeds only after both nodes report the proof bead closed.

## Diff summary

- Commits: pending local commit stacked after `d735145b3`, `bb989e8c2`, `70d82b843`, and `9e72d7e79`.
- Files touched: `scripts/microvm-dispatch-integration.sh`, `docs/investigations/bd-05c1c5-microvm-dispatch-lane.md`, `README.md`, `SPEC.md`, `AGENTS.md`, `.cacophony/agent/ms-dev-cacophony-caco-dev-msd-3/summary/0069/summary.md`.
- Tests: `bash -n scripts/microvm-dispatch-integration.sh`; `git diff --check`; invalid closeout timeout/interval option checks; fake non-execute plan validation confirming closeout evidence and metadata; fake-ready execute validation proving phase `cleanup` / status `passed` and closed bead records; real-host skip validation rc 77.
- Behavioural delta: `--execute` no longer stops at the closeout boundary; it records `vm-closeout.log` and reaches cleanup after alpha/beta closed-state convergence.

## Operator-takeaway

The scripted microVM lane now covers dispatch enqueue, exact worker pickup, exact worker completion, direct reintegration into the alpha-projected bare repo, and alpha/beta proof-bead closeout observation. Remaining risk is proving the full flow on a supported Linux/KVM host with the required VSOCK and hypervisor pieces installed.
