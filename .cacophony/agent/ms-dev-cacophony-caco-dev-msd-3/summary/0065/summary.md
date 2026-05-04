# Session summary — microVM worker-pickup slice

## Goal

Continue `bd-05c1c5` past dispatch enqueue by proving the beta guest can report exactly one queued scripted worker pickup for the proof bead over VSOCK, then stop truthfully at the still-unimplemented worker-completion/closeout boundary.

## Bead(s)

- `bd-05c1c5` — Add hermetic microVM multinode dispatch integration lane

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: execute-mode fake-ready validation reached `worker-pickup` after VSOCK readiness, runtime projection, daemon status, beads routing, and alpha-side dispatch enqueue.
- Context: the harness created the proof bead and queued beta dispatch, but worker pickup was still only a future execution-plan step.

## After state

- Failing tests: none from targeted shell validation.
- Relevant metrics: fake-ready execute-mode validation now reaches phase `worker-completion` after creating `vm-worker-pickup.log` with exactly one beta agent match for the dispatched bead; real `ms-dev` host still skips with rc 77 because the default Firecracker binary is not on PATH.
- Context: `--execute` now polls beta over VSOCK with `caco agent list --json` until exactly one agent for the dispatched bead is visible on node `beta`, controlled by `--worker-pickup-timeout-secs` and `--worker-pickup-interval-secs`.

## Diff summary

- Commits: pending local commit (`bd-05c1c5: observe microvm worker pickup`).
- Files touched: `scripts/microvm-dispatch-integration.sh`, `docs/investigations/bd-05c1c5-microvm-dispatch-lane.md`, `README.md`, `SPEC.md`, `AGENTS.md`, `.cacophony/agent/ms-dev-cacophony-caco-dev-msd-3/summary/0065/summary.md`.
- Tests: `bash -n scripts/microvm-dispatch-integration.sh`; `git diff --check`; invalid worker-pickup timeout/interval option checks; fake non-execute plan validation confirming worker-pickup metadata; fake-ready execute validation with fake `caco`, `ip`, `id`, `ssh`, `socat`, runner assets, and `CACO_MICROVM_DISPATCH_TEST_ASSUME_VHOST_VSOCK=1` proving phase `worker-completion` plus worker-pickup evidence; real-host skip validation rc 77.
- Behavioural delta: execute mode now records worker-pickup evidence and moves the explicit remaining boundary from worker pickup/completion to worker completion/closeout.

## Operator-takeaway

The microVM dispatch lane now observes the target-side beta worker pickup for the proof bead. The remaining full proof is scripted worker completion, direct reintegration into the local bare remote, bead closeout, replicated observation, and final teardown/retention evidence.
