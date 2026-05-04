# Session summary — microVM worker-completion slice

## Goal

Continue `bd-05c1c5` past target-side pickup by observing deterministic beta scripted-worker completion through the guest agent surface over VSOCK, then stop truthfully at the still-unimplemented reintegration/closeout boundary.

## Bead(s)

- `bd-05c1c5` — Add hermetic microVM multinode dispatch integration lane

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: execute-mode fake-ready validation reached `worker-completion` after creating `vm-worker-pickup.log` with exactly one beta agent match for the dispatched bead.
- Context: the harness proved target worker pickup but did not yet poll for a completed scripted worker before its explicit boundary.

## After state

- Failing tests: none from targeted shell validation.
- Relevant metrics: fake-ready execute-mode validation now reaches phase `reintegration` after creating `vm-worker-completion.log` with exactly one completed beta agent match for the dispatched bead; real `ms-dev` host still skips with rc 77 because the default Firecracker binary is not on PATH.
- Context: `--execute` now polls beta with `caco agent list --project <project> --state completed --json` over VSOCK, filters for the dispatched bead on node `beta`, fails on duplicates with `duplicate_worker_completion`, and records timeout/interval controls via `--worker-completion-timeout-secs` and `--worker-completion-interval-secs`.

## Diff summary

- Commits: pending local commit stacked after `9e72d7e79`.
- Files touched: `scripts/microvm-dispatch-integration.sh`, `docs/investigations/bd-05c1c5-microvm-dispatch-lane.md`, `README.md`, `SPEC.md`, `AGENTS.md`, `.cacophony/agent/ms-dev-cacophony-caco-dev-msd-3/summary/0066/summary.md`.
- Tests: `bash -n scripts/microvm-dispatch-integration.sh`; `git diff --check`; invalid worker-completion timeout/interval option checks; fake non-execute plan validation confirming worker-completion metadata; fake-ready execute validation with fake `caco`, `ip`, `id`, `ssh`, `socat`, runner assets, and `CACO_MICROVM_DISPATCH_TEST_ASSUME_VHOST_VSOCK=1` proving phase `reintegration` plus worker-completion evidence; real-host skip validation rc 77.
- Behavioural delta: execute mode now records worker-completion evidence and moves the explicit remaining boundary from worker completion/closeout to reintegration/closeout.

## Operator-takeaway

The microVM dispatch lane now observes the target-side beta worker completion for the proof bead. The remaining full proof is direct reintegration into the local bare remote, bead closeout, replicated observation, and final teardown/retention evidence.
