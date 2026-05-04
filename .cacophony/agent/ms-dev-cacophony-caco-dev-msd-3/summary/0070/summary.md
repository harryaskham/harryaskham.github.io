# Session summary — microVM replicated agent-state observation slice

## Goal

Continue `bd-05c1c5` beyond proof-bead closeout by proving both alpha and beta observe the completed beta worker in first-party agent inventory after closeout.

## Bead(s)

- `bd-05c1c5` — Add hermetic microVM multinode dispatch integration lane

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: fake-ready execute-mode validation reached phase `cleanup` / status `passed` after proof bead closeout was visible on both alpha and beta.
- Context: the harness verified closed bead state on both nodes, but did not yet record a separate replicated agent-inventory assertion after closeout.

## After state

- Failing tests: none from targeted shell validation.
- Relevant metrics: fake-ready execute-mode validation still reaches phase `cleanup` / status `passed`; `vm-replication.log` contains `observer: alpha`, `observer: beta`, and one `completed_beta_worker_matches` entry per observer; real `ms-dev` host still skips with rc 77 because the default Firecracker binary is not on PATH.
- Context: execute mode now runs a distinct `replication` phase after closeout. It polls both guests over VSOCK with `caco bd show --json` and `caco agent list --project <project> --state completed --json`, requiring both nodes to see the proof bead closed and exactly one completed beta worker for that bead.

## Diff summary

- Commits: pending local commit stacked after `c965c61f9`, `fe0ab42b5`, `f49fe535c`, `9124ea487`, and `21581a931`.
- Files touched: `scripts/microvm-dispatch-integration.sh`, `docs/investigations/bd-05c1c5-microvm-dispatch-lane.md`, `README.md`, `SPEC.md`, `AGENTS.md`, `.cacophony/agent/ms-dev-cacophony-caco-dev-msd-3/summary/0070/summary.md`.
- Tests: `bash -n scripts/microvm-dispatch-integration.sh`; `git diff --check`; invalid replication timeout/interval option checks; fake non-execute plan validation confirming replication evidence and metadata; fake-ready execute validation proving phase `cleanup` / status `passed` plus replicated completed-agent inventory records; real-host skip validation rc 77.
- Behavioural delta: `--execute` now has a distinct `replication` phase and `vm-replication.log` after closeout, covering both bead state and completed-agent inventory convergence.

## Operator-takeaway

The scripted microVM lane now covers dispatch enqueue, exact worker pickup, exact worker completion, alpha bare-repo reintegration, alpha/beta proof-bead closeout, and alpha/beta completed-agent inventory observation. The remaining gap for full `bd-05c1c5` acceptance is a real supported Linux/KVM execution with VSOCK and the requested hypervisor available, plus any teardown/retention evidence discovered by that run.
