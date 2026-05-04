# Session summary — microVM cleanup receipt slice

## Goal

Continue `bd-05c1c5` after replicated bead/agent observation by recording deterministic cleanup evidence for preserved microVM dispatch-lane runs.

## Bead(s)

- `bd-05c1c5` — Add hermetic microVM multinode dispatch integration lane

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: fake-ready execute-mode validation reached phase `cleanup` / status `passed` after dispatch, worker pickup/completion, reintegration, bead closeout, and replicated completed-agent inventory observation.
- Context: the harness already tore down guests and network resources through the EXIT trap, but preserved runs did not have a structured receipt proving what cleanup attempted and which resource flags were cleared.

## After state

- Failing tests: none from targeted shell validation.
- Relevant metrics: fake-ready execute-mode validation still reaches phase `cleanup` / status `passed`; preserved `vm-cleanup.json` records `keep: true`, guest stop requested, network teardown requested, `guests_started_after: false`, and `network_setup_applied_after: false`.
- Context: report evidence now includes `vm_cleanup_receipt`, and `--keep` runs preserve `evidence/vm-cleanup.json` after stop/teardown helpers run.

## Diff summary

- Commits: pending local commit stacked after `45156c617`, `c965c61f9`, `fe0ab42b5`, `f49fe535c`, `9124ea487`, and `21581a931`.
- Files touched: `scripts/microvm-dispatch-integration.sh`, `docs/investigations/bd-05c1c5-microvm-dispatch-lane.md`, `README.md`, `SPEC.md`, `AGENTS.md`, `.cacophony/agent/ms-dev-cacophony-caco-dev-msd-3/summary/0071/summary.md`.
- Tests: `bash -n scripts/microvm-dispatch-integration.sh`; `git diff --check`; fake non-execute report validation confirming `vm_cleanup_receipt`; fake-ready execute validation confirming preserved cleanup receipt fields.
- Behavioural delta: cleanup now writes a structured receipt before optional workdir removal, making preserved evidence bundles explicit about teardown attempts and post-cleanup flags.

## Operator-takeaway

The scripted microVM lane now records cleanup/teardown receipt evidence in addition to dispatch, worker, reintegration, closeout, and replication proofs. Full `bd-05c1c5` acceptance still needs a real supported Linux/KVM execution with VSOCK and the requested hypervisor available.
