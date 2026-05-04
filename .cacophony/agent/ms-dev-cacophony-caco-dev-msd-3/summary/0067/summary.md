# Session summary — microVM hermetic guest git remote slice

## Goal

Continue `bd-05c1c5` by removing the next hermeticity gap before direct reintegration: generated guest configs must not point at the host workdir's bare repository path, because the isolated two-VM proof must exercise guest-only networking and runtime projection.

## Bead(s)

- `bd-05c1c5` — Add hermetic microVM multinode dispatch integration lane

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: fake-ready execute-mode validation reached phase `reintegration` after observing exactly one completed beta worker for the proof bead.
- Context: the seeded bare repository existed under the host evidence workdir and was written directly into generated guest project config, which would not be reachable inside isolated guests without an explicit projection/share path.

## After state

- Failing tests: none from targeted shell validation.
- Relevant metrics: generated guest project configs now use `git://alpha/project.git`; alpha runtime projection includes the seeded bare repository under `/var/lib/cacophony/git/project.git` and starts a guest-local `git daemon` with receive-pack enabled during projection apply; fake-ready execute-mode validation still reaches phase `reintegration`.
- Context: this keeps direct reintegration hermetic to the alpha/beta VM network rather than depending on production GitHub, host `/tmp`, host daemon state, or production ports.

## Diff summary

- Commits: pending local commit stacked after `70d82b843` and `9e72d7e79`.
- Files touched: `scripts/microvm-dispatch-integration.sh`, `docs/investigations/bd-05c1c5-microvm-dispatch-lane.md`, `SPEC.md`, `AGENTS.md`, `.cacophony/agent/ms-dev-cacophony-caco-dev-msd-3/summary/0067/summary.md`.
- Tests: `bash -n scripts/microvm-dispatch-integration.sh`; `git diff --check`; fake non-execute plan validation confirming `project_remote == git://alpha/project.git`, generated alpha config uses that URL, and the projection manifest documents the alpha git daemon; fake-ready execute validation proving phase `reintegration` still works and reports the guest remote; real-host skip validation rc 77.
- Behavioural delta: generated guest configs and JSON reports now use the guest-reachable remote URL, while the host-seeded bare repository becomes alpha projection input/evidence.

## Operator-takeaway

The lane is one step closer to true isolated reintegration: the scripted worker can complete against a guest-reachable remote endpoint. Remaining work is to verify the reintegration result in the alpha-projected bare repository, close the proof bead, observe replicated state, and finalize teardown/retention evidence.
