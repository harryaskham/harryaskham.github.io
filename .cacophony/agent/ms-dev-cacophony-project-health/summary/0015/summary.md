# Session summary — Persistent-worker health notes

## Goal

Add a lightweight, first-party convention for persistent workers to record bounded outage windows in health notes when daemon, inbox, bead, or assigned-surface checks are temporarily unreachable, without encouraging broad duplicate outage beads or local repair attempts.

## Bead(s)

- `bd-b8394b` — Track persistent-worker outage windows in caco health notes

## Before state

- Failing tests: none known for this documentation/profile slice.
- Relevant metrics: persistent workers had guidance to use scratch for handoff state, and `log-monitor` had a bounded scratch-unavailable fallback, but generic persistent-worker instructions did not tell workers how to record outage windows for controller correlation.
- Context: the bead was filed from a caco-transcription session where repeated bounded daemon/bead/inbox checks failed but there was no durable lightweight outage marker tied to the affected persistent agent/surface.

## After state

- Failing tests: none introduced by this slice.
- Relevant metrics: queued `cargo test -p caco-profile persistent_instructions_mention_health_notes --lib` passed as `tj-9483b78d`; queued `cargo run -p caco-profile --bin caco-docs-gen -- --check` passed as `tj-7f8951db`; `docs/validate-pages.sh` passed with 3313 checks; `git diff --check` passed.
- Context: persistent worker common instructions, project-health, and caco-transcription profiles now describe compact scratch health notes and bounded fallback reconciliation for outage receipts.

## Diff summary

- Commits: code commit `405a0ecfe` plus this summary commit
- Files touched: `crates/caco-profile/src/persistent_instructions.txt`, `crates/caco-profile/src/instructions.rs`, `.cacophony/profiles/project-health.md`, `.cacophony/profiles/caco-transcription.md`, `docs/profiles.html`
- Tests: +1 profile-instruction assertion / -0 / flipped 0
- Behavioural delta: persistent agents are now instructed to append `health:<agent-id>` scratch entries for repeated bounded outage windows, include surface/error/action/recovery details, connect the note to the agent, and use only a bounded agent-directory fallback when scratch is unavailable.

## Operator-takeaway

Controllers now have a low-noise convention to correlate persistent-worker outage windows without every affected worker filing a broad runtime bug or attempting local service repair during recovery.
