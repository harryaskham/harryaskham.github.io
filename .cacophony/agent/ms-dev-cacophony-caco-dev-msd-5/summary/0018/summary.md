# Session summary — bd-4c1ac4 SPEC cacophony-state artefact contract

## Goal

Close the SPEC documentation gap left by bd-f2d9e2: codify the
`.cacophony/agent/<agent-id>/**` artefact prefix contract in SPEC.md
so profile authors stop inventing parallel filesystem conventions.

## Bead(s)

- `bd-4c1ac4` — SPEC.md should describe the cacophony-state artefact
  contract (any `.cacophony/agent/<id>/**` subdir)

## Before state

- The artefact-path contract (post-bd-f2d9e2 broadening) was only
  documented in the `crates/caco-daemon/src/cacophony_state.rs`
  module preamble.
- SPEC.md §17 (Reintegration Policy) had no description of the
  cacophony-state artefact prefix — profile authors had no
  authoritative reference and could invent parallel filesystem
  conventions outside `.cacophony/agent/<id>/` and have artefacts
  silently dropped during reintegrate.

## After state

- SPEC.md gains §17.3.1 "cacophony-state Artefact Contract" with:
  - Explicit prefix contract (`.cacophony/agent/<agent-id>/`)
  - Subdir naming is open (`summary/`, `session/`, `reflect/`,
    `scratch/`, `traces/`, `bench/`, `coverage/`, …)
  - Per-agent isolation rule
  - `caco agent artefacts` discovery surface
  - Recorded reintegration mode interplay (bd-d48494, bd-ae8de9)
  - Implementation reference back to `cacophony_state.rs`
- Three concrete recommendations for profile authors close out the
  section.

## Diff summary

- 1 file modified, 49 insertions:
  - `SPEC.md` — new §17.3.1.
- Tests: cargo test-small green (docs-only change; verified the
  workspace still builds + invariant tests pass).

## Operator-takeaway

Closes the documentation gap that bd-f2d9e2 opened up — the matcher
broadening now has an authoritative SPEC reference. Future profile
authors writing reflect-session / bench / trace mixins will know to
write under `.cacophony/agent/<agent-id>/<their-subdir>/` instead of
discovering the silent-drop the hard way.
