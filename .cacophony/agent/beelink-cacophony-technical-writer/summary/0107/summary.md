# Session summary — summary-pending conflict guidance

## Goal

Implement `bd-5b662b` by turning the rebase friction from the previous technical-writer cycle into durable workflow guidance. The aim was to reduce future conflicts in `.cacophony/agent/<agent>/summary/pending/summary.md` when persistent agents accumulate several commits before they can reintegrate.

## Bead(s)

- `bd-5b662b` — Reduce conflicts in long-lived summary/pending artifacts

## Before state

- Failing tests: none known.
- Relevant metrics: the prior technical-writer reintegration replayed many stale edits to the same pending summary path and required manual conflict resolution; `docs/validate-pages.sh` was the relevant docs validation surface.
- Context: Existing session-recording guidance already said summaries should be authored immediately before reintegration, but it did not explicitly warn long-lived persistent agents not to commit repeated pending-summary updates across a multi-commit stack.

## After state

- Failing tests: none in docs validation.
- Relevant metrics: `./docs/validate-pages.sh` passed with `3313 passed, 0 warnings, 0 failed`; `git diff --check` passed.
- Context: The guidance now tells persistent agents to keep running state in `caco scratch` or messages during long cycles, then write/rewrite `summary/pending/summary.md` once as a final pre-reintegration artifact after the content diff and validation evidence are known.

## Diff summary

- Commits: final landed commit pending from reintegration receipt.
- Files touched: `.cacophony/profiles/session-recording.md`, `.cacophony/profiles/technical-writer.md`, `README.md`, `docs/reintegration-policy.md`, `docs/reintegration-policy.html`, `docs/profiles.html`, `docs/transcription.html`, and this summary.
- Tests: `./docs/validate-pages.sh`; `git diff --check`.
- Behavioural delta: documentation/profile guidance only; no runtime behavior changed. Because `.cacophony/profiles/technical-writer.md` changed, the live persistent technical-writer may need profile-artifact refresh/recreate coordination before the new prompt wording is guaranteed in-session.

## Operator-takeaway

Future long-lived persistent agents should not keep editing and committing the same pending summary throughout a blocked or extended cycle. They should keep transient notes outside git and make one final summary update immediately before reintegration, which should make ordinary rebases much quieter.
