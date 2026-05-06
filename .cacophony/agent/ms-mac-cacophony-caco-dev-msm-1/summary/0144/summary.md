# Session summary — quarantine invalid UTF-8 feed records

## Goal

Fix the ms-mac startup robustness bug where one corrupt non-UTF-8 record in `daemon/feed.jsonl` could block daemon or standalone beads-daemon startup. The session also honored the operator recovery warning after the projects-empty/autowipe incident (`bd-dcafee`) by rehydrating summaries, checking inbox/claims, and preserving local WIP before editing.

## Bead(s)

- `bd-613b3b` — Feed replay should quarantine invalid UTF-8 records instead of aborting startup.
- Related incident tracker: `bd-dcafee` — projects-empty/autowipe recovery guardrail.

## Before state

- Failing tests: none known for this checkout; the incident evidence was operational rather than a local red test.
- Relevant metrics: no claimed bead at first daemon recovery check; later `bd-613b3b` was claimed and hydrated after the beads daemon recovered.
- Context: `SPEC.md` §11.3 already required malformed or non-UTF-8 `daemon/feed.jsonl` records not to crash-loop startup while preserving the raw file. The existing lossy reader avoided UTF-8 read errors but could still parse otherwise valid JSON after replacing invalid bytes with U+FFFD.

## After state

- Failing tests: none observed in targeted validation.
- Relevant metrics: queued validation `tj-fc6b0622` passed 2 focused `bd_613b3b` tests; queued validation `tj-59946f7a` passed `cargo check -p caco-daemon --lib`.
- Context: feed replay now detects lines that contained invalid UTF-8 bytes, logs a quarantine diagnostic, skips only that record during rebuild, and leaves the original `feed.jsonl` bytes intact for forensics. Feed JSONL retention pruning also preserves raw invalid bytes when it has to rewrite the file.

## Diff summary

- Commits: `6275b6a23d`.
- Files touched: `crates/caco-daemon/src/store.rs`; `.cacophony/agent/ms-mac-cacophony-caco-dev-msm-1/summary/pending/summary.md`; `.cacophony/agent/ms-mac-cacophony-caco-dev-msm-1/summary/pending/evidence/2026-05-06-ms-mac-lifecycle-recovery.md`.
- Tests: +2 focused unit regressions / -0 / flipped 0.
- Behavioural delta: `LossyJsonlReader` now carries both raw bytes and decoded text plus an invalid-UTF-8 flag. Startup replay skips flagged records with a first-party diagnostic instead of indexing lossy replacements, and pruning writes raw kept lines back out instead of replacing invalid bytes.

## Embedded artefacts

- `evidence/2026-05-06-ms-mac-lifecycle-recovery.md` — bounded recovery evidence from the ms-mac daemon/bd-daemon outage that motivated this feed hardening slice.

## Operator-takeaway

The feed ledger is now more forensic-safe: one corrupt byte no longer prevents startup, and the original corrupt record remains available for later repair analysis instead of being silently rewritten or lossy-indexed.
