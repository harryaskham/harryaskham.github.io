# Session summary — Audio transcription 502 diagnostic severity

## Goal

Prevent non-fatal `POST /api/v1/audio/transcription -> 502` route failures from re-entering crash/error channels after the earlier crash-log mirror fix, while keeping the failures visible as daemon/feed diagnostics for debugging.

## Bead(s)

- `bd-89c1aa` — Audio transcription 502 recurs after bd-45b572 close

## Before state

- Failing tests: none known before this slice.
- Relevant metrics: log-monitor reported one post-close audio transcription 502 recurrence in the 2026-05-09T01:36:12Z to 2026-05-09T01:51:12Z sweep.
- Context: `daemon:http` non-fatal records already avoided stderr mirroring, but transcription 502s still used `severity:error`, which keeps them in Errors/crash-adjacent diagnostics despite being routine upstream/provider route failures.

## After state

- Failing tests: none from focused validation.
- Relevant metrics: queued validation passed before rebase (`tj-1124c910`, `tj-743410f5`, `bj-3b274bc6`) and after rebase (`tj-e7c0cafa`, `tj-606328c9`, `bj-ce3fd41f`).
- Context: telemetry still reports transcription 502s, but classifies the exact observed `/api/v1/audio/transcription` 502 class as `warning`; unrelated daemon HTTP failures and transcription 500s remain `error`.

## Diff summary

- Commits: `ad30670dc5`.
- Files touched: `crates/caco-daemon/src/lib.rs`.
- Tests: +1 daemon unit test / -0 / flipped 0.
- Behavioural delta: non-fatal audio transcription 502s stay in daemon log/feed diagnostics at WARN severity and no longer populate error/crash channels reserved for actionable daemon failures.

## Operator-takeaway

This narrows the previous crash-log hygiene fix: transcription upstream 502s are still observable, but they now behave like bounded diagnostics rather than fresh daemon crash/error evidence.
