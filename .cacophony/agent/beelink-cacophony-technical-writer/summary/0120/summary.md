# Session summary — stale diagnostic stderr replay docs

## Goal

Run a technical-writer review pass over fresh mainline commits, update operator-facing documentation and GitHub Pages content for any behavior drift, validate the docs site, and reintegrate documentation-only changes.

## Bead(s)

- `bd-d1cf99` — source-side PID-only watchdog diagnostics are not stderr mirrored
- `bd-5d628b` — legacy structured persistent-idle advisory diagnostics are not re-mirrored
- `bd-adb882` — watchdog respawn diagnostics without the stable marker are suppressed from daemon-crash evidence
- `bd-5cdbb4` — stale/replayed audio transcription 502 diagnostics are suppressed from daemon-crash evidence
- `bd-c381f2` — crash-log rotation prunes known non-crash diagnostic tails while retaining real crash evidence
- `bd-21dc3d`, `bd-469c75`, `bd-ae7bc4` — TUI performance/profile updates reviewed as internal/no public docs drift

## Before state

- Failing tests: none known for docs.
- Relevant metrics: checkout was behind `origin/main` by six first-parent commits at pass start.
- Context: recent commits refined daemon stderr mirroring for legacy idle-advisory shapes, source-side watchdog diagnostics, watchdog respawn messages, stale audio-transcription 502 route logs, and crash-log rotation pruning for known non-crash tails.

## After state

- Failing tests: none in docs validation.
- Relevant metrics: `./docs/validate-pages.sh` passed with 3313 passed, 0 warnings, 0 failed; `git diff --check` was clean.
- Context: logs/daemon guidance now states that stale/replayed watchdog, transcription-502, and legacy idle-advisory stderr tails stay out of `daemon-crash.log`, and that crash-log rotation prunes those known non-crash tails while retaining real panic/backtrace evidence.

## Diff summary

- Commits: current agent-branch documentation commit `bd-5cdbb4: document stale diagnostic stderr suppression`
- Files touched: `AGENTS.md`, `README.md`, `docs/daemon.html`, `docs/logs.md`, `docs/logs.html`, `.cacophony/agent/beelink-cacophony-technical-writer/summary/pending/summary.md`
- Tests: +0 / -0 / flipped 0
- Behavioural delta: documentation-only. Operator-facing logging guidance now covers source-side and stale/replayed non-crash diagnostics plus crash-log rotation pruning.

## Operator-takeaway

The crash-log contract is narrower and clearer: stale or replayed diagnostic tails for PID-only audio watchdogs, non-fatal transcription 502s, and legacy persistent-idle advisories are diagnostics, not new daemon-crash evidence; rotation now decays those known non-crash tails without waiting for another oversized crash-log event.
