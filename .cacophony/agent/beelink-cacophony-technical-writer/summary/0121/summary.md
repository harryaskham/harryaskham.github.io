# Session summary — crash-log pruning cadence docs

## Goal

Run a technical-writer review pass over fresh mainline commits, update operator-facing documentation for any drift, validate the GitHub Pages docs, and reintegrate documentation-only changes.

## Bead(s)

- `bd-41e1a5` — crash-log rotation runs frequently enough for watchdog diagnostics to decay between log-monitor sweeps
- `bd-5b5de1` — log-monitor suppresses duplicate recurrence beads for already-covered idle/watchdog/transcription-502 shapes
- `bd-70109d`, `bd-42cc81`, `bd-d30fa3`, `bd-ae7bc4` — TUI performance/profile updates reviewed as internal/no public docs drift

## Before state

- Failing tests: none known for docs.
- Relevant metrics: `origin/main` had advanced four first-parent commits beyond the previous technical-writer landing `6911825e1`.
- Context: prior docs already covered stale diagnostic stderr suppression and crash-log pruning, but fresh daemon/profile commits changed the pruning cadence and log-monitor recurrence guidance.

## After state

- Failing tests: none in docs validation.
- Relevant metrics: `./docs/validate-pages.sh` passed with 3313 passed, 0 warnings, 0 failed; `git diff --check` was clean.
- Context: logs/daemon guidance now states that known non-crash crash-log tails are pruned on a short cadence, about once a minute, and that log-monitor profiles should aggregate already-covered recurrences instead of filing a new bead every sweep.

## Diff summary

- Commits: current agent-branch documentation commit `bd-41e1a5: document crash-log pruning cadence`
- Files touched: `AGENTS.md`, `README.md`, `docs/daemon.html`, `docs/logs.md`, `docs/logs.html`, `.cacophony/agent/beelink-cacophony-technical-writer/summary/pending/summary.md`
- Tests: +0 / -0 / flipped 0
- Behavioural delta: documentation-only. Operator-facing logging guidance now matches the daemon's faster crash-log rotation and log-monitor duplicate-suppression profile guidance.

## Operator-takeaway

Known non-crash diagnostic tails should disappear from `daemon-crash.log` promptly through the normal rotation loop, and log-monitor should aggregate those covered recurrences rather than creating per-sweep duplicate beads.
