# Session summary — interactive web PTY terminals

## Goal

Remove the remaining Phase 1 read-only web terminal limitations for `bd-34d508` by moving current caco-web terminal surfaces from the legacy `/pty/stream` tailer to the structured bidirectional `/pty` WebSocket, and update tests/docs so the interactive contract stays pinned.

## Bead(s)

- `bd-34d508` — Remove Phase 1 read-only terminal limitations

## Before state

- Failing tests: none known for this bead at claim time.
- Relevant metrics: no in-progress beads assigned before claim; `bd-34d508` was claimed through the first-party board surface.
- Context: the daemon already exposed `/api/v1/agents/{id}/pty` with structured `input`, `resize`, `signal`, `ping`, and `snapshot` frames, but some web terminal code and docs still referenced the read-only `/pty/stream` Phase 1 path and banner copy.

## After state

- Failing tests: none from targeted validation; one malformed two-filter cargo invocation failed before running tests, and one queued test attempt returned retryable daemon-restart infrastructure error before passing on retry.
- Relevant metrics: `docs/validate-pages.sh` passed; targeted queued caco-web and daemon tests passed.
- Context: current agent detail and integrated workspace terminal paths dial `/pty`, send JSON input/resize frames, parse structured snapshot/error/pong/hello frames, and no longer show the obsolete Phase 1 read-only limitation banner for the agent detail terminal.

## Diff summary

- Commits: `be1c652933` (implementation commit after first-party rebase; final landed squash SHA to be assigned by reintegration receipt).
- Files touched: `SPEC.md`, `crates/caco-daemon/src/pty_stream.rs`, `crates/caco-web/src/tests.rs`, `crates/caco-web/src/ws_proxy.rs`, `crates/caco-web/static/app.js`, `crates/caco-web/static/workspace-integrated.js`, `docs/design/bd-bf8064-terminal-session-broker.md`, `docs/logs.md`, `docs/logs.html`.
- Tests: updated caco-web source-contract tests for app.js and workspace-integrated terminal wiring; daemon pty_stream tests unchanged but re-run.
- Behavioural delta: caco-web terminal surfaces now use the structured interactive PTY route for keyboard and resize support, while `/pty/stream` remains documented as legacy compatibility only.

## Operator-takeaway

The web terminal path is no longer framed as Phase 1 read-only for current interactive surfaces: keystrokes and resize events now target the daemon’s structured `/pty` WebSocket, with tests and docs guarding against regressions back to `/pty/stream` for operator-facing terminals.
