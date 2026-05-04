# bd-806bea — bidirectional workspace terminal

## Summary

Implemented the first full-duplex workspace terminal slice for `bd-806bea`.

Changes:

- `crates/caco-web/static/workspace-terminal-pane.js`
  - Switched workspace terminal panes from the raw `/api/v1/agents/<id>/pty/stream` tail endpoint to the structured `/api/v1/agents/<id>/pty` WebSocket.
  - Sends keystrokes as `{ type: 'input', data }` frames.
  - Sends resize events as `{ type: 'resize', cols, rows }` frames after xterm fit/ResizeObserver updates.
  - Handles structured daemon frames: `hello`, `snapshot`, `pong`, and `error`.
  - Preserves cross-pane broadcast by sending structured input frames to sibling panes instead of raw bytes.
  - Keeps kitty graphics filtering/rendering in the structured snapshot path.
  - Separates local read-only toggles from daemon-advertised `read_only` state so a future read-only daemon hello cannot be accidentally overridden by the local button.

- `crates/caco-daemon/src/pty_stream.rs`
  - Handles structured `ClientFrame::Resize { cols, rows }` in the workspace `/pty` path.
  - Validates resize dimensions: rejects zero and dimensions above `1000`.
  - Applies accepted browser resizes to the backing tmux pane via `tmux resize-pane -t <pane> -x <cols> -y <rows>`.
  - Returns structured `resize_failed` errors on validation/tmux failures.

- `crates/caco-web/src/tests.rs`
  - Updated workspace terminal endpoint contract tests to pin the interactive `/pty` route while preserving the legacy `/pty/stream` route.
  - Added static contract coverage for structured input, resize, and snapshot handling.
  - Updated read-only test coverage for combined local/daemon read-only state.

## Validation

Passed:

- `node --check crates/caco-web/static/workspace-terminal-pane.js`
- `git diff --check -- crates/caco-daemon/src/pty_stream.rs crates/caco-web/src/tests.rs crates/caco-web/static/workspace-terminal-pane.js`
- `cargo test -p caco-web workspace_terminal_pane -- --nocapture`
- `cargo test -p caco-web workspace_pty_ws_endpoint_is_registered_for_workspace_panes_bd4431dc -- --nocapture`
- `cargo test -p caco-web terminal_proxy_fixture_relays_input_and_resize_without_live_agent_bd_e8b8e5 -- --nocapture`
- `cargo test -p caco-daemon pty_stream -- --nocapture`

One attempted combined `cargo test` invocation was malformed because Cargo accepts only one test-name filter; the focused tests above were rerun separately and passed.

## Notes

Board/reintegration was not performed while the local/authoritative board views were marked stale or syncing. Re-check board safety before reintegration/close.
