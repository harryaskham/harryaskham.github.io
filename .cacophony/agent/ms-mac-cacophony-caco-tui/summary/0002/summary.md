# Session summary — workspace MVP thin slice

## Goal

Ship the first usable slice of the new web workspace surface so the rest of the workspace-view epic can build in parallel on top of stable contracts. This session aimed to prove the end-to-end shape: a dedicated `/workspace` route, a draggable two-pane layout, a bead list in one pane, and a live tmux-backed terminal in the other, with browser-side state and shared `window.Workspace` contracts ready for downstream beads.

## Bead(s)

- `bd-a78749` — [workspace-view MVP] 2-pane split + 1 terminal + 1 bead list + layout JSON scaffold (end-to-end thin slice)
- (parent: `bd-027e9d` — [EPIC] caco-web Workspace View: splittable panes, saved layouts, multi-agent terminals, parity with TUI)

## Before state

- `caco-web` had the main dashboard and the agent terminal surface, but no dedicated `/workspace` route.
- The existing terminal path was `/api/v1/agents/<id>/pty/stream`, a legacy read-only-ish stream contract used by the current dashboard terminal surface.
- There was no web-side `window.Workspace` object, no MVP pane tree contract, and no `workspace.*` localStorage namespace for layout state.
- There was no dedicated acceptance test for a workspace page plus terminal WebSocket handshake.

## After state

- `caco-web` now serves `/workspace` with a two-pane split view, draggable splitter, bead list pane, and xterm.js-backed terminal pane.
- The browser now exposes `window.Workspace` with `paneTree`, `bus.on()/emit()`, and local persistence under `workspace.split_ratio`, `workspace.selected_agent`, and `workspace.project`.
- The daemon and web proxy now expose a dedicated `/api/v1/agents/<id>/pty` WebSocket path for the workspace MVP, while preserving the older `/pty/stream` route for the existing terminal surface.
- The workspace route now injects the configured default project into the page and the client also falls back to `ui/snapshot.default_project` / first configured project when needed.
- `caco-web` now has an acceptance test that loads `/workspace` and confirms the terminal WebSocket handshake.

## Diff summary

- Commits: `a734c27b`
- Files touched: `crates/caco-web/src/server.rs`, `crates/caco-web/src/proxy.rs`, `crates/caco-web/src/static_assets.rs`, `crates/caco-web/src/tests.rs`, `crates/caco-web/src/ws_proxy.rs`, `crates/caco-web/static/workspace.html`, `crates/caco-web/static/workspace.css`, `crates/caco-web/static/workspace.js`, `crates/caco-daemon/src/pty_stream.rs`, `crates/caco-daemon/src/agent/health.rs`, `crates/caco-daemon/src/lib.rs`, `crates/caco-cli/src/lib.rs`, `README.md`, `AGENTS.md`
- Tests: added workspace-MVP acceptance coverage in `crates/caco-web/src/tests.rs`; retained daemon PTY-stream unit coverage; full `cargo test-small` and `cargo check --workspace --tests` preflight passed.
- Behavioural delta: operators can now open a dedicated browser workspace page and use a simple saved split plus a live terminal selection workflow, and downstream workspace beads now have concrete page, bus, storage, and WebSocket contracts to build against instead of inventing parallel ones.

## Operator-takeaway

This session established the MVP contract that the rest of the web-workspace buildout can safely target. The crucial outcome is not just the page itself, but the fact that the workspace route, `window.Workspace` contract, localStorage keys, and dedicated `/pty` terminal path now exist as stable integration points for the remaining parallel beads.
