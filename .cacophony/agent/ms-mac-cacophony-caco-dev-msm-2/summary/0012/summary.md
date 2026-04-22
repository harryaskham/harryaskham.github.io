# Session summary — caco-web SSE visibility rescue (bd-9d2d65)

## Goal

caco-web's connection-status pill goes grey during a daemon flap and stays grey even after the daemon recovers; F5 fixes it. Make tab focus restore the connection without a hard reload.

## Bead(s)

- `bd-9d2d65` — caco-web SSE reconnect loop on transient daemon hiccup — page goes 'offline' and stays even after daemon recovers.

## Before state

- `connectSSE()` in `app.js` already has exponential backoff up to 30s and a setTimeout retry chain.
- BUT background-tab browser throttling and half-open TCP sockets across laptop sleeps can stall the chain for arbitrarily long, leaving the pill grey until the operator hard-reloads.

## After state

- New `installSseVisibilityRescue()` registers a one-time `visibilitychange` listener. When the tab becomes visible AND `state.connectionStatus !== 'connected'`, it resets the retry counter and calls `connectSSE()` immediately — short-circuiting backoff for the foreground case while leaving background-tab backoff alone.
- Called from the main init block right after `connectSSE()`. Idempotent via `_sseVisibilityRescueInstalled` flag.
- New test `app_js_has_sse_visibility_rescue` asserts the function definition, the idempotency flag, and the actual call site.
- `cargo test -p caco-web --lib app_js_has_sse` — 2 / 2.
- `cargo test-small` clean. `cargo check --workspace --tests` clean.

## Diff summary

- Commit: `a9fa9044`
- Files touched: `crates/caco-web/static/app.js` (+~25 lines), `crates/caco-web/src/tests.rs` (+~17 lines).
- Tests: +1 unit; 0 removed; 0 flipped.
- Behavioural delta: when the operator focuses a previously-hidden caco-web tab and the SSE is disconnected, reconnection happens immediately instead of waiting on backoff (up to 30s).

## Out of scope

- Replay-on-reconnect (server `?since=<lastEventId>`) — bead's longer-term ask.
- Server-side SSE keep-alive ping cadence — orthogonal.

## Operator-takeaway

Focusing a stale caco-web tab now triggers an immediate reconnect — pill goes from grey through 'Connecting…' to 'Connected' on tab-focus instead of waiting on backoff or needing F5.
