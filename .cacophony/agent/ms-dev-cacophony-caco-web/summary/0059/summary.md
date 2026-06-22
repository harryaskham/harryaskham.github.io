# Session summary — bd-916b85: TTY/TTS interval visibility-gating

## Goal

Continue the caco-web perf polish loop. bd-6b0f19 noted two
remaining setInterval callbacks in app.js still firing on
hidden tabs. This cycle hits both.

## Bead(s)

- `bd-916b85` — [caco-web] gate TTY polling fallback (1s) + TTS status (10s) on visibilityState

## Before state

| File:line | Interval | Endpoint | Hidden-tab cost |
|-----------|----------|----------|-----------------|
| `app.js:8565` | 1s | `/api/v1/agents/{id}/logs` (TTY fallback) | **60 req/min, up to 200 KB/tick = 12 MB/min** |
| `app.js:7151` | 10s | `/api/v1/tts/status` | 6 req/min |

Combined: 66 req/min wasted on idle hidden tabs.

The TTY polling path is the WebSocket-unavailable fallback
for live terminal output. Browsers already throttle
WebSockets on hidden tabs, so the polling fallback should
match that behavior. Each tick fetches the full tmux capture
and rewrites the xterm buffer, so the savings are not just
network-tier but also xterm/CPU work.

## After state

Both callbacks short-circuit on
`document.visibilityState !== 'visible'`. TTY polling
additionally registers a visibilitychange listener (stored
on `ttyState._vizListener` for clean teardown) that fires
one immediate `tick()` on tab return so the terminal buffer
is fresh the moment the operator looks. `teardownAgentTty`
removes the listener to avoid leaking across agent switches.

The TTS interval already self-clears when the operator
navigates away from the services view, so no immediate-on-
return handler is needed there -- the next regular tick
picks up state within 10s.

## Visibility-gated polling pattern, now established

| Bead | Surface | Interval | Pattern |
|------|---------|----------|---------|
| bd-f8ae0d | snapshot refresh | 60s | guard + visibilitychange refresh |
| bd-aba11f | relative-time DOM walk | 30s | guard only (visibilitychange refresh upstream) |
| bd-aba11f | SSE liveness probe | 15s | guard only |
| bd-6b0f19 | workspace bead/agent refresh | 5s, 10s | guards + visibilitychange refresh |
| bd-916b85 | TTY polling fallback | 1s | guard + visibilitychange refresh + teardown listener removal |
| bd-916b85 | TTS status | 10s | guard only |

The dashboard is now fully visibility-gated for all
high-frequency polling paths.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted to avoid self-referential SHA.
- Files touched:
  - `crates/caco-web/static/app.js` -- TTY polling tick() guard + visibilitychange listener wiring + teardown removal; TTS interval guard.
  - `crates/caco-web/src/tests.rs` -- regression test pins guard at TTY tick(), listener registration, teardown removal of listener, and guard at TTS interval.
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*` -- bounded validation receipts.
- Tests: +1 caco-web static asset regression test. Net pass count: 465 -> 466; 11 pre-existing failures on main unchanged.

## Operator-takeaway

Idle hidden tabs no longer drain network on agent terminals
or TTS status polls. For an operator with the agent-tty tab
open in a backgrounded window: 60 req/min + up to 12 MB/min
of fetched tmux capture -> 0 while hidden, with one
immediate refresh on return so the terminal buffer is fresh
the moment they look. Combined with bd-6b0f19, the
dashboard's hidden-tab footprint is now uniformly lean
across all 6 polling paths.
