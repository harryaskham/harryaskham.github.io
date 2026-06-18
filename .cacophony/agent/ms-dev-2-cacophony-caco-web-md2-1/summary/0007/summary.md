# Session summary — caco-web: stop showing "Connected/Live" when the daemon is unreachable

## Goal

Fix a P2 operator-trust bug found via a degraded-state probe: when caco-web is up
but the daemon backend is unreachable, the dashboard showed "Connected · Live
data connected" indefinitely, rendering the on-disk last-known-good (LKG)
snapshot as if it were live — no "Backend unavailable" / cached indicator. Make
the dashboard surface a cached/degraded state per the AGENTS.md/SPEC contract.

## Bead(s)

- `bd-46770c` — caco-web shows Connected/Live with a dead daemon (serves LKG snapshot, ignores backend_unavailable SSE error events)

## Before state

- Failing tests: none.
- Repro: `caco web --daemon-url http://127.0.0.1:1` (dead). The dashboard showed
  conn="Connected", `state.connectionStatus=connected` across 5/12/18/25/32s
  samples (past the 15s SSE grace); no degraded text; console clean.
- Backend signalled correctly (so frontend bug): `/api/v1/node` → 200
  `backend_unavailable:true`; `/api/v1/ui/stream` → repeated `event: error
  {backend_unavailable:true}`; `/api/v1/ui/snapshot` → on-disk LKG snapshot
  (200, real data, no `ok:false`) so `snapshotUnavailable` stayed false.
- Root cause (app.js): caco-web holds the SSE connection OPEN and sends
  `event: error` DATA messages; `onopen` set 'connected' (LKG snapshot loaded);
  the named server-sent error events had NO handler; the native `onerror` grace
  path is gated on `readyState !== OPEN`, never satisfied while the stream stays
  open. The periodic snapshot refresh also re-set 'connected' off the LKG 200.

## After state

- Failing tests: none. `node --check` clean; `cargo test -p caco-web --lib`
  passed via the daemon test queue (exit 0).
- Verified with the dead-daemon repro: conn="◌ Cached · ↻ N",
  `connectionStatus=cached`, stale badge "Showing last known state — backend
  unreachable, reconnecting…", **persistently** at 5/15/30s, console clean.
- Verified the live-daemon case is unaffected: conn="Connected",
  `connectionStatus=connected`, `streamBackendUnavailable=false`, console clean.

## Diff summary

- Code/content commit: pending final squash SHA from the reintegration receipt.
- Files touched: `crates/caco-web/static/app.js` —
  - new `state.streamBackendUnavailable` latch;
  - `sse.addEventListener('error', …)` handles server-sent `event: error`
    backend_unavailable data messages (distinct from native connection errors
    which carry no `event.data`) → sets cached/backend_unavailable + stale badge;
  - `sse.onopen` keeps the degraded state while the latch is set instead of
    blindly setting 'connected';
  - the snapshot-success path treats a 200 snapshot as LKG cache (not live) while
    the latch is set;
  - `sse.onmessage` clears the latch and restores 'connected' when a real UI
    event proves the backend recovered.
- Tests: +0 / -0 (JS behavior change; validated via live dead/live-daemon probes).
- Behavioural delta: the dashboard now shows cached/backend-unavailable (with a
  stale badge) when the daemon is unreachable, instead of falsely claiming live.

## Embedded artefacts

- `web/screenshots/after-backend-unavailable.png` — dashboard in the cached/
  backend-unavailable state with a dead daemon (after the fix).

## Operator-takeaway

The dashboard could claim "Live data connected" while the daemon was completely
down, because caco-web serves the on-disk LKG snapshot as a 200 and keeps the SSE
open while sending backend_unavailable error events that nothing handled. The fix
latches an explicit stream-backend-unavailable signal and makes onopen / snapshot
refresh respect it, so stale cache is shown AS cached. Recovery is automatic on
the next real UI event. (A complementary backend improvement would be to mark the
LKG snapshot response as stale, but the frontend latch is the robust client fix.)
