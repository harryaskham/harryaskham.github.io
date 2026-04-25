# Session summary — bd-9d7ded prefer SSE freshness when snapshot pending

## Goal

Stop the caco-web dashboard from misrepresenting itself as broken
when the bulk `/api/v1/ui/snapshot` endpoint times out but SSE is
delivering live data correctly.

## Bead(s)

- `bd-9d7ded` — caco-web status hero shows 'Snapshot pending' / 'beads: partial' even when SSE is actively delivering data (P3 bug, slice 4 audit)

## Before state

- When the daemon's bulk `/ui/snapshot` endpoint hit its 30s timeout
  (HTTP 500 `request timed out after 30s`), the dashboard's hero
  freshness pill said `Snapshot pending`, the freshness-indicator
  said `beads: partial`, and the Refresh button stayed pinned on
  `Refreshing…` for the entire 30s window — even though SSE was
  populating 26 agents, 71 beads, 3 services and 2 notifications in
  the same view at the same time.
- Two presentation-only paths were both gated on the bulk path:
  `renderStatusHero` and `applySnapshot.snap.freshness`.

## After state

Three coordinated fixes (A/B/C from the bead body):

- **A** `renderStatusHero` consults `state.lastSseActivity` in the
  `Snapshot pending` branch and emits `Live SSE · Ns ago` (with
  `.fresh` styling and a tooltip pointing at bd-9d7ded) when SSE
  has been active in the last 10s.
- **B** `loadSnapshot` installs a 35s `setTimeout` that forces
  `setRefreshButtonBusy(false)` if the in-flight reset never fires
  (stalled connection / promise that never settles). Cleared in the
  finally block on normal settlement, and the daemon's own 30s
  timeout still gets a clean win window.
- **C** `applySnapshot` gates the `beads: partial / agents: partial`
  branch on `!sseFresh`. When SSE is fresh, the indicator becomes
  `● Live (SSE · Ns)` with `.fresh` styling and a tooltip that
  preserves the bulk endpoint's reported partial state for
  diagnostic purposes.

## Diff summary

- Commit: 263d90dfa
- Files touched: `crates/caco-web/static/app.js` (+58, -2 across
  three blocks) and `crates/caco-web/src/tests.rs` (+46).
- New source-level guard
  `status_hero_prefers_sse_freshness_when_snapshot_pending_bd_9d7ded`
  asserts (a) the `Live SSE · Ns ago` label exists in the pending
  branch, (b) the busy-cap timeout is installed and cleared, and
  (c) the partial-issues path is gated on `!sseFresh` plus the
  Live(SSE) indicator wording is present.
- Tests: cargo test-small 262/262 pass.

## Operator-takeaway

The dashboard now tells the truth when the bulk snapshot endpoint
is timing out: SSE-backfilled data is surfaced as live, not
misreported as stale or pending, and the Refresh button can't be
pinned past the daemon's own timeout. Happy-path behaviour
(bulk snapshot succeeds) is unchanged — the SSE-fresh checks only
fire when the bulk path has produced a stale or pending pill in
the first place.
