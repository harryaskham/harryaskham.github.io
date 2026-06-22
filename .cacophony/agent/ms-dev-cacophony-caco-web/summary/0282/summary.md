# Session summary — bd-057f2e: surface store-lock-contended degraded snapshot sections

## Goal

Per Harry's "take any capable bead, don't sit idle" directive (my Pico lane being
complete + the clean dev backlog draining fast), pitch in on a high-impact
backend-Rust bug ADJACENT to my caco-web lane: the daemon ui-snapshot's
store-backed sections silently serve EMPTY under store-lock write-saturation,
masking real fleet state. Make the empty inventory read as "degraded, not real"
end-to-end (daemon marker → caco-web dashboard).

## Bead(s)

- `bd-057f2e` — Sustained daemon store-lock contention blanks agent list / ps /
  UI-snapshot (empty despite agents running). Filed by caco-ctrl. Claimed +
  marker-slice implemented this session.

## Before state

- `/api/v1/ui/snapshot` store-backed sections (recent events, notifications,
  chat/speech history, operator inbox) degrade to EMPTY via `try_lock`
  (bd-fc7dca) when the shared daemon store lock is write-saturated.
- That empty serving is SILENT — consumers (caco-web dashboard, TUI) cannot
  tell degraded-empty from real-empty, masking real fleet state (the core
  concern: a failed agent would be hidden by the empty list).
- `SnapshotFreshness` (bd-3d9693) already annotated `beads`/`agents` freshness
  but had NO indicator for the store-backed sections.
- caco-web lib tests: prior baseline green.

## After state

- `SnapshotFreshness` gains a `store: String` field ("fresh"/"degraded",
  serde-default "fresh" for backward-compat).
- `handle_ui_snapshot` tracks a `store_contended` flag set in all 5 degraded
  arms (3 store-backed-section arms + 2 operator-inbox arms); when set,
  `freshness.store = "degraded"`.
- caco-web freshness indicator surfaces `freshness.store === 'degraded'` as
  `inventory degraded (store contended)` so empty reads as degraded-not-real.
- Marker-only mitigation of the MASKING RISK; the heavier read-snapshot
  decoupling (the root contention fix) is intentionally left as a follow-on for
  a daemon specialist (flagged to caco-ctrl).
- Tests: daemon `snapshot_freshness_round_trips_json` extended (store round-trip
  + legacy-default); new caco-web guard
  `store_contended_freshness_surfaced_in_indicator_bd_057f2e`.

## Diff summary

- Code commit: `dcd54bac33` (pre-rebase; final landed squash SHA from the
  reintegration receipt).
- Files touched: `crates/caco-daemon/src/ui_stream.rs` (field + accumulator +
  5 set-sites + apply + round-trip test), `crates/caco-tui/src/app.rs` +
  `crates/caco-tui/src/state/tests.rs` (test-literal field adds, compile-only),
  `crates/caco-web/static/app.js` (indicator store check),
  `crates/caco-web/src/tests.rs` (needle guard).
- Tests: +1 caco-web guard, +2 assertions in the daemon round-trip test.
- Behavioural delta: degraded store sections are now operator-visible as
  "inventory degraded (store contended)" instead of silently-empty; no change
  to the lock/write hot path (low-risk).

## Operator-takeaway

This is the bounded, low-risk MASKING mitigation for bd-057f2e — it does NOT fix
the underlying store-lock write-saturation (the empty sections are still empty),
but it stops the empty inventory from silently hiding real fleet state by
labelling it degraded across the dashboard. The durable root fix (serve the
aggregate reads from a periodically-refreshed snapshot decoupled from the live
write-lock) remains open as a follow-on for a daemon specialist. A caco-web
specialist took this because the dashboard consumes the exact /api/v1/ui/snapshot
that degrades.
