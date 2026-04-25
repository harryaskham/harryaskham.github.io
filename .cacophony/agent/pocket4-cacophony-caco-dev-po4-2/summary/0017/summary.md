# Session summary — TUI full-state bootstrap now survives degraded daemons

## Goal

Fix the ms-mac TUI failure mode where chat and bead RPCs could keep working,
but the daemon full-state bootstrap never converged. The goal was to make the
TUI resilient when `/api/v1/ui/snapshot` is temporarily degraded: show a
last-known-good full snapshot with an explicit stale/disconnected signal, and
then automatically converge once the daemon-side full snapshot becomes fresh
again.

## Bead(s)

- `bd-f85b12` — TUI never converges to daemon full state on ms-mac (no cache / no eventual delivery)

## Before state

- The TUI bootstraps full state from `GET /api/v1/ui/snapshot` and uses
  `/api/v1/ui/stream` only for live deltas.
- The daemon snapshot handler already had bounded degradation for slow mesh
  sections:
  - return cached/empty data with `freshness = partial|stale`
  - refresh the cache in a background task
- But after that background refresh completed, existing TUI clients were not
  prompted to re-fetch the full snapshot.
- The TUI also had no persisted last-known-good full snapshot, so a failed
  initial bootstrap could leave the session on the empty pre-snapshot skeleton.

## After state

- The TUI now persists the last-known-good full UI snapshot under the normal
  TUI state directory.
- If the initial full-state fetch fails and the in-memory state is still empty,
  the TUI loads that cached snapshot and forces its freshness markers to
  `stale`, while preserving the disconnected service state so the operator sees
  cached-but-not-live data rather than a false healthy connection.
- While any snapshot section remains degraded (`beads_freshness != fresh` or
  `agents_freshness != fresh`), the TUI periodically re-fetches the full
  snapshot instead of waiting forever for unrelated reconnect events.
- The daemon now emits a `ui.snapshot_refresh_ready` notification after the
  background mesh-bead refresh completes, so connected TUIs can promptly kick a
  full snapshot refresh and converge.

## Diff summary

- Files touched:
  - `crates/caco-daemon/src/ui_stream.rs`
  - `crates/caco-tui/src/app.rs`
  - `crates/caco-tui/src/event.rs`
  - `crates/caco-tui/src/state/mod.rs`
  - `crates/caco-tui/src/state_persistence.rs`
- Behavioural delta:
  - degraded full-state bootstrap no longer means permanent incomplete TUI state
  - cached full-state fallback is available across restarts
  - daemon-side background refresh now results in a TUI refresh hint instead of
    waiting for a later manual/reconnect-triggered snapshot
- Validation:
  - `cargo build -p caco-tui`
  - `cargo test -p caco-tui persisted_ui_snapshot_roundtrip -- --nocapture`
  - `cargo test -p caco-tui snapshot_failed_loads_cached_snapshot_when_empty -- --nocapture`
  - `cargo test -p caco-tui snapshot_refresh_hint_starts_background_refresh -- --nocapture`
- Operational side context from Harry’s parallel directive:
  - ms-mac-local agents verified `caco-tts-daemon` running, unmuted, and recent
    probes reaching `outcome=played`; if silence persists, the remaining likely
    issue is host audio-device routing outside the daemon

## Operator-takeaway

The root cause was not a generic macOS transport outage; it was a full-state
recovery gap. The daemon could recover its snapshot cache in the background,
but the TUI had no durable fallback and no automatic way to rehydrate once that
cache became fresh. This patch closes both gaps so ms-mac can degrade visibly
and then converge automatically instead of staying incomplete forever.
