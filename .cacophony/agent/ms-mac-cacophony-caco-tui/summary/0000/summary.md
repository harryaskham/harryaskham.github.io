# Session summary — Snapshot bead counts and lazy-load recovery

## Goal

Restore operator trust in bead status visibility after the snapshot trim change by making closed, draft, and computed-blocked counts accurate again, then adding on-demand loading so TUI and web operators can actually browse trimmed terminal bead statuses without waiting for a full untrimmed snapshot.

## Bead(s)

- `bd-0f746d` — [supersedes bd-a3ee9d] Closed/draft bead counts wrong + no lazy-load — fix in TUI + webapp + Android companion (all snapshot consumers)

## Before state

- Failing tests: none known in this area, but the user-visible regression was live.
- Relevant metrics: `/api/v1/ui/snapshot` trimmed `beads` down to `open | in_progress | permanent`, so closed/draft rows disappeared from bootstrap state.
- Context:
  - `crates/caco-daemon/src/ui_stream.rs::trim_snapshot_beads` dropped closed and draft beads from the snapshot payload.
  - `compute_bead_stats` still counted by raw persisted `status`, so computed-blocked beads that remained persisted as `open` were miscounted as open rather than blocked.
  - TUI and web badge/count surfaces relied on trimmed state in enough places that operators could see misleading totals during incident response.
  - TUI and web had no first-party way to hydrate closed/draft rows on demand from the main beads views.
  - Android parsed only the trimmed snapshot bead list, although its later pull-sync path could hydrate full per-project bead lists after connection.

## After state

- Failing tests: none in validated Rust/web paths.
- Relevant metrics:
  - Snapshot `bead_stats` now split computed-blocked beads into the blocked bucket instead of open.
  - TUI and web can lazily hydrate closed/draft bead rows on demand from the main beads views.
  - Android now parses `bead_stats` from snapshots and keeps those counts updated on `bead_updated` SSE events.
- Context:
  - Daemon snapshot stats now reflect operator-facing effective blocked/open totals.
  - Web summary cards, hero stats, project cards, status chips, and bead list totals use `bead_stats` rather than only the trimmed `state.beads` payload.
  - Web closed/draft sections lazy-load from canonical bead endpoints, and search from the main beads view can trigger hydration of trimmed terminal statuses.
  - TUI closed/draft sections now lazy-load from canonical bead endpoints, and search from the main unfiltered beads views can hydrate closed/draft rows before local matching.
  - Android now understands `bead_stats` in snapshot fixtures and app state, and its tests cover count hydration plus pull-sync insertion of closed/draft rows outside the trimmed snapshot.
  - Android local unit execution remains environment-limited on this node because no Java runtime / Gradle wrapper was available in the checkout environment.

## Diff summary

- Commits: `10968da5`
- Files touched:
  - `crates/caco-daemon/src/ui_stream.rs`
  - `crates/caco-tui/src/app.rs`
  - `crates/caco-tui/src/client.rs`
  - `crates/caco-tui/src/event.rs`
  - `crates/caco-web/static/app.js`
  - `companion/android/app/src/main/java/com/cacophony/companion/state/Models.kt`
  - `companion/android/app/src/main/java/com/cacophony/companion/state/AppStateStore.kt`
  - `companion/android/app/src/main/java/com/cacophony/companion/ui/overview/OverviewScreen.kt`
  - `companion/android/app/src/main/java/com/cacophony/companion/ui/beads/BeadsListScreen.kt`
  - `companion/android/app/src/test/java/com/cacophony/companion/AppStateStoreTest.kt`
  - `companion/android/app/src/test/java/com/cacophony/companion/BeadsScreenTest.kt`
  - `companion/android/app/src/test/resources/snapshot_full.json`
  - `companion/android/app/src/test/resources/snapshot_empty.json`
- Tests: added daemon/unit coverage for computed-blocked snapshot stats, added Android state/count coverage, no tests removed.
- Behavioural delta:
  - Count surfaces no longer imply that blocked work is still open just because persisted bead rows remain `open` under the hood.
  - Closed and draft beads are no longer permanently invisible from the TUI/web main beads flows after snapshot trim; they can be hydrated on demand instead of forcing an untrimmed bootstrap payload.

## Operator-takeaway

The important fix here is not just “show more beads” — it is restoring trust in the trimmed snapshot model. Operators can keep the smaller snapshot payload for fast bootstraps, while count summaries remain truthful and the terminal statuses that were trimmed out can still be fetched exactly when someone drills into them or searches for them.