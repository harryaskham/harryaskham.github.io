# bd-bf1e86 polish #5: empty-state hints in feed + merge_queue panels

## Goal

Continue the empty-state keystroke/context hint pattern established in polish #3 (inbox) and #4 (events + notifications). Apply to the remaining two panels that previously rendered empty states as a single dim line: the feed and the merge-queue panel's "no recent activity" arm.

## Bead(s)

- bd-bf1e86 (P2 permanent — TUI polish; stays open after reintegrate per endless-mode rules)

## Before state

- `crates/caco-tui/src/views/feed.rs:73`: empty feed rendered only `"  Waiting for events..."`. New operators couldn't tell whether the feed was waiting on agent output, daemon logs, or inter-agent messages.
- `crates/caco-tui/src/views/merge_queue.rs:208-213`: when `state.merge_queue_fetched && report.is_none()` the panel rendered only `"  No reintegration activity."`. No indication of what action would populate it.

This is the fifth cycle on bd-bf1e86 in this session:
- Polish #1: inbox poll-error UI surfacing.
- Polish #2: merge-queue freshness indicator (`updated Ns ago` in title).
- Polish #3: inbox empty-section keystroke hints.
- Polish #4: events + notifications empty-state hints.
- Polish #5 (this commit): feed + merge-queue empty-state hints.

## After state

**`feed.rs`**: empty feed now renders three lines:
```
  Waiting for events...

  Feed shows agent stdout/stderr, daemon logs, and inter-agent messages as they stream in.
```

**`merge_queue.rs`**: "No reintegration activity" arm now renders:
```
  No reintegration activity.

  This panel populates as agents call `caco agent reintegrate` (squash-merge into main).
```

`Loading…` arm (cold start, no fetch yet) is unchanged — it correctly conveys async wait without a hint being needed.

Verification:
- `cargo test-small`: 56/56 PASS
- `cargo clippy --workspace --all-targets -- -D warnings`: clean

## Diff summary

3 files changed, +23 / -14:

- `crates/caco-beads/src/model.rs`: `+0 / -0` net (resolved a stash-conflict against peer's identical SnapshotBead-fields addition; took upstream — this is a peer-overlap artefact, not a real change in this commit).
- `crates/caco-tui/src/views/feed.rs`: +9 / -3 (List items with hint paragraph)
- `crates/caco-tui/src/views/merge_queue.rs`: +5 / -0 (extra Line::push under the no-activity arm)

## Operator-takeaway

Five cycles of bd-bf1e86 polish landed in one session — the permanent-bead pattern works well for accumulating tiny TUI papercut fixes that would individually be too small to file. Each cycle adds 5-30 lines of code and improves one specific rough edge.

The empty-state hint pattern is now applied to: inbox sections (polish #3), events timeline (polish #4), notifications panel (polish #4), feed (polish #5), merge-queue (polish #5). Remaining surfaces with no-data states that could benefit from the same treatment — found via `grep "is_empty\|(no data)\|loading" crates/caco-tui/src/views/`:
- `releases.rs`: per-project release jobs panel (different shape — per-project hashmap; needs more thought).
- Various view-specific empty rows (cluster panels, agents list).

**Peer-overlap context**: today's session has run side-by-side with bd-fb9318 (`estimated_effort` schema add, ms-mac msm-3/msm-5) — that landed mid-cycle and I caught the resulting reconcile_skips_export_when_content_unchanged regression on rebase, root-caused it (4 SELECTs in store.rs had `close_reason, closed_by_session` line duplicated AND SnapshotBead was missing 4 fields causing round-trip default reset), filed bd-10e37c, started fixing — but msm-3 landed an identical fix concurrently. Stashed-changes conflict resolved by taking upstream. bd-10e37c now belongs to msm-3. Documented for the test-health log on bd-274c2d as wave #8.

Polish #6 candidate: investigate `releases.rs` per-project shape and decide whether the freshness-suffix pattern from polish #2 generalizes to that surface. Or pivot to something other than empty-state work since five cycles is a lot of one theme.
