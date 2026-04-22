# bd-bf1e86 polish: merge-queue panel "updated Ns ago" freshness indicator

## Goal

Operators staring at the `Cluster > Merge Queue` panel can't tell if the data they're seeing is fresh (just polled) or stale (last poll was 2 minutes ago and the daemon may have hiccupped). This polish cycle adds a compact relative-time freshness indicator to the panel title, so the difference between "live" and "stale" is glanceable.

## Bead(s)

- bd-bf1e86 (P2 permanent — caco-tui polish; remains open after reintegrate per endless-mode rules)

## Before state

- `Cluster > Merge Queue` title was static: ` Merge Queue ─ window: 24h ─ in-flight: 3 ─ recent: 12 `.
- No way to tell when the poll last fired. Both fresh data and 5-minute-stale cached data look identical.
- `(no data)` and `loading…` placeholders also gave no temporal context.
- `merge_queue_fetched_at` field did not exist on `TuiState`.

## After state

- New `TuiState::merge_queue_fetched_at: Option<chrono::DateTime<Utc>>` set every time `MergeQueueFetched` or `MergeQueueFailed` is processed (both code paths).
- `views::merge_queue::format_relative_secs(secs)` formats compact human durations: `5s`, `2m`, `1h`, `3d`. Saturates at days. Local helper to avoid pulling in a relative-time crate.
- `render()` appends ` ─ updated Ns ago` to the panel title whenever a fetch has occurred. Suffix omitted only on cold-start (no fetch yet, "loading…" already conveys it).
- Both the data-present and `(no data)` titles get the suffix.
- 11-case unit test covers all duration buckets including boundary values (59s/60s, 3599s/3600s, 86399s/86400s, week).

Verification:
- `cargo test -p caco-tui --lib views::merge_queue::tests::format_relative_secs_buckets_correctly`: PASS
- `cargo test-small`: 56/56 PASS
- `cargo clippy --workspace --all-targets -- -D warnings`: clean

## Diff summary

3 files changed, +69 / -3:

- `crates/caco-tui/src/state/mod.rs`: +5 (field declaration + Default-impl initializer)
- `crates/caco-tui/src/app.rs`: +2 (set timestamp in MergeQueueFetched + MergeQueueFailed handlers)
- `crates/caco-tui/src/views/merge_queue.rs`: +62 (helper fn + helper test + suffix wiring) / -3 (old static-title arms)

## Operator-takeaway

Tiny UX win that pays off every time a poll hiccups silently: the operator sees `updated 47s ago` in the title and immediately knows whether to retry or trust. The same pattern (timestamp the last fetch, render relative-age in the title) is reusable for every other periodically-polled subpanel — release queue, agents list, inbox, feed. If this lands cleanly we should propagate to those sites.

bd-bf1e86 is permanent — not closed at reintegrate. The next polish cycle on this bead can either propagate the freshness indicator to other panels or pick a different inbox/merge-queue papercut. The previous polish (bd-bf1e86's earlier inbox-error surfacing) is already in main; this is the second cycle on the same permanent bead.
