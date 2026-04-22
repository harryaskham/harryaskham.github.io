# Session summary — bd-c55c0c: stop TUI from replaying 2-week-old chat as live

## Goal

Stop the TUI from surfacing weeks-old chat messages as if they were live
traffic when a node comes back online after a long quiet period. Two
layered defects had to be fixed together: full-state replication shipped
its 50 newest chat events with no age bound, and the TUI hydration path
appended them straight into the live chat surfaces with no marker.

## Bead(s)

- `bd-c55c0c` — TUI startup replays 2-week-old chat messages as fresh:
  full-state chat_history has no age cutoff, hydrate_chat_from_history
  doesn't mark backfill

## Before state

- Failing tests: none related; `[broken-on-main]` for
  `agent_attach_help_shows_id_and_raw_args` (caco-cli) acknowledged via
  inbox as unrelated breakage being tracked by another worker.
- `crates/caco-daemon/src/replication.rs` capped `chat_history` to 50
  rows by count only (`FULL_STATE_MAX_CHAT_HISTORY = 50`), no age bound.
- `crates/caco-tui/src/state/mod.rs::hydrate_chat_from_history` pushed
  historical rows directly into `global_chat`/`project_chats` with no
  flag, so backfill was indistinguishable from live SSE-applied chat.
- TUI bubble renderer had no concept of "historical" messages.

## After state

- Failing tests: none from this change. `cargo test-small` 195 + 107 +
  716 + 277 + 18 + 2775 + 43 = all green. New unit tests pass:
  `bound_chat_history_by_age_drops_stale_rows_and_keeps_fresh`,
  `bound_full_state_dump_drops_stale_chat_rows`, and an updated
  `hydrate_chat_from_history_populates_global_and_project` that asserts
  the new `hydrated=true` invariant.
- Daemon: `FULL_STATE_CHAT_HISTORY_MAX_AGE_SECS = 24*60*60` and a new
  `bound_chat_history_by_age` retain-filter applied inside
  `bound_full_state_dump` before the count cap. Unparseable timestamps
  are kept (fail open).
- TUI: `ChatMessage` gains `hydrated: bool`. All ~71 existing
  constructors set `hydrated: false`; the two struct literals inside
  `hydrate_chat_from_history` set `hydrated: true`. The chat bubble
  renderer in `crates/caco-tui/src/views/chat.rs` draws a centred
  `— previously —` / `— live —` divider on transitions and dims the
  hydrated bubble border colour to `NORD3`.

## Diff summary

- Commits: `bd7b93e3`
- Files touched:
  - `crates/caco-daemon/src/replication.rs` (age constant +
    `bound_chat_history_by_age` + invocation in `bound_full_state_dump`
    + 2 new unit tests)
  - `crates/caco-tui/src/state/mod.rs` (`hydrated` field + 2 hydrate
    sites set it to true)
  - `crates/caco-tui/src/state/tests.rs` (new assertion)
  - `crates/caco-tui/src/views/chat.rs` (divider + dimmed border for
    hydrated bubbles)
  - `crates/caco-tui/src/app.rs` (mechanical `hydrated: false` adds for
    existing ChatMessage constructors)
- Tests: +3 (2 daemon + 1 invariant added to existing TUI test). 0
  removed, 0 flipped.
- Behavioural delta: full-state pushes drop chat rows older than 24h
  before the 50-row count cap; TUI tags hydrated bubbles and renders
  them as backfill, not live traffic.

## Operator-takeaway

A long-offline node will no longer "wake up" with two weeks of stale
gossip pretending to be live chat. Anything older than 24h is now
silently dropped at full-state push time, and anything that does come
in from a snapshot is rendered behind a `— previously —` divider with
a dimmed border so the operator knows immediately that they are looking
at backfill, not new activity. Knob to tune later if needed:
`FULL_STATE_CHAT_HISTORY_MAX_AGE_SECS` (currently a `pub const`; a
config-level override is the natural follow-up if 24h proves wrong on
real-world lag patterns).
