# Session summary — bd-91a14c msg `delivered_at` slice

## Goal

Land the smallest correct slice of bd-91a14c (msg
state-machine slice 2): give `project_messages` rows a
`delivered_at` column distinct from `read_at` and surface it
on `caco msg status`. Defer `--require-ack`, `--cc operator`
forwarding, and SSE delivery events to follow-ups since each
needs its own design pass.

## Bead(s)

- `bd-91a14c` — [bd-fd0ed4 follow-up] msg state-machine slice
  2: delivered_at distinct from read_at + --require-ack send
  mode + --cc operator forwarding + SSE delivery events.
  This cycle covers the **delivered_at** sub-item only.

## Diff summary

**`crates/caco-daemon/src/messaging.rs`:**

- `Message` struct gains `pub delivered_at: Option<DateTime<Utc>>`
  (serialized with `skip_serializing_if = "Option::is_none"`).
- `init_table` schema definition gains `delivered_at TEXT`
  next to `read_at`.
- New migration block (matching the existing `expires_at` /
  `reply_to` / `visibility` migration pattern) that ALTERs
  legacy tables and backfills:
    UPDATE project_messages SET delivered_at = ts
    WHERE delivered_at IS NULL;
  Existing rows are by definition delivered the moment their
  row was inserted — there's no separate transport step in
  this single-node store.
- `MessageStore::insert` stamps `delivered_at = Utc::now()`
  on insert when the caller did not pre-set it (replication
  / re-insert paths preserve original stamp). `with_default_ttl`
  is unchanged.
- INSERT statement includes `delivered_at` parameter.
- `get_message` SELECT projects `delivered_at` last so the
  parse_row tolerance (`row.get(12).ok().flatten()`) keeps
  older queries that don't request the column working.
- `parse_row` reads + parses RFC3339 into the field.
- 6 internal `Message { ... }` constructor sites in the same
  file gained `delivered_at: None,` so the `insert()` path
  remains the canonical place that stamps the time.
- 3 new unit tests:
  - `insert_stamps_delivered_at_when_unset`: calling
    `insert` with `delivered_at: None` stamps a wall-clock
    value bracketed by the Utc::now() bookends, distinct
    from `read_at` (which is None).
  - `insert_preserves_caller_supplied_delivered_at`:
    pre-setting `delivered_at` to `now - 60s` survives
    round-trip, second-resolution.
  - `migration_backfills_delivered_at_to_ts`: builds a
    pre-migration table by hand, inserts a row with a known
    `ts`, runs `init_table`, and asserts the backfilled
    `delivered_at` equals the original `ts`.

**`crates/caco-daemon/src/lib.rs` (`handle_msg_status`):**

- JSON response gains a `"delivered_at"` field (RFC3339 or
  null). Same `state_label` logic as before — no semantic
  change to the state machine.

**`crates/caco-daemon/src/ui_stream.rs` (drive-by):**

- `bead_snapshot_from_bead_preserves_project_and_description`
  test fixture missed two new `caco_beads::Bead` fields from
  bd-a23a7e (`last_seen_at: None`, `occurrence_count: 1`).
  Was blocking `cargo test -p caco-daemon --lib` cluster-wide.
  Filled in. Same pattern as the bd-1c0bdd / bd-c36993 sweeps.

**`crates/caco-tui/src/app.rs` (drive-by):**

- 8 `state::AttachMetadata` / `SessionKickedModal` literal
  sites missed `tmux_history_limit: None` /
  `tmux_history_size: None` after msd-2's bd-c36993 added
  them to the struct definitions. (My earlier bd-1c0bdd
  fix-forward had REMOVED them on the assumption the
  fields didn't belong on those structs; msd-2 chose the
  other resolution and added them. This sweep aligns with
  msd-2's choice.) Brace-walker insertion at each site.

## Before state

- `Message::delivered_at` did not exist.
- `caco msg status` returned `state: delivered | read |
  expired` based on `read_at` only — there was no separate
  signal for "row materialised here" vs "caller surfaced it".
- `cargo test -p caco-daemon --lib` failed to compile from the
  bd-a23a7e drive-by miss.
- `cargo test-small` failed to compile from the bd-c36993
  AttachMetadata / SessionKickedModal literal sites.

## After state

- New rows always have `delivered_at` populated.
- Legacy rows backfilled once on first init_table run after
  upgrade; subsequent inits are no-ops via the
  `pragma_table_info` guard.
- `caco msg status` JSON exposes `delivered_at`; clients can
  compute delivered-vs-read latency.
- Both broken-on-main test compiles green again.

## Notes / verification

- `cargo test-small` 56/56 green.
- `cargo test -p caco-daemon --lib messaging::tests` 70/70
  green (12 pre-existing + 3 new + 55 unrelated module tests).
- Migration tested against a hand-built pre-migration schema
  (the most common upgrade path).

## Out of scope

- `caco msg send --require-ack` blocking send mode — needs
  client-side polling design + timeout taxonomy. Filed-out
  for follow-up.
- `caco msg send --cc operator` undelivered-N-minutes
  forwarding — needs background sweep + operator-id
  resolution; defer until `--require-ack` lands so failure
  semantics are shared.
- SSE / feed event on `delivered` / `read` transitions —
  trivial mechanical change (add new feed event variant +
  emit at insert / mark-read sites) but every other slice
  of bd-91a14c already needs the variant, so let one of
  them ship it.
- Cluster-wide "delivered" semantics across replicating
  nodes — `delivered_at` here means "row landed in this
  store"; for replicated rows the timestamp will be slightly
  later than the origin. The status endpoint surfaces both
  `ts` and `delivered_at` so callers can disambiguate.

## Operator-takeaway

`caco msg status <id>` now returns a `delivered_at` field
distinct from `read_at`. Existing rows are backfilled on
first daemon start after upgrade; new rows are stamped on
insert. Same `state` label semantics — the API extension is
purely additive. Pairs with bd-fd0ed4 (parent slice 1).
Drive-by: re-fixed two broken-on-main test compiles
(bd-a23a7e occurrence sweep miss in ui_stream.rs +
bd-c36993 AttachMetadata literal sites in caco-tui/app.rs).
