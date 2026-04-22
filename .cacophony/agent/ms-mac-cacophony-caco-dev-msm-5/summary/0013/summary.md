# Session summary 0013 — bd-20381a: daemon.db unbounded-table audit

## Goal

Bd-20381a parent audit: enumerate every daemon.db table that grows
without time-based retention pruning, file per-table follow-up beads
where action is needed, and confirm tables that the bead's
hypothesis-list mentioned but which actually don't exist.

## Bead(s)

- `bd-20381a` (parent audit) — this commit.
- Filed children:
  - `bd-8bb05a` P2: notifications table has no retention.
  - `bd-713cd8` P2: images table has no retention (highest disk
    risk because of PNG blob size).
  - `bd-a0a229` P3: note_delivery_tracking has no retention.

## Before state

bd-727210 had recently established perf_events retention at 2 days,
but the question raised in bd-20381a was: do other daemon.db tables
share the same unbounded-growth pattern? The bead listed feed_events,
agent_events, message_history, spawn_history, reconcile_attempts as
likely candidates. No comprehensive audit existed.

## After state

Full audit complete. Every CREATE TABLE in `crates/caco-daemon/src/`
classified into 3 buckets: has-retention, doesn't-need-retention,
file-as-bug. Three new beads filed for the genuinely unbounded
tables; four hypothesised tables confirmed not to exist as separate
schema (they're typed rows in already-pruned feed_events /
project_messages).

## Audit method

Enumerated every `CREATE TABLE` in `crates/caco-daemon/src/`,
cross-referenced each table name against `DELETE FROM <table>`
patterns and `prune_retention` callsites. Tables with only ID-based
DELETEs (no time/age filter) and no entry in `store.rs::prune_retention`
were flagged.

## Findings

### Tables WITH retention (good)

| Table | Retention | Source |
|-------|-----------|--------|
| perf_events | 2 days | store.rs:432 (bd-727210) |
| feed_events | 7 days | store.rs:475 |
| exceptions | 7 days | store.rs:482 |
| operator_inbox_reads | 30 days | store.rs:452 |
| operator_inbox_archived | parent-cascading | store.rs:462 |
| project_messages (read direct) | via prune_read_direct | store.rs:486 |
| project_messages (expired) | via prune_expired | store.rs:492 |
| outbox | hours-based | outbox.rs:42 |

### Tables WITHOUT retention (filed as children)

| Table | Risk | Bead |
|-------|------|------|
| notifications | medium (text rows) | bd-8bb05a P2 |
| images | high (blob rows) | bd-713cd8 P2 |
| note_delivery_tracking | low (slow leak) | bd-a0a229 P3 |

### Tables WITHOUT retention but acceptable

| Table | Reason no retention needed |
|-------|----------------------------|
| agents | Not append-only; one row per active agent, deleted on terminal-purge. |
| node_state | One row per known node; updated in place. |
| project_state | One row per project; updated in place. |
| config_hashes | One row per node; updated in place. |
| spawn_queue | Self-draining queue (rows deleted on dispatch). |
| dynamic_nodes | One row per dynamic node; updated in place. |
| beads_primary_state | One row per project; updated in place. |
| scratchpad_notes | Operator-authored content; auto-pruning would be destructive. |
| messages (legacy table store.rs:282) | Project messages are pruned via the messaging.rs path; the bare `messages` table appears unused in the current schema. |

### Tables hypothesised by the bead but NOT FOUND

Confirmed these don't exist as separate tables, so no audit needed:
- `agent_events` — agent events flow through `feed_events` (already
  pruned) as typed rows.
- `spawn_history` — same; spawn lifecycle events are feed_events
  entries.
- `reconcile_attempts` — same.
- `message_history` — `project_messages` IS the message history; it
  has retention.

## Diff summary

- No code changes in this commit.
- 3 new beads filed (bd-8bb05a, bd-713cd8, bd-a0a229).
- 1 session summary (this file).

## Out of scope

The actual retention-policy implementations are scoped to the
3 child beads above so each can be claimed and reviewed
independently. The images-table fix in particular needs an
operator policy decision (cascade-delete vs. unreferenced-only) so
it shouldn't block the cheaper notifications fix.

## Operator-takeaway

3 daemon.db tables grow unbounded:
- **images** (highest risk — PNG blobs) → bd-713cd8.
- **notifications** (medium — text rows) → bd-8bb05a.
- **note_delivery_tracking** (low — slow leak) → bd-a0a229.

The bead's hypothesised `agent_events` / `spawn_history` /
`reconcile_attempts` / `message_history` aren't separate tables —
they're typed rows in `feed_events` (which IS pruned at 7d) or
`project_messages` (which has its own retention paths). So the
audit is narrower than feared.
