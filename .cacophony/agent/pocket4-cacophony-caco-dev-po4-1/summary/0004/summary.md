# Session summary — inbox sender-class filter

## Goal

Implement a profile-level inbound inbox filter so agents (especially
the router) can suppress agent-to-agent traffic and only receive
operator/user messages, cutting context noise without brittle
in-prompt heuristics.

## Bead(s)

- `bd-9969d1` — [profile] Add sender-class inbound filter (P1 feature)

## Before state

- `effective_subscriptions()` force-included Direct + Broadcast for
  every profile, even those that explicitly listed only `[direct]`.
- No sender-class filtering existed — all messages from all senders
  reached every agent's inbox. The router profile had to discard
  70%+ filler from agent broadcasts in-prompt.
- `InboxQuery` had no sender-class axis.

## After state

- New `InboxSenderClass` enum (operator, user, agent, controller,
  broadcast, self) + `InboxFilter { from: Vec<…> }` in caco-profile.
- New `inbox: Option<InboxFilter>` field on `AgentProfile`, composed
  last-one-wins.
- `effective_subscriptions()` no longer force-includes Direct +
  Broadcast when an inbox filter is set.
- `InboxQuery.sender_classes` in caco-daemon; `read_inbox_query()`
  filters post-assembly, pre-pagination.
- `handle_msg_inbox` resolves the caller's profile at read time and
  passes the filter through.
- 4 new unit tests in caco-profile (parse, absent, suppression,
  legacy compat).
- `cargo check -p caco-daemon` + `cargo test-small` green (162/162).

## Diff summary

- Commits: `d366f311a`
- Files touched:
  - `crates/caco-profile/src/model.rs` (+78 lines: enum, struct, field, accessors)
  - `crates/caco-profile/src/compose.rs` (+5 lines: last-one-wins)
  - `crates/caco-profile/src/lib.rs` (+62 lines: 4 tests + fixture field)
  - `crates/caco-profile/src/bridge.rs` (+4 lines: fixture fields)
  - `crates/caco-daemon/src/messaging.rs` (+65 lines: filter logic)
  - `crates/caco-daemon/src/lib.rs` (+30 lines: resolver + wiring)
- Tests: +4 (inbox_filter_parses_from_yaml, inbox_filter_absent_means_no_filter,
  inbox_filter_suppresses_forced_direct_broadcast,
  no_inbox_filter_still_forces_direct_broadcast).

## Operator-takeaway

The inbox filter is a daemon-side enforcement — messages are dropped
before they reach the agent's inbox SQL read, not in-prompt. The
controller heuristic (sender ID contains "ctrl"/"controller") is
imperfect but matches the fleet's naming convention. A future
improvement would store the sender's auth scope on the message row
at write time so the filter doesn't need to guess. The router.md
profile should now add `inbox: { from: [operator, user] }` to get
the noise reduction the operator asked for.
