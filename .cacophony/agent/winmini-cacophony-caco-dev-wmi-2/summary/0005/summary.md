# Session summary — TUI inbox poll-error surfacing (bd-bf1e86)

## Goal

bd-bf1e86 is the permanent caco-tui polish bead — small, taste-driven
papercut hunting. This cycle: surface inbox poll errors instead of
silently swallowing them.

## Bead(s)

- `bd-bf1e86` (permanent, P2)

## Before state

`crates/caco-tui/src/views/inbox.rs` rendered a clean
`InboxLoadState::Error` branch (`"Failed to load inbox"`)... but the
load function `App::request_inbox_poll` collapsed every failure into
an empty list:

```rust
let items = client.fetch_operator_inbox().await.unwrap_or_default();
```

Result: the Error UI was unreachable dead code, and the operator saw
a blank inbox during a daemon hiccup with no signal that anything
went wrong. Especially disorienting because "no messages" and "lost
connection" rendered identically.

## After state

Errors are threaded end-to-end:

1. `ActionResult::InboxPollFailed { error }` event variant added
   (`crates/caco-tui/src/event.rs`) + Debug impl entry.
2. `TuiState::inbox_last_error: Option<String>` field added,
   defaulting to `None`.
3. `request_inbox_poll` now matches on `Result` and emits
   `InboxPolled` or `InboxPollFailed` with the `to_string()` error.
4. App handler:
   - On `InboxPolled` success: clear `inbox_last_error`, merge items.
   - On `InboxPollFailed`: store error; flip into `Error` state
     **only if no prior snapshot exists** (graceful degradation —
     a good cached inbox stays visible across a transient blip).
5. View `InboxLoadState::Error` branch renders the captured error
   message and a `Press 'r' to retry` hint instead of the vague
   `"Failed to load inbox"`.

Net effect: the previously-dead Error UI fires for the first time,
and transient daemon hiccups no longer blank a previously-good inbox.

## Tests

Two new in `caco-tui state::tests`:
- `inbox_last_error_starts_none`
- `inbox_last_error_round_trips_through_state`

`cargo test-small`: **4214 PASS / 0 FAIL** (+4 from baseline).
`cargo clippy --workspace --all-targets -- -D warnings`: PASS clean.

## Diff summary

Commit (this session, post-rebase): `1d9aa394`
Files (5 / +71 / -6):
- `crates/caco-tui/src/event.rs` — `InboxPollFailed` variant + Debug.
- `crates/caco-tui/src/state/mod.rs` — `inbox_last_error` field +
  `Default` init.
- `crates/caco-tui/src/state/tests.rs` — 2 new tests.
- `crates/caco-tui/src/app.rs` — `request_inbox_poll` error wiring;
  handler for both `InboxPolled` (clear) and `InboxPollFailed` (store
  + conditional Error transition).
- `crates/caco-tui/src/views/inbox.rs` — Error branch shows real
  error + retry hint.

## Operator-takeaway

A two-part papercut closure: the silent-empty-on-failure
disorientation goes away, and the previously-dead
`InboxLoadState::Error` UI now actually fires. Graceful-degradation
choice (keep last-good inbox if you ever loaded one) means a 200ms
daemon hiccup doesn't blank your inbox screen; it only triggers the
banner if you never had data to begin with. That feels right for
"subtle enhancements" — the loud failure mode appears only when it
matters.

Permanent bead remains open for the next polish cycle.
