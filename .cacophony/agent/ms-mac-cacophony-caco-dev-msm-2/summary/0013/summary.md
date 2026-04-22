# Session summary — caco msg send --reply-to (bd-f8f754)

## Goal

Expose the existing daemon-side `reply_to` field as a flag on `caco msg send` so operators can thread messages without using the separate `msg reply` subcommand (which requires --target and lacks auto-resolve).

## Bead(s)

- `bd-f8f754` — caco msg send: --reply-to <msg-id> for threaded conversations + inbox shows reply-graph.

## Before state

- Daemon `POST /api/v1/projects/{p}/messages/send` already accepted `reply_to` and persisted to `project_messages.parent_msg_id` (since bd-1616c1).
- CLI did not expose it on `msg send`; operators had to use `msg reply` which requires `--target` and rejects bare invocations.

## After state

- `caco msg send --reply-to <msg-id>` threads the new message under an existing one.
- Body construction extracted to `build_msg_send_request_body()` (4 args + Option<&str> reply_to) so the omit/include behaviour is unit-testable in isolation.
- 2 new tests cover both branches.
- Smoke test on the live daemon: parent looked up from inbox tail-1, send with `--reply-to`, message arrived in own inbox confirming end-to-end plumbing.
- `cargo test-small` clean, `cargo check --workspace --tests` clean.

## Diff summary

- Commit: `1c09045f`
- Files touched: `crates/caco-cli/src/lib.rs` (+64 / -7).
- Tests: +2 unit; 0 removed; 0 flipped.
- Behavioural delta: opt-in flag; existing scripts unaffected.

## Out of scope (filed implicitly for follow-up)

- `msg reply` auto-resolve target from message-id — needs `GET /api/v1/projects/{p}/messages/{id}` or an inbox scan.
- `caco msg thread <msg-id>` chronological thread view.
- Inbox / SSE / web / TUI surface rendering of the reply graph.

## Operator-takeaway

`caco msg send --target X --body Y --reply-to <msg-id>` is the new threaded-send primitive; surfacing the thread graph in inbox/web/TUI is independent follow-up work.
