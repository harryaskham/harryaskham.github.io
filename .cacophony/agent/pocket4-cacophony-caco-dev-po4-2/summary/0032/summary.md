# Session summary — message success acks no longer print fake 'unknown' ids

## Goal

Fix the operator-facing CLI acknowledgement bug where successful messaging
commands could print the literal word `unknown` as if it were a real message ID.
This was actively confusing workers on live nodes: `caco msg speak` success text
like `speak message unknown in project cacophony` looked like a failure, so some
workers fell back to noisy broadcast updates. The goal was to make success text
unambiguously success-shaped while staying local to the CLI formatter layer.

## Bead(s)

- `bd-50c446` — `caco msg speak` returns `speak message unknown` — confusing success ack causes workers to broadcast instead

## Before state

- `crates/caco-cli/src/msg_cmd.rs` formatted several messaging success paths by
  reading only `resp["data"]["id"]` and falling back to the literal string
  `"unknown"` when that field was absent.
- At least some daemon messaging responses use `message_id` rather than `id`, so
  valid successful requests could still render as:
  - `speak message unknown in project ...`
  - related send/broadcast/reply/loop output with `unknown` in the success text
- That wording looked like an error and caused avoidable coordination noise.

## After state

- The CLI now resolves accepted message IDs from either:
  - `data.id`
  - `data.message_id`
- The formatter rejects the literal sentinel `unknown` as a usable success ID.
- When the daemon accepts a request without returning any message ID, success
  text now stays explicit and success-shaped instead of pretending `unknown` is
  the ID:
  - speak → `speak accepted in project ... (daemon returned no message id)`
  - send → `message send accepted to ... in project ... (daemon returned no message id)`
  - related reply / broadcast / loop output follows the same pattern
- `--require-ack` on send now errors clearly if the daemon accepted the request
  without returning any message ID, rather than attempting to poll delivery for
  a fake `unknown` id.

## Diff summary

- Files touched:
  - `crates/caco-cli/src/msg_cmd.rs`
  - `crates/caco-cli/src/lib.rs`
- Helper additions:
  - `accepted_message_id(...)`
  - `format_send_acceptance(...)`
  - `format_speak_acceptance(...)`
- Tests added:
  - `accepted_message_id_prefers_id_then_message_id_and_rejects_unknown`
  - `format_speak_and_send_acceptance_avoid_unknown_success_ids`
- Validation:
  - `cargo fmt --all`
  - `cargo test -p caco-cli accepted_message_id_prefers_id_then_message_id_and_rejects_unknown -- --nocapture`
  - `cargo test -p caco-cli format_speak_and_send_acceptance_avoid_unknown_success_ids -- --nocapture`
  - `cargo build -p caco-cli`
- Behavioural delta:
  - successful messaging operations no longer print `unknown` as a fake message
    ID, so workers see clear success text and are less likely to mis-route
    progress into noisy fallback broadcasts.

## Operator-takeaway

This was a pure CLI acknowledgement bug, not a daemon-delivery failure. The
underlying requests were succeeding; the formatter just assumed one exact field
name and then rendered the fallback sentinel as if it were a real message ID.
The landed fix makes success text trustworthy again and also hardens the related
send/broadcast/reply/loop success paths against the same response-shape drift.
