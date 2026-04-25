# Session summary — speak message id fallback

## Goal

Fix `bd-879ee1`, where some `caco msg speak` calls printed `speak message unknown` even though the daemon accepted and stored the speak request.

## Bead(s)

- `bd-879ee1` — caco msg speak returns message unknown for worker status updates

## Before state

- Failing tests: none; this was reported live by workers whose speak surface returned `speak message unknown`.
- Relevant metrics: the CLI human formatter only read `data.id`, but the daemon speak response can return `data.message_id` in accepted speak paths.
- Context: workers were falling back to broadcast for progress narration because the speak success line was ambiguous.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: CLI speak formatting now accepts either `data.id` or `data.message_id`, preferring `id` when both are present and using `accepted-no-message-id` only as an explicit last-resort token.
- Context: this preserves existing daemon response compatibility while avoiding the confusing generic `unknown` message id.

## Diff summary

- Commits: `95869a8f5`
- Files touched: `crates/caco-cli/src/msg_cmd.rs`
- Tests: added three unit tests for speak id extraction; ran `cargo test -p caco-cli speak_message_id -- --nocapture`, `cargo check -p caco-cli --tests`, and `cargo fmt --all`.
- Behavioural delta: accepted speak responses that carry `message_id` now print the actual stable message id.

## Operator-takeaway

Workers should no longer see a successful-but-confusing `speak message unknown` when the daemon returns the message id under its canonical `message_id` field.
