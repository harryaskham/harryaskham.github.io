# Session summary — remediation notifications prepared through canonical messages

## Goal

Finish the non-destructive notification/nudge slice of the agent-health remediation ladder by adding a side-effect-free dispatch preparation helper. The helper should only prepare canonical `Message` values after the existing cooldown/attempt gate allows an action, leaving actual persistence/delivery to the execution layer.

## Bead(s)

- `bd-f8be23` — Send remediation notifications via canonical message surface
- Parent context: `bd-829a2c` — Execute remediation notification and nudge actions
- Dependency context: `bd-bc4281` — Track remediation notification cooldown and attempt state

## Before state

- `bd-bc4281` had already added pure cooldown/attempt state helpers for `NotifyOperator` and `NudgeAgent`.
- `bd-4b3258` had already added the pure remediation execution intent model that maps decisions to first-party surfaces.
- There was no helper connecting those two pieces to the canonical messaging model after the cooldown gate passed.

## After state

- Added `RemediationNotificationDispatch`, carrying the prepared canonical message, updated attempt state, cooldown decision, and diagnostic string.
- Added `RemediationNotificationDispatchError` for side-effect-free rejection cases: unsupported action, cooldown/max-attempt block, action/surface mismatch, disabled direct execution, missing project, and missing sender.
- Added `remediation_notification_dispatch(...)`, which:
  - refuses destructive or choice-required intents;
  - evaluates the cooldown/attempt gate before creating a message;
  - maps `NotifyOperator` to `Message::speak(...).with_default_ttl()`;
  - maps `NudgeAgent` to `Message::direct(...).with_default_ttl()` targeted at the agent in the attempt key;
  - returns the next attempt state for callers to record after accepting the send.

## Diff summary

- Code/content commits: `a5dff9735` (`bd-f8be23: prepare remediation messages after cooldown gate`)
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched:
  - `crates/caco-daemon/src/agent/health.rs`
- Tests: +4 focused unit tests / -0 / flipped 0
- Validation:
  - `git diff --check`
  - queued `RUST_MIN_STACK=33554432 cargo test -p caco-daemon bd_f8be23 -- --test-threads=1 --nocapture` via `caco test run`, passed as `tj-00be8010`
- Behavioural delta: downstream remediation execution code can now use one pure helper to prove that a notification/nudge should be sent through the canonical message model, and it cannot accidentally construct messages before the cooldown/attempt gate passes.

## Operator-takeaway

This slice still does not mutate lifecycle state or deliver messages itself. It narrows the future execution layer to safe first-party message shapes and makes the ordering explicit: decide intent, pass cooldown, prepare canonical message, then record/send outside this helper.
