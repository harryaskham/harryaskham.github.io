# bd-14e75e: caco choices reissue — bump pending choice with new urgency

## Goal

Implement Slice 1 of bd-14e75e: a way for an agent (or operator) to bump a still-pending choice with a stronger notify_mode (e.g. `escalate`, `broadcast`) without invalidating the `choice_id`. Pairs with bd-939541 (notify_mode field) and bd-a167d6 (no-operator-response threshold-cross).

## Bead(s)

- bd-14e75e (claimed; closes via reintegrate)

## Before state

`notify_mode` was set at presentation time and immutable for the choice's lifetime. If the operator didn't respond and the choice grew stale, the agent had to either re-`present` (new choice_id; loses correlation), or just spam `speak` messages.

## After state

End-to-end implementation of the reissue flow:

**1. Persistence layer (`crates/caco-daemon/src/operator_inbox.rs`)**
- Added `pub notify_mode: Option<String>` to `ChoiceData` (the inbox-row payload).
- `from_active_choice` now copies `notify_mode` in; `to_active_choice` round-trips it back out.
- New helper `update_active_choice_payload(db, item_id, choice_data)` runs `UPDATE operator_inbox SET choice = ?, updated_at = ? WHERE id = ? AND status = 'active'` — first-writer-wins guard means terminal items are unmoved.

**2. Domain layer (`crates/caco-daemon/src/choices.rs`)**
- New `ChoicesState::reissue(choice_id, new_notify_mode) -> Option<ActiveChoice>`. Returns the updated choice on success, `None` if missing or already terminal.
- Imports `ChoiceData` from `operator_inbox`.

**3. HTTP layer (`crates/caco-daemon/src/choices.rs` + `lib.rs`)**
- New `ReissueChoiceRequest { notify_mode: Option<String> }` body.
- New `handle_reissue_choice` handler. On success: emits a `choice_presented` UI event (signal-only for slice 1 — slice 2 will route through supervisor for actual channel-escalation) and returns `{ reissued: true, choice_id, notify_mode }`. On miss: `{ reissued: false, error: "choice not found or already resolved (bd-14e75e)" }`.
- Route `POST /api/v1/choices/{choice_id}/reissue` wired in `daemon::lib.rs::router`.

**4. CLI layer (`crates/caco-cli/src/lib.rs`)**
- New `CHOICES_REISSUE_ARGS` (`--choice-id` required, `--notify-mode` optional).
- New `CommandSpec` entry under choices subcommands (mcp_enabled, agent_safe, idempotent).
- New dispatch arm `[cmd, sub] if cmd == "choices" && sub == "reissue"`.
- New `dispatch_choices_reissue(choice_id, notify_mode, json_requested)` (~60 LOC). Validates `notify_mode` against the canonical allowed set client-side (bd-eb84c8 pattern: reject bogus values before round-trip). Posts to `/api/v1/choices/{id}/reissue`, returns either JSON-pretty or human-friendly status.

**5. Tests (`crates/caco-daemon/src/choices.rs`)**
- 4 new tokio tests on `ChoicesState::reissue`:
  - `reissue_active_choice_updates_notify_mode`: persistence round-trips
  - `reissue_nonexistent_returns_none`: no false success
  - `reissue_resolved_choice_returns_none`: first-writer-wins (cannot resurrect terminal)
  - `reissue_with_none_notify_mode_clears_field`: explicit clear semantics

Verification:
- `cargo test -p caco-daemon --lib choices::tests::reissue`: 4/4 PASS
- `cargo build -p caco-daemon --tests`: clean
- `cargo build -p caco-cli`: clean
- `cargo test-small`: 57/57 PASS
- `cargo clippy --workspace --all-targets -- -D warnings`: clean

## Diff summary

4 files changed, +210 / −5:

- `crates/caco-daemon/src/operator_inbox.rs`: +25 / −2 (`notify_mode` field on ChoiceData; round-trip in 2 helpers; new `update_active_choice_payload` function)
- `crates/caco-daemon/src/choices.rs`: +120 / −2 (`reissue` method; `ReissueChoiceRequest` + `handle_reissue_choice`; 4 unit tests)
- `crates/caco-daemon/src/lib.rs`: +4 (router wire)
- `crates/caco-cli/src/lib.rs`: +90 / −0 (CHOICES_REISSUE_ARGS, CommandSpec, dispatch arm, dispatch_choices_reissue)

## Operator-takeaway

This is a thin, surgical Slice 1: the data plumbing (persistence + endpoint + CLI). It deliberately does **not** route the bumped notify_mode through the actual notification channel — that's slice 2's supervisor work, and it'd entangle this PR with the choice-supervisor refactor.

What slice 1 enables today: an agent can call `caco choices reissue --choice-id X --notify-mode escalate`, and the choice's `notify_mode` field is persisted. Anyone subscribing to `choice_presented` UI events sees the bump. The supervisor can then (in slice 2) read the new field and route through the stronger channel.

For the autonomy_tier escalation pattern (bd-a167d6 threshold-cross), this is the cheap building block: the agent's poll-loop notices the choice is over the threshold and calls `reissue --notify-mode escalate` before falling back to `--notify-mode broadcast` after another threshold. No choice_id churn, no superseded-events to clean up.

Validation: `caco choices reissue --choice-id <real-id> --notify-mode escalate` should print `reissued choice <id> (notify_mode=escalate)`. Test against a missing id should print `reissue failed: choice not found or already resolved (bd-14e75e)` with non-zero exit.

Slice 2 follow-up: thread `reissue` through the supervisor's notify-channel-routing (currently only consulted at `present` time).
