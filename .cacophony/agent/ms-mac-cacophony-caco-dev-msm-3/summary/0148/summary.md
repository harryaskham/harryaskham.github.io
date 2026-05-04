# Session summary — bd-fb719b Pi post-compaction continuation

## Goal

Fix `bd-fb719b`: managed Pi agents must continue their assigned work automatically after `/self-compact` or runtime context compaction, instead of relying on an operator/controller to notice an idle post-compaction prompt and manually nudge the worker.

## Changes

- Updated `plugins/caco-agent/pi-extensions/caco-lifecycle.mjs`:
  - post-compaction lifecycle handling still queues the immediate follow-up continuation nudge;
  - added a bounded idle-recovery watchdog after compaction;
  - if the Pi runtime is still idle after the recovery delay and the managed lifecycle is non-terminal, it sends an immediate runtime-native user message to continue the same project/agent/bead;
  - records durable `caco-lifecycle` entries for scheduled recovery, active-session observation, terminal-state skips, recovery nudges, and recovery failures;
  - the post-compaction continuation text explicitly preserves managed project, agent id, assigned bead, profile, and lifecycle instructions.
- Added `plugins/caco-agent/pi-extensions/caco-lifecycle.test.mjs` with Node regression tests for:
  - ordinary follow-up post-compaction nudge delivery;
  - idle recovery sending an immediate user message;
  - active sessions not receiving duplicate recovery nudges;
  - terminal lifecycle states skipping recovery nudges.
- Added `crates/caco/tests/pi_lifecycle.rs` so the Node lifecycle regression suite is reachable from Cargo validation.
- Updated `SPEC.md`, `README.md`, `AGENTS.md`, and `specs/pi-agent-widgets.md` to document the post-compaction continuation + idle-recovery contract.

## Validation

- `node --test plugins/caco-agent/pi-extensions/caco-lifecycle.test.mjs` — passed.
- `rustfmt --edition 2021 --check crates/caco/tests/pi_lifecycle.rs` — passed.
- `git diff --check` — passed.
- `cargo test -p caco pi_lifecycle -- --test-threads=1` — passed.
- `cargo clippy -p caco --tests --no-deps -- -D warnings` — passed.

## Notes

- I did not drive a live production Pi agent through compaction; this slice adds the first-party runtime hook/watchdog and regression coverage for the continuation behavior.
- The recovery watchdog defaults to 15 seconds and can be overridden with `CACO_PI_COMPACTION_IDLE_RECOVERY_DELAY_MS` for targeted runtime validation.
