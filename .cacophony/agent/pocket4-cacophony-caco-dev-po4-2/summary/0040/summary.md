# Session summary — update-helper now states that Pi /loop overrides generic caco cron guidance

## Goal

Take the next unresolved profile-audit slice after the board-preflight/control-plane fix and resolve the recurring-work ambiguity for persistent update-helper. The target was `bd-57904c`, where generic persistent guidance about `caco cron` could still be misread even though this Pi-managed profile already intended `/loop` to own the 20-minute release cadence.

## Bead(s)

- `bd-57904c` — Profile audit: recurring-work instructions mix caco cron with Pi /loop for persistent update-helper

## Before state

- `update-helper.md` already said the old `caco-release` cron was removed and already instructed startup through `/loop`.
- `pi-loop.md` already said `/loop` is the preferred runtime-native path and said not to create daemon-global `caco cron` entries for per-agent loops.
- But neither place stated the precedence sharply enough to neutralize generic persistent-agent cron guidance on startup, so the reified prompt still left room to misread “use caco cron” as applicable to update-helper.

## After state

- Updated `.cacophony/profiles/pi-loop.md` to say explicitly that when `pi-loop` is composed, it overrides generic persistent-agent `caco cron` guidance for per-agent recurring work.
- Updated `.cacophony/profiles/update-helper.md` to say explicitly that update-helper’s recurring cadence is owned by the Pi `/loop` path, not by generic `caco cron` guidance.
- Added a targeted profile test in `crates/caco-profile/tests/profile.rs`:
  - `update_helper_profile_prefers_pi_loop_over_caco_cron_bd_57904c`

## Diff summary

- Files touched:
  - `.cacophony/profiles/pi-loop.md`
  - `.cacophony/profiles/update-helper.md`
  - `crates/caco-profile/tests/profile.rs`
- Validation (queued, first-party):
  - `caco test run --wait true --command "cargo test -p caco-profile update_helper_profile_prefers_pi_loop_over_caco_cron_bd_57904c -- --nocapture" --cwd "$PWD"`
  - `caco build run --wait true --command "cargo build -p caco-profile" --cwd "$PWD"`
- Behavioural delta:
  - repo-owned prompt text now states the intended precedence directly: for Pi-managed per-agent recurring work, `/loop` owns the cadence and generic `caco cron` guidance does not apply unless a role/operator explicitly overrides it

## Operator-takeaway

This was another prompt-precedence cleanup, not a scheduler implementation change. The repo now says more plainly what update-helper was already trying to do: its 20-minute cadence belongs to the Pi `/loop` path, and agents should not reintroduce daemon-global `caco cron` just because generic persistent guidance elsewhere mentions it.
