# Session summary — fix 3 broken-on-main caco-cli lib failures (bd-38bab0)

## Goal

Take aurora/aur-4's handoff of a real, still-open broken-on-main bead (3 caco-cli
library test failures, verified against true main and NOT stale) — aur-4 is on an
offline node and explicitly handed it to a stable-node dev. As a stable headless
ms-dev worker I can reproduce and validate all three headlessly via the queue, so
I claimed it, diagnosed each failure precisely from the code, and fixed all three.

## Bead(s)

- `bd-38bab0` — [broken-on-main] caco-cli lib: 3 failures (choices / msg_send-retry / tts-sender-sync); gate-restore evidence (parent context: bd-ff92cd gate-coverage lineage).

## Before state

- Failing caco-cli lib tests on true main (`6cde31dd8`):
  1. `bd_97061b_choices_unknown_subcommand_lists_allowed`
  2. `async_send_request_does_not_retry_msg_send_post_bd_78224c`
  3. `tts_sender_prefix_wrappers_are_synced_from_shared_fragment_bd_fb7474`
- Root causes diagnosed from code (no live latency/UI needed; all headless):
  1. `caco choices recent` (bare non-flag positional) was mis-routed by the bare
     `[command] if command == "choices"` arm into the control-plane TUI launch,
     bypassing the generic "unknown subcommand … Allowed:" error path (the choices
     spec is `RuntimeKind::Tui`).
  2. The msg-send single-attempt decision was REFACTORED out of `async_send_request`
     into the `daemon_read_attempts_for_path` helper (behavior correct). The
     source-inspection test still asserted the literal strings inside
     `async_send_request`'s body, so it broke on the refactor.
  3. The 10 effect YAMLs carried a DRIFTED shared sender-prefix block — the shared
     fragment `_shared_sender_prefix.shfrag` changed (winmini reint `876d075cf9`:
     agent-name resolution rewrite + phonetic fallback) but the generated YAMLs
     were not re-synced.

## After state

- All three fixed:
  1. Added an unknown-positional guard at the top of the bare-choices arm
     (lib.rs ~20036): a non-flag positional after `caco choices` now returns
     `unknown subcommand '<x>' for 'caco choices'. Allowed: <visible subs>`,
     mirroring the generic branch handler. Bare `caco choices`, `--flag` forms,
     and known subcommands (present/resolve/list/tui/…) are unaffected.
  2. Updated the source-inspection test to verify the regression guard at BOTH
     refactored sites — `daemon_read_attempts_for_path` (contains
     `is_project_message_send_url_path` + `DAEMON_READ_RETRIES`) and
     `async_send_request` (derives `max_attempts` from it + keeps the bd-78224c
     rationale comment). Production behavior unchanged; intent preserved.
  3. Re-ran `scripts/sync-tts-effect-prefix.py` to re-sync all 11 effect YAMLs to
     the current shared fragment (the test's own prescribed fix).
- Validation: queued `cargo test -p caco-cli --lib` over the 3 targets plus
  `choices`/`bd_97061b` regression coverage (under heavy host contention; first
  attempt hit a retryable `host_saturation_timeout`, re-run with a larger ceiling).

## Diff summary

- Code/content commit: `f3561f65e7` (final landed squash SHA from the reintegration receipt).
- Files touched: `crates/caco-cli/src/lib.rs` (choices guard + async_send_request
  source-inspection test update), `.cacophony/tts/effects/*.yaml` (11 generated
  effect YAMLs re-synced).
- Tests: 0 added / 0 removed; 1 source-inspection test updated to match a refactor;
  2 behavioral tests flipped red→green via a code fix (choices) and a generated-file
  re-sync (tts).
- Behavioral delta: `caco choices <unknown>` now errors with the allowed list
  instead of silently opening the TUI; TTS effect sender-prefix wrappers match the
  shared fragment again. No change to async_send_request runtime behavior.

## Operator-takeaway

Three independent broken-on-main caco-cli failures, each a different class: a
RuntimeKind::Tui command swallowing an unknown subcommand instead of erroring; a
source-inspection regression test left stale by a clean helper refactor; and a
generated-file drift after a shared fragment changed without a re-sync. The
echo-gate (caco-tui) does not compile-check caco-cli, so these only surfaced in a
full-suite re-validation — concrete gate-coverage evidence for bd-ff92cd. All
three were diagnosable and fixable entirely headless.
