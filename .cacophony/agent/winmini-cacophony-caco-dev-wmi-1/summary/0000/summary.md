# Session summary — bd triage 405 leak, mutual-exclusion, bd stalled --threshold validator

## Goal

Fix three sibling-misses left over from the prior `bd-be23e4` raw-daemon-error-leak sweep and the `bd-bc52ef` flag-validator family: `caco bd triage --promote/--discard/--defer` were leaking raw `daemon returned 405:` envelopes regardless of bead-id validity, conflicting triage action flags were silently accepted, and `caco bd stalled --threshold bogus` was silently parsing as 0 instead of erroring with the same shape as `caco summary --since`.

## Bead(s)

- `bd-33b6d9` — caco bd triage --discard/--promote/--defer leak raw 'daemon returned 405:' on missing/invalid bead-id and on conflicting flag combos; caco bd stalled --threshold bogus silently treats as 0

## Before state

- `caco bd triage --discard --bead-id bd-nosuch` → `error: daemon returned 405:` (raw HTTP status, empty body)
- `caco bd triage --promote --discard --bead-id bd-nosuch` → same raw 405, no client-side mutual-exclusion guard
- `caco bd stalled --threshold bogus` → returned the full in_progress list as if `--threshold 0`
- Failing tests: none (these were operator-facing UX bugs, not test failures)

## After state

- `caco bd triage --discard --bead-id bd-nosuch` → `error: bead not found: bd-nosuch` (polished daemon-error wording flows through)
- `caco bd triage --promote --discard --bead-id bd-nosuch` → `error: caco bd triage accepts only one action flag per invocation; got --promote, --discard (pick one of --promote / --discard / --defer)`
- `caco bd stalled --threshold bogus` → `error: invalid --threshold value 'bogus' (expected e.g. 6h, 90m, 1d)`
- `caco bd stalled --threshold 5h` → renders normally
- Failing tests: none. Added 3 new lib tests:
  - `parse_duration_secs_rejects_garbage_strings`
  - `bd_triage_dispatcher_enforces_action_mutual_exclusion`
  - `bd_triage_actions_use_patch_not_put`
- `cargo test-small` green (57 passed); `cargo clippy -p caco-cli` clean.

## Diff summary

- Commit: 12d2532a0
- File: `crates/caco-cli/src/lib.rs` (+107 / −7)
- Three changes:
  1. Switch all four triage-action HTTP requests (promote / discard / defer + the `do_patch` sub-helper) from `Method::PUT` to `Method::PATCH` so they hit `handle_update_bead` (the daemon route is registered as PATCH/DELETE/GET only). Root cause of the 405.
  2. Add mutual-exclusion guard in `dispatch_bd_triage`: collect which of `--promote/--discard/--defer` were given; if more than one, return `invalid_argument` listing the conflicting flags.
  3. Add `parse_duration_secs` validation for `--threshold` in `dispatch_bd_stalled` before rewriting flags into the worker-age view; mirrors the `--since` validator wording.
- Tests: +3 unit tests in the existing `tests` module of `caco-cli`.
- Behavioural delta: three CLI surfaces stop leaking raw daemon HTTP status / silently dropping malformed input; instead they error with operator-actionable messages.

## Operator-takeaway

When a CLI dispatcher mints its own URL+method instead of going through the shared `bd_send_request` / `bd_daemon_result` plumbing, it will skip both the daemon-error-message extraction and the polished-restart envelope path. The fix here is small (PATCH not PUT, plus a mutual-exclusion guard) but the structural lesson is that any future `caco bd <verb>` action that needs to mutate a bead should route through the same builder helpers as `bd update` rather than rolling its own request — otherwise it inherits this exact class of bug. Worth a follow-up sweep to audit any remaining hand-rolled mutating endpoints.
