# Session summary — `caco tts set` validation parity (bd-205b39)

## Goal

Make `caco tts set` validate `--voice` / `--model` / `--speed`
inputs against the daemon's enumeration endpoints BEFORE the
cluster-mutating POST, matching the bd-b327cd pattern already
applied to `set-voice` / `set-model` / `set-speed`.

## Bead(s)

- `bd-205b39` — Align caco tts set with set-* subcommands
  (P2, caco, cli, consistency)
- Sibling: `bd-a96ff3` (closed earlier this turn) — added the
  `--filter` arg; THIS bead closes the validation-parity drift.

## Before state

- `set-voice` / `set-model` / `set-speed` validated inputs
  via `validate_tts_voice` / `validate_tts_model` /
  `validate_tts_speed` BEFORE issuing the POST (per bd-b327cd:
  prevents typo'd values from silently breaking live cluster
  TTS until next utterance).
- `dispatch_tts_set` (the unified setter from bd-410108) did
  NOT call any of the validate_tts_* helpers — operators
  could ship a typo'd voice name through `caco tts set --voice
  <typo>` and only discover the failure at the next TTS
  utterance.

## After state

- `dispatch_tts_set` invokes all three validators upstream of
  the POST:
  - `validate_tts_voice(&client, &base, voice)` when `--voice`
  - `validate_tts_model(&client, &base, model)` when `--model`
  - `validate_tts_speed(parsed)` when `--speed` (parses first
    via the same `--speed must be a valid float` error that
    set-speed uses)
- Network failures during validation continue to fall through
  to the dispatch path per the existing `validate_tts_*`
  contract (so a transient unreachable-daemon doesn't turn
  into a hard refusal).
- `--filter` continues to use `parse_tts_filter_flag`
  (bd-a96ff3); no validation change there.

## Diff summary

- Files touched:
  - `crates/caco-cli/src/lib.rs` — `dispatch_tts_set` body
    extended with three validate_tts_* call sites + bd-205b39
    comment block; one new test
- Tests: +1 / -0 / flipped 0
  - `tts_set_dispatch_invokes_validation_helpers` — source-
    level contract test that scans the `dispatch_tts_set`
    function body and asserts all three `validate_tts_*`
    helpers are referenced. Without this assertion a future
    refactor could drop validation from the unified setter
    without any other test failing (the live HTTP path can't
    be exercised from tests).
- Test command:
  `cargo test -p caco-cli tts_set_dispatch_invokes_validation_helpers`
  → 1 passed (after 2m10s recompile).

## Operator-takeaway

`caco tts set --voice <typo>` now errors immediately with
"unknown TTS voice '<typo>' — available: …" instead of
silently accepting the value and breaking the next utterance.
Same for `--model <typo>` and `--speed <out-of-range>`.

Behaviour parity with `set-voice` / `set-model` / `set-speed`
is now complete:

| Validation | set-voice | set-model | set-speed | tts set (before) | tts set (after) |
|---|---|---|---|---|---|
| Voice known to daemon | ✓ | — | — | ✗ | ✓ |
| Model known to daemon | — | ✓ | — | ✗ | ✓ |
| Speed in range / finite | — | — | ✓ | ✗ | ✓ |
| Filter on/off vocabulary | — | — | — | ✓ (bd-a96ff3) | ✓ |

Network-failure fallback unchanged: a transient unreachable
daemon during validation still falls through to the dispatch
path; only enumerated-but-not-matching values hard-error.

Honored constraints:
- No `cargo test --workspace`; targeted single-test run.
- No daemon-side surface touched (parity is at the CLI
  validation layer, which is where the original bd-b327cd
  pattern lives).
- Operator no-narrator rule honored — claim + close speaks
  issued by msm-2 directly.

16th bead closed this session (cumulative). 9th in this turn.

The bd-205b39 + bd-a96ff3 pair brings `caco tts set` to full
parity with the set-* family. If there's further drift to
chase (instance-name flag? json-mode envelope shape?) it
would belong in a follow-up bead.
