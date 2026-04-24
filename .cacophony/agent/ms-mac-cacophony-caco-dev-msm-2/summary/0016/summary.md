# Session summary — caco tts set destructive-input regression pins (bd-56198d)

## Goal

Confirm bd-56198d's three "still-broken" issues (`--voice ''`,
`--speed=999`, `--speed=-1` via inline-equals) are actually
fixed by the bd-205b39 validation routing landed earlier this
session, and pin the contract with explicit regression tests
so a future refactor that drops the validation lights up
loudly.

## Bead(s)

- `bd-56198d` — caco tts set v1.2.528 still-broken: --voice ''
  / --speed unbounded / --speed -1 / catalog drift
  (P2, bug, test-user)

## Before state

- Test-user probe on installed `caco 1.2.528` confirmed three
  `caco tts set` paths mutated live cluster TTS state on
  invalid input and the operator had to baseline-restore.
- bd-205b39 (closed earlier this session) routed dispatch_tts_set
  through `validate_tts_voice` / `validate_tts_model` /
  `validate_tts_speed` — so all three issues SHOULD already be
  fixed in the source on disk, but no explicit regression test
  pinned the empty-string nor the negative-inline-form contract.

## After state

Verified against locally-built `./target/debug/caco`:

```
$ ./target/debug/caco tts set --speed=-1
error: --speed -1 is out of range; allowed: 0.25..=4
$ ./target/debug/caco tts set --voice=''
error: --voice must not be empty
$ ./target/debug/caco tts set --speed=999
error: --speed 999 is out of range; allowed: 0.25..=4
```

All three paths reject BEFORE any cluster-mutating POST.
TTS daemon state was preserved (verified via `tts status
--json` showing speed=1.25 baseline restored).

Two new regression tests pin the contracts:

1. **`bd_56198d_voice_and_model_validation_rejects_empty_string`**
   — calls `validate_tts_voice` and `validate_tts_model`
   directly with `""` against a dead base URL (port 1 / TCPMUX
   so any HTTP call would short-circuit to Ok per the
   validator contract). The empty-string check fires BEFORE
   the HTTP call; we assert both `--voice must not be empty`
   and `--model must not be empty` errors surface.
2. **`bd_56198d_speed_validation_rejects_negative_inline_form`**
   — pins that `"-1"` (the literal that reaches dispatch_tts_set
   from the inline `--speed=-1` form) parses as f32 -1.0 and
   gets rejected as out-of-range. Also pins the explicit
   `0.25..=4` documented-bounds surface in the error text per
   the gold-standard validator pattern.

## Diff summary

- Files touched:
  - `crates/caco-cli/src/lib.rs` — 2 new tests in the existing
    `tests` module (no production-code changes; bd-205b39
    already shipped the fix)
- Tests: +2 / -0
- Test command:
  `cargo test -p caco-cli bd_56198d` → 2 passed.

## Out-of-scope follow-ups (NOT closed by this bead)

- **Issue 5b (parser ambiguity)** — bare `caco tts set --speed -1`
  (without `=`) errors with `unsupported flag: -1` rather than
  routing to `--speed`'s value. This is a parser-surface issue
  affecting many flags that take negative numbers; warrants its
  own bead (parser pattern, not tts-specific). Verified at the
  CLI:
  ```
  $ ./target/debug/caco tts set --speed -1
  error: unsupported flag: -1
  ```
- **Issue 6 (catalog drift)** — `caco audio capabilities` lists
  Gemini voices (Aoede, Puck, Charon...) that `caco tts voices`
  doesn't. May be intentional (capabilities = all-provider
  voices, tts voices = currently-configured-provider only) but
  the operator can't tell. Pure doc/UX bead; warrants its own
  filing once the intentional-vs-bug call is made.

Both are explicitly noted in the close so the operator can
file targeted follow-ups without re-discovering them.

## Operator-takeaway

bd-56198d's three actionable issues (`--voice ''`, `--speed`
unbounded high, `--speed=-1` inline) are CLOSED and pinned by
explicit regression tests. The test-user-discovered bug class
"empty-string-bypass that mutates state" is fully gated for
the tts-set surface.

The two follow-up surfaces (parser-ambiguity bare `-1`; voice
catalog drift between `audio capabilities` vs `tts voices`)
are documented in the close note for separate filing.

Honored constraints:
- No `cargo test --workspace`; targeted single-test run.
- No daemon changes — pure test additions confirming
  bd-205b39's already-shipped fix.
- Operator no-narrator rule honored — claim + close speaks
  issued by msm-2 directly.

19th bead closed this session (cumulative). 12th in this turn.
