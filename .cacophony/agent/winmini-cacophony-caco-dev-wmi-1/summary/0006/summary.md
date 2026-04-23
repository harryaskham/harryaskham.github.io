# Session summary — bd-db10da: STT indicator hide toggle

## Goal

Land AC5 from the bd-c16753 STT visual-indicators umbrella: the operator
can hide the always-on STT visual-state dot in the speech indicator via
the speech popup, the preference persists across restarts, and the mic
+ dB level block remains visible regardless.

## Bead(s)

- `bd-db10da` — [stt-ux] AC5: speech-popup toggle to hide STT visual
  indicator (split off bd-c16753).

## Before state

- `SpeechState` had no field representing operator preference for
  whether the STT visual-state dot is visible. The dot in
  `speech_indicator_spans` rendered unconditionally whenever
  `capabilities.stt_available`.
- `PersistedMuteState` only stored `muted: bool`; no surface for any
  other persisted operator preference.
- The speech popup view exposed Input Mute, Read Messages Aloud, and
  STT model rows, but no row for hiding the STT dot.

## After state

- `PersistedMuteState` gains a `stt_indicator_hidden: bool` field with
  `#[serde(default)]` so existing on-disk files continue to load (a
  new test asserts the back-compat path).
- `SpeechState` gains a runtime `stt_indicator_hidden: bool` field
  (default `false`) and a `toggle_stt_indicator_hidden()` method that
  flips the in-memory flag and best-effort persists it alongside
  `muted` when a `mute_state_path` is configured. Persistence is
  skipped when `state_readonly` is set.
- On `SpeechState::from_speech_config`, the persisted file is read and
  `stt_indicator_hidden` is restored independently of mute_policy so
  the preference survives whether or not the operator uses local mute.
- `speech_indicator_spans` checks the new flag and skips the
  red/grey/green/amber dot block when hidden. The mic + dB level block
  (bd-bcf470 / bd-56e4b1) is unaffected — the entire indicator
  surface still renders.
- The speech popup view exposes a new "STT Indicator Dot" row with
  values "VISIBLE" / "HIDDEN" and wires both the typed activation path
  and the click-router branch to call the new toggle method.

## Diff summary

- 3 files changed, +166 / -8:
  - `crates/caco-tui/src/speech.rs` — add `stt_indicator_hidden` to
    `PersistedMuteState` (+ `#[serde(default)]`) and to `SpeechState`,
    update `Default`, restore from disk in `from_speech_config`,
    update both `PersistedMuteState` write sites, add
    `toggle_stt_indicator_hidden()`, and add 5 unit tests.
  - `crates/caco-tui/src/views/speech_indicator.rs` — early-return
    branch in the dot-render block + 1 unit test that confirms the
    dot drops while the broader indicator surface continues to render.
  - `crates/caco-tui/src/views/speech_popup.rs` — add
    "STT Indicator Dot" row to `stt_rows` and route it in both the
    typed-activation and click-router branches.

## Validation

- `cargo test -p caco-tui --lib stt_indicator_hidden`: **5 passed,
  0 failed**.
- `cargo test -p caco-tui --lib persisted_mute`: **4 passed, 0 failed**
  (existing tests still pass; back-compat test asserts that an
  on-disk file containing only `{"muted":true}` loads with
  `stt_indicator_hidden = false` defaulted).
- `cargo check -p caco-tui --tests`: clean.

## Operator-takeaway

This is one of three sub-beads of the bd-c16753 STT-UX umbrella. With
this in, the operator can suppress the dot without losing any other
speech-indicator information; the mic + dB block remains the always-on
recording surface. The two remaining children are bd-7a8bc1 (P1 error
toast with open-doctor link) and bd-88798a (P2 partial ghost text +
final-commit flash) — those need new state-machine surfaces and a
dedicated tab-bar overlay row, larger than this one. After all three
land, bd-c16753 can close.
