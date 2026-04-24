# Session summary — bd-128ea6 stabilize scribble STT

## Goal

Operator reports the existing scribble/whisper STT integration is
flaky. Acceptance criteria 4 (structured-log event_class), 5
(`caco stt --doctor`), and 6 (failure-mode README) — the
diagnose-and-document subset that future flake fixes can build on.

## Bead(s)

- `bd-128ea6` — [stt-xplat] Stabilize existing scribble/whisper STT
  integration (flaky → reliable): VAD tuning, error paths, tests
  (re-scoped from "engine work" per operator note 2026-04-23)

## Before state

- `crates/caco-daemon/src/scribble_stt.rs` had four ad-hoc
  `eprintln!` calls and no machine-parseable log key. Empty
  transcripts were indistinguishable from VAD-dropped-all in logs.
- No subcommand for an operator to walk "is my STT actually working?"
  — required reading code + crafting curl by hand.
- No documented catalog of failure modes; flake reports had to be
  rediagnosed from scratch every time.

## After state

- New `SttEventClass` enum + `.log()` method emits canonical
  `stt_event_class=<key> k=v ...` lines on every model-load,
  transcribe-start/done, and error path. VAD-dropped-all is its own
  key, distinct from empty-result, so downstream UX can render
  "no speech detected" affordances instead of silent empty bubbles.
- New `caco stt doctor` CLI: walks config, posts a self-test WAV
  from `tests/stt-corpus/`, prints colour-coded ok/warn/fail per
  finding. JSON variant for scripting.
- `scribble_stt.rs` module docs now have a "Failure modes" section
  tying each operator-reported flake source to its event-class hook.
- Bonus fixup: `peer_consult_timeout_ms` was missing from 6 test-site
  struct literals after my own bd-cef230 landed, breaking workspace
  compile on main; reported by ms-mac-cacophony-caco-tui, fixed in
  this commit.

## Diff summary

- 7 files modified (~370 LOC net new):
  - `crates/caco-daemon/src/scribble_stt.rs` (event class + plumbing
    + module docs + 1 test)
  - `crates/caco-cli/src/lib.rs` (stt subcommand + doctor dispatcher
    + 1 registration test)
  - `crates/caco-daemon/{src/beads.rs,src/election.rs,tests/multinode.rs}`,
    `crates/caco-sidecar/src/lifecycle.rs` (peer_consult fixup)
  - `crates/caco-stt-bench/src/main.rs` (context body shape fix)
- Tests: 146 test-small + new lib tests pass.

## Remaining scope (follow-ups worth filing)

- VAD false-negative auto-bypass: when `vad_dropped_all` fires twice
  in a row for a session, retry once with VAD off and surface
  "VAD bypassed" in UI.
- Audio-format autodetect: surface a structured "decode" error when
  the input isn't WAV/MP3/etc, with a hint about supported formats.
- caco stt doctor --rotate: cycle through all 50 corpus clips and
  print per-category pass-rate (composes with bd-68b76d corpus).

## Operator-takeaway

Two knobs an operator now has when STT acts flaky:

```
caco stt doctor                        # one-shot diagnostic
journalctl -u caco-daemon | grep stt_event_class=  # canonical key
```

The latter answers "was it VAD or the model?", "how long did it
take?", and "what's the error class?" without code-diving.
