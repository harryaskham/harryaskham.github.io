# Session summary — bd-842add: TTS normalizer now actually applies dash cleanup

## Goal

Burn down the open TTS-quality bead reporting bizarre spoken rewrites /
can't-speak behaviour from narrator-style text. The example in the bead is
full of punctuation-heavy status prose, em-dashes, curly quotes, and timing
phrases. The immediate goal was to find a concrete, code-local defect in the
TTS normalization pipeline and land it with a regression test.

## Bead(s)

- `bd-842add` — `broken weird tts rewrites cant speak see example`

## Before state

The TTS normalization pipeline in `tts_daemon_normalize_speakable(...)` did:

1. replace bead IDs with spoken names
2. normalize curly quotes
3. normalize duration shorthand (`1h`, `44m`, ...)
4. normalize compact metrics (`key=value`, `word(s)`, ...)

But it **did not call** `caco_daemon::llm::normalize_dashes_for_tts(...)`, even
though that helper already existed with tests and a very explicit comment:

> Some TTS providers mis-pronounce em-dashes (e.g. reading `foo — bar`
> as "foo A A E A bar").

So the codebase already knew dashes were a TTS footgun, but the actual TTS
normalizer path was skipping the fix.

## After state

`tts_daemon_normalize_speakable(...)` now applies dash normalization between
quote normalization and duration expansion:

- `Narrator update — quiet stable slice`
- becomes
- `Narrator update - quiet stable slice`

Added regression test:

- `tts_daemon_normalize_speakable_flattens_em_dash_garbage`

Validation:

- `cargo test -p caco-cli --lib tts_daemon_normalize_speakable_flattens_em_dash_garbage` — pass
- `cargo test -p caco-cli --lib tts_daemon_normalize_speakable` — pass (4 tests)
- `cargo build -p caco-cli` — clean
- `cargo test-small` — 185 passed, 0 failed

## Diff summary

Files touched:

- `crates/caco-cli/src/lib.rs`
- `.cacophony/agent/winmini-cacophony-caco-dev-wmi-2/summary/0023/summary.md`

Code changes:

1. `tts_daemon_normalize_speakable(...)`
   - inserted `caco_daemon::llm::normalize_dashes_for_tts(&text)` into the
     actual normalization pipeline
   - added bead reference comment explaining why this matters for providers
     that speak em/en dashes as garbage

2. tests
   - added `tts_daemon_normalize_speakable_flattens_em_dash_garbage`
   - existing TTS normalization tests still pass

## Operator-takeaway

This bead had at least one concrete bug: the TTS stack had a dash-cleanup helper
but never actually used it in the real narration path. That is now fixed.

Important nuance: the grotesque sample in the bead likely contains more than one
punctuation issue (there may still be mojibake / provider-specific weirdness in
some cases), but this specific omission was real, locally testable, and exactly
in the class of "weird TTS rewrites / can't speak narrator text". If the
operator still hears corrupted narration after this lands, the next follow-up
should capture one fresh raw input string + one raw spoken/output transcript so
we can isolate the remaining transform instead of guessing.