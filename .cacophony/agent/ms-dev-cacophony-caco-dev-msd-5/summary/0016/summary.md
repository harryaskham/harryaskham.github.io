# Session summary — bd-37c8f9 audio capabilities voice catalog

## Goal

Make `caco audio capabilities` agree with `caco tts voices` on the
Mai-Voice-Finetuned-1 variants the TTS daemon actually ships.

## Bead(s)

- `bd-37c8f9` — caco audio capabilities and caco tts voices report
  DIFFERENT voice sets

## Before state

- `caco audio capabilities` reported `Mai-Voice-Finetuned-1:M` only.
- `caco tts voices` reported all five variants `:M :D :R :K :N`.
- Active voice (often `:D`) wasn't in capabilities — operators
  conclude the active voice doesn't exist; bd-b327cd's eventual
  set-voice validator would reject known-good voices.

## After state

- FINETUNED_VOICES expanded to all five variants. capabilities and
  the daemon catalog now agree on the Mai-Voice-Finetuned-1 family.
- Comment in `crates/caco-daemon/src/audio.rs` documents the
  short-term fix vs. the deeper Option B reconciliation deferred to
  a follow-up.

## Diff summary

- 1 file modified, 37 insertions, 1 deletion:
  `crates/caco-daemon/src/audio.rs` (FINETUNED_VOICES + comment + 1
  new test).
- Tests: cargo test-small 179 passed; new
  `finetuned_voices_includes_all_five_mai_variants_bd_37c8f9`
  exercises `build_voice_list` against `localhost_fixture()`.

## Out of scope

- Option B reconciliation (audio capabilities reads provider config;
  tts voices reads the TTS daemon's loaded catalog) — file as
  follow-up if the divergence keeps biting.
- bd-b327cd set-voice validator (separate bead).
- Three-different-envelope-shapes triage (sibling of bd-bc52ef).

## Operator-takeaway

Single-bead surface fix that resolves the immediate operator
confusion. The deeper architectural drift (two surfaces with
different sources of truth) is real but worth keeping as a separate
bead — collapsing them prematurely could break other consumers of
either endpoint.
