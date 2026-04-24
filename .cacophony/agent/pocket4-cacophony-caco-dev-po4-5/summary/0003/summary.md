# Session summary — Clarify voice catalog divergence between `caco audio capabilities` and `caco tts voices`

## Goal

Close bd-f57ea3: operators see different voice sets from `caco audio
capabilities` vs `caco tts voices` and cannot tell whether it's a bug
or intentional. Document the distinction on both surfaces so the
difference is self-explanatory.

## Bead(s)

- `bd-f57ea3` — Voice catalog drift: 'caco audio capabilities' lists
  Gemini voices that 'caco tts voices' omits — clarify or unify

## Before state

- `caco audio capabilities` lists 30+ voices (OpenAI + Gemini + Azure
  + finetuned) with no explanation of scope.
- `caco tts voices` lists ~16 voices (OpenAI defaults + config custom)
  with no explanation of scope.
- Operator sees divergence, assumes bug.

## After state

- `caco audio capabilities` appends a footer: "Note: voices is the
  union across configured providers (OpenAI + Gemini + Azure +
  finetuned). For only the voices loaded by the running TTS daemon
  use `caco tts voices`."
- `caco tts voices` appends: "Note: this lists voices loaded by the
  running TTS daemon (OpenAI defaults + speech.tts.voices custom
  entries). For the full cross-provider catalog (incl. Gemini
  Aoede/Charon/...) use `caco audio capabilities`."
- JSON output unchanged.

## Diff summary

- Commit: bd-f57ea3 clarify voice catalog divergence
- Files: `crates/caco-cli/src/lib.rs` (+26 lines)
- Tests: no new tests (render-only change); cargo test-small green
  (182 passed).
- Behavioural delta: two clarifying footer hints in human-readable
  text output. No functional change.

## Operator-takeaway

The two surfaces intentionally show different scopes: `audio
capabilities` = all-provider union (for discovery), `tts voices` =
running daemon's loaded set (for what-works-right-now). The footers
make this self-documenting so operators stop filing it as a bug.
