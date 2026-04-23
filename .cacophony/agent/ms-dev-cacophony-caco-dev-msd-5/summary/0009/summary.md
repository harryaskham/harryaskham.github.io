# Session summary — bd-68b76d STT corpus + call-reliability harness

## Goal

Ship a CI-runnable WER + voice-call-reliability harness for the
already-shipped scribble STT integration. Not building STT from
scratch — the corpus exists to *defend* the live voice-call UX
(operator → controller) by catching regressions in agent-id and
command-verb recognition before they hit operators.

## Bead(s)

- `bd-68b76d` — [stt-xplat] Synthetic test corpus + WER harness:
  50+ ground-truth clips covering commands, agent ids, bead ids,
  noise (re-filed after bd-128ea6 retitle to engine work)

## Before state

- Existing scribble integration (`crates/caco-daemon/src/scribble_stt.rs`,
  TUI surfaces in `views/speech_popup.rs`) had no test corpus, no
  WER measurement, no regression gate. Operator reported it as flaky.
- No call-reliability metric existed: a hypothesis with 5% WER could
  still drop the agent name and route the wrong worker.

## After state

- New crate `caco-stt-bench` (~570 LOC, 15 unit tests) with a
  `SttEngine` trait and three implementations:
  - `mock` (CI default): echoes ground truth or reads
    `predicted/<id>.txt` sidecars; lets gate logic be tested without
    bundling any STT model.
  - `external <cmd>`: shells out per clip.
  - `daemon --base <url> --model scribble`: POSTs each WAV to a
    running daemon's `/api/v1/audio/transcription`. Drives the *real
    shipped scribble transcriber* — same code path the TUI speech
    popup uses.
- Corpus `tests/stt-corpus/`:
  - `manifest.json`: 50 clips across 6 categories.
  - `audio/`: 50 espeak-ng-synthesized WAVs (~4 MB).
  - `synth.sh`: idempotent regen recipe.
  - `must_recognize` tokens on 30 of 50 clips covering every agent
    id, every command verb, and the agent/action spans of operator
    phrases.
- CI `.github/workflows/stt-bench.yml`: WER gate (≤12% overall,
  ≤3pp per-category regression) + call-reliability gate (≥95% of
  must-recognize clips).

## Diff summary

- Files: 9 new (crate sources + corpus + CI workflow + README + 50
  WAVs + synth script), 2 modified (workspace Cargo.toml + Cargo.lock).
- Tests: 15 unit (gate-trip in 4 directions). `cargo test-small`:
  140 passed. Live mock-engine bench: PASS, 100% call-reliability.

## Operator-takeaway

The bench is the *measurement surface* for every subsequent
hardening change to scribble. To validate a scribble fix:

```
cargo run -p caco-stt-bench --release -- \
    --engine daemon --base http://127.0.0.1:12100 --model scribble
```

The numbers print per-category, name the missing must_recognize
tokens on each failing clip, and exit non-zero if any gate trips —
so a flaky-vs-fixed scribble change is a one-command answer.

Next pulled: bd-c16753 (visual STT indicators in TUI, P0) — the
first of the new visual-feedback beads from the re-scoped epic.
