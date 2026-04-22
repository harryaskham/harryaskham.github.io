# Session summary — bd-45ea63 audio speak per-invocation overrides (slice 1)

## Goal

Land the small, immediately-deliverable slice of bd-45ea63: add
`--speed` and `--instructions` flags to `caco audio speak` so callers
can override `values.speed` and pass voice-instructions for a single
synthesis without round-tripping through config edits. File two
follow-up beads for the larger SSML express-as block and voice-filter
post-processing pieces.

## Bead(s)

- `bd-45ea63` — TTS: ssml.azure_express_as block + caco audio speak
  --filter/--express-as/--speed flags (re-scoped to "partial — slice 1
  landed; follow-ups bd-68bde8 and bd-5b199b track the rest")
- `bd-68bde8` — (filed) caco audio speak --voice-filter post-processing
- `bd-5b199b` — (filed) SSML azure_express_as config block + --express-as flag

## Before state

- `caco audio speak` accepted `--model`, `--voice`, `--text`, `--format`,
  `--output`, `--stdout`. No way to override the configured speed or
  pass voice-instructions per call.
- `SpeechRequest` on the daemon side already had `speed: Option<f32>`
  and `instructions: Option<String>` fields — they were just unreachable
  from the CLI surface.
- `.cacophony/tts.yaml` carried a `# TODO (bd-45ea63)` block for the
  full Azure SSML express-as design.

## After state

- Two new flags on `caco audio speak`:
  - `--speed <f32>` — parsed, validated, forwarded as `speed` in the
    `/api/v1/audio/speech` request body.
  - `--instructions <string>` — forwarded as `instructions`.
- Both flags omitted by default so the daemon's existing config-resolved
  defaults remain in effect; only present in the JSON body when the
  caller supplied them.
- New helper `build_audio_speech_request_body` extracted for testability.
- Unit tests cover both invariants (omitted-defaults vs all-set).
- Two follow-up beads (`bd-68bde8`, `bd-5b199b`) carry the larger
  voice-filter pipeline and SSML express-as workstreams. The original
  bead description is updated to flag the partial landing and link
  follow-ups.

## Diff summary

- Files touched: `crates/caco-cli/src/lib.rs` (CLI surface, dispatch,
  helper extraction, two unit tests; `#[allow(clippy::too_many_arguments)]`
  on the now 9-arg dispatch fn).
- Tests: +2, all pass.
- Behavioural delta: `caco audio speak --speed 1.4 --instructions "warm
  and conspiratorial" --text "..."` now reaches the daemon's existing
  speed/instructions handling instead of falling back to defaults.

## Operator-takeaway

Small, immediately-useful slice landed in isolation rather than
holding back on the full bd-45ea63 multi-week SSML + filter
implementation. The two follow-up beads carry the remaining work,
and the parent bead description now tells the story of the partial
landing so anyone picking it up next will know which file the
unfinished bits live in.
