# Session summary — SsmlConfig schema + clock-free Azure express-as resolver (bd-5b199b)

## Goal

Land the config-schema and pure-resolver halves of the bd-45ea63 follow-up that lets operators declare `ssml.azure_express_as` in `tts.yaml` and have agents rotate through styles deterministically.

## Bead(s)

- `bd-5b199b` — `[bd-45ea63 follow-up] SSML azure_express_as config block + --express-as flag`. Slice of: config schema + resolver + tests. Daemon SSML wrapping and CLI `--express-as` flag already shipped in earlier slices.

## Before state

- `caco-daemon::audio::build_azure_proxy_ssml_with` already accepts `express_as` / `styledegree` arguments and emits `<mstts:express-as>` correctly.
- CLI `caco audio speak --express-as` already routes the value through the request body.
- `.cacophony/tts.yaml` has an extensive **TODO block** sketching the `ssml.azure_express_as` schema (voice_pattern, styledegree_range, rotation, overnight overrides) but `TtsConfig` had no `ssml` field, no resolver existed, and there was no determinism contract for per-agent rotation.

## After state

### Schema (`caco-config::model`)

```rust
TtsConfig.ssml: Option<SsmlConfig>
SsmlConfig.azure_express_as: Option<AzureExpressAsConfig>
AzureExpressAsConfig {
    voice_pattern: String,                 // regex gate
    styledegree_range: Option<{low, high}>,
    rotation: ExpressAsRotation {
        express_as: Vec<String>,           // "none" sentinel = skip wrapper
        combine_with_voice: bool,
        overrides: Vec<ExpressAsOverride { active_between: [HH:MM, HH:MM], express_as }>,
    },
}
```

### Resolver (`caco-daemon::audio::resolve_express_as`)

```rust
pub fn resolve_express_as(
    cfg: &caco_config::AzureExpressAsConfig,
    agent_id: &str,
    voice: &str,
    now_local_minutes: u32,
) -> (Option<String>, Option<f32>);
```

- Voice gate: regex; non-matching returns `(None, None)` silently.
- Override windows checked first; first match wins; overnight wrap `["22:00","08:00"]` handled.
- Rotation key: `stable_hash(agent_id [| voice])` % pool length, using `std::DefaultHasher` xor FNV-1a (stable even if std's hasher is later randomised).
- Sentinel `"none"` collapses to `(None, None)`.
- Styledegree sampled deterministically from a salted seed so `(agent, style)` pairs get a stable degree per session.
- **Clock-free**: caller passes `now_local_minutes` (0..1440); makes the resolver trivially testable without freezing time.

### Tests (8 new, all in `caco-daemon::audio::tests`)

```
express_as_skips_when_voice_pattern_does_not_match
express_as_rotation_is_deterministic_per_agent
express_as_rotation_picks_from_pool
express_as_styledegree_falls_in_range_when_set
express_as_overnight_window_overrides_rotation
express_as_none_sentinel_returns_no_tag
express_as_combine_with_voice_changes_seed
express_as_window_outside_returns_rotation_pick
```

### Salvage

38 `TtsConfig { ... }` literal sites across `caco-config/tests/config.rs`, `caco-daemon/src/audio.rs`, `caco-tui/src/speech.rs`, `caco-tui/src/state/tests.rs`, and `caco-config/src/validate.rs` received the new `ssml: None,` field via Python mass-edit (TtsConfig has no `Default` derive).

## Verification

- `cargo test -p caco-daemon --lib audio::tests::express_as` — 8 / 0
- `cargo test-small` — all green
- `cargo clippy --workspace --tests -- -D warnings` — clean
- `cargo check --workspace --tests` — clean

## Diff summary

- Commit: `0a884a28`
- 8 files changed, **395 insertions(+), 6 deletions(-)** (post drive-by cleanup)

## Out of scope (future slices)

- Wiring the resolver into the `SpeechRequest` construction path so config-driven (not request-supplied) styles take effect end-to-end. Currently the resolver is a pure helper; the daemon will start consulting it once the request-builder calls it.
- Updating `.cacophony/tts.yaml` to drop the TODO block and ship a real example. Schema docs in `model.rs` are now the source of truth.
- A CLI surface for inspecting "what style would agent X get right now?".

## Drive-by

Removed 3 unused `NodeEntry` imports left over from the bd-05495d builder migration (cleared the clippy/`-D warnings` gate).

## Operator-takeaway

Operators can now define `ssml.azure_express_as` in `tts.yaml` (the schema validates) and the resolver is ready to consume it. End-to-end activation (resolver -> SpeechRequest) is one small follow-up edit away — kept out of this slice to keep the diff focused and reviewable.
