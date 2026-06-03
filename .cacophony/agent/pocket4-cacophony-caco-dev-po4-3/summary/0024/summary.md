# Session Summary — bd-8ad093 (Expose tts.lang in TUI toggles and settings surfaces)

## Bead
**bd-8ad093** (P2 feature, labels: language, tts, tui, ui; oracle complexity 5/5, decompose)
"Expose tts.lang in TUI toggles and settings surfaces"

Dependent on bd-0d9db5 (the core `speech.tts.lang` config slice, landed earlier this
session at `8c1b89149`). This adds the full runtime-control + TUI-surfacing vertical for it.

## What was implemented
A complete vertical slice making the TTS daemon's default synthesis language viewable and
mutable from the TUI, applied immediately and persisted for session resumption:

### TTS daemon control (`crates/caco-cli/src/tts_daemon.rs`)
- `TtsDaemonStatusResponse.lang` — `GET /api/v1/tts/status` now reports the runtime lang.
- `SetLangRequest` + `handle_tts_ctrl_set_lang` + `POST /api/v1/tts/lang` route — sets the
  runtime language (blank resets to `en-US`) and persists it via `persist_tts_daemon_state`.

### CLI (`crates/caco-cli/src/lib.rs`)
- `caco tts set-lang --lang <code>` subcommand (CommandSpec + `TTS_SET_LANG_ARGS` +
  dispatch arm), MCP-enabled / agent-safe / idempotent, mirroring `set-voice`/`set-speed`.
- `dispatch_tts_control` `"set-lang"` arm POSTs `{"lang": ...}` to the daemon.

### TUI (`crates/caco-tui/src/`)
- `TtsDaemonLiveStatus.lang` (speech.rs) + parsed from the status JSON (app.rs).
- `speech_popup.rs`: a "Daemon Lang" settings row (reads live lang, falls back to `en-US`)
  and an `activate_row` mapping to `daemon_cycle_lang()`.
- `SpeechState::daemon_cycle_lang()` + `daemon_lang_cycle_pool()` + `DAEMON_LANG_PRESETS`
  (speech.rs): cycles through a preset list of common locale codes with the live language
  folded in so cycling always advances from the active value; applies via the daemon
  control server (immediate + persisted).

### Docs
- README `caco tts` command row lists `set-lang`.

## Acceptance criteria (all met)
- ✅ tts.lang appears in TUI settings menu (Daemon Lang row in the TTS Daemon tab).
- ✅ Language selection toggle available (cycle control via `daemon_cycle_lang`).
- ✅ Changes reflected immediately (POST `/api/v1/tts/lang` updates runtime + status poll).
- ✅ UI consistency with other TTS settings (same SettingRow + cycle pattern as
  Voice/Speed/Model).

## Decomposition
Oracle flagged complexity 5/5 / decompose. The cycle pool currently uses a built-in preset
list (live lang folded in). Sourcing it from the operator's configured `speech.tts.lang`
list is filed as follow-up **bd-3b2b2e** (non-blocking; the selector is already functional).

## Tests
- `daemon_lang_cycle_pool_defaults_to_en_us_first_bd_8ad093` — default pool, en-US first,
  no duplication.
- `daemon_lang_cycle_pool_folds_in_unknown_active_lang_bd_8ad093` — non-preset active lang
  folded in once; preset active lang not duplicated.
- `daemon_tab_rows_with_live_status` extended — asserts the Daemon Lang row + live value.

## Validation (queued on shared host per merge-queue policy)
- `cargo check -p caco-cli -p caco-tui --tests` → passed.
- `cargo test -p caco-tui daemon_lang` → 2 passed; `daemon_tab_rows_with_live_status` → passed.
- `cargo clippy -p caco-cli -p caco-tui --lib -- -D warnings` → passed.

## SPEC
Preserves the TUI speech-settings contract (consistent SettingRow presentation + cycle
activation) and the TTS daemon runtime-control + persisted-state pattern. CLI is canonical
(MCP generated from metadata).

## Diff
Landed squash SHA: see reintegration receipt.
