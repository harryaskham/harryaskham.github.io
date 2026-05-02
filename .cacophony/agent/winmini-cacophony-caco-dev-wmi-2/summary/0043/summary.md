# Session summary — TTS effect wrappers stop drifting to ambient node-style prefixes

## Goal

Fix the TTS effect regression where many agents were being prefixed with the same NATO-spelled phrase — reported as repeated "November Oscar Delta" — instead of using the actual speaking agent identity. The goal was to make the effect wrappers prefer the sender metadata the daemon already knows, and to stop them from deriving misleading prefixes from ambient daemon-style environment values.

## Bead(s)

- `bd-b48e4d` — TTS effect agent prefix regressed to repeated November Oscar Delta

## Before state

- The daemon already injected sender-specific command-template env vars such as `CACO_SPEAK_AGENT_ID`, `CACO_SPEAK_AGENT_SHORT_NAME`, and `CACO_SPEAK_AGENT_NAME`.
- The duplicated effect wrapper block in `.cacophony/tts/effects/*.yaml` still preferred older ambient compatibility variables like `CACO_AGENT_ID`, `CACO_NODE`, and `CACO_PROJECT`.
- If those ambient values did not resolve cleanly, the wrappers NATO-spelled the first three characters of `CACO_AGENT_ID`, which matched the observed repeated bogus prefix behavior.
- The existing daemon test from `bd-3dacb5` proved env injection existed, but it did not pin wrapper-side preference order or forbid ambient NATO fallback.

## After state

- All ten effected TTS wrapper files now prefer sender-specific metadata in this order:
  - `CACO_SPEAK_AGENT_NAME`
  - `CACO_SPEAK_AGENT_SHORT_NAME`
  - derived name from `CACO_SPEAK_AGENT_ID` / `CACO_SPEAK_NODE` / `CACO_SPEAK_PROJECT`
  - existing `agent.json` short-name fallback if present
- The wrappers no longer NATO-spell ambient fallback values like `CACO_AGENT_ID` when sender-specific identity is unavailable.
- Prefixes are now omitted when no trustworthy sender-specific identity can be resolved, instead of inventing a misleading node-ish label.
- Daemon test coverage now also checks:
  - `CACO_SPEAK_AGENT_NAME` injection
  - sender-specific identity winning over ambient compatibility vars
  - wrapper files themselves containing the sender-specific preference path and no old ambient NATO fallback block

## Diff summary

- Commit: `988bc62b0` — `bd-b48e4d: prefer sender-specific TTS effect prefixes`
- Files touched:
  - `.cacophony/tts/effects/alien-transmission.yaml`
  - `.cacophony/tts/effects/arcade-glitch.yaml`
  - `.cacophony/tts/effects/cyberdeck.yaml`
  - `.cacophony/tts/effects/haunted-cathedral.yaml`
  - `.cacophony/tts/effects/helmet-comms.yaml`
  - `.cacophony/tts/effects/mission-control.yaml`
  - `.cacophony/tts/effects/noir-tape.yaml`
  - `.cacophony/tts/effects/numbers-station.yaml`
  - `.cacophony/tts/effects/subway-pa.yaml`
  - `.cacophony/tts/effects/walkie-talkie.yaml`
  - `crates/caco-daemon/src/audio.rs`
- Tests / validation:
  - `cargo test -p caco-daemon --lib audio::tests::run_tts_command_template_injects_sender_metadata_bd_3dacb5 -- --exact --nocapture`
  - `cargo test -p caco-daemon --lib audio::tests::run_tts_command_template_sender_name_beats_ambient_prefix_fallback_bd_b48e4d -- --exact --nocapture`
  - `cargo test -p caco-daemon --lib audio::tests::tts_effect_wrappers_prefer_sender_specific_identity_bd_b48e4d -- --exact --nocapture`
  - `cargo build -p caco-daemon`
  - `cargo clippy -p caco-daemon --all-targets --no-deps -- -D warnings`
- Behavioural delta:
  - TTS effect prefixes should now come from the actual speaking sender instead of collapsing to an ambient node-ish fallback such as the reported repeated "November Oscar Delta".

## Operator-takeaway

This regression was not that the daemon forgot sender metadata — it was already exporting it. The real bug was that the effect wrappers were still preferring older ambient fallback logic, so one shared shell block could turn many agents into the same bogus spoken prefix. This fix moves the wrappers onto the sender-specific path and removes the misleading NATO fallback behavior.