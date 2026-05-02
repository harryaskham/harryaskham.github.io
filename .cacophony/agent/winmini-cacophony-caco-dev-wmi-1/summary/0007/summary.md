# Session summary — bd-fb7474 shared TTS sender-prefix source of truth

## Goal

Reduce regression risk in the TTS effect wrappers by factoring the duplicated sender-prefix shell block into one checked-in source of truth, so future fixes to sender naming and intro generation land once and are then synced mechanically into every wrapper.

## Bead(s)

- `bd-fb7474` — Factor shared sender-prefix logic out of duplicated TTS effect wrappers
- adjacent context only: `bd-b48e4d` is the active semantic regression fix owned elsewhere; this bead stayed scoped to deduplicating the shared wrapper block

## Before state

- Failing tests: no existing regression checked that the ten TTS effect wrappers stayed aligned with the same sender-prefix logic.
- Relevant metrics: the same long shell block for `effective_body`, `include_agent_name`, `agent.json` probing, persistent-id stripping, NATO fallback, and intro phrase selection was duplicated across ten files under `.cacophony/tts/effects/*.yaml`. Any future sender-prefix fix had to be hand-applied in every wrapper.
- Context: this duplication became obvious while handing context to the active owner of the separate TTS prefix regression bead. The risk was not only current breakage, but future drift whenever one wrapper got patched and others did not.

## After state

- Failing tests: none in the focused config/script lane.
- Relevant metrics: the shared sender-prefix block now lives in `.cacophony/tts/effects/_shared_sender_prefix.shfrag`, and `scripts/sync-tts-effect-prefix.py` syncs it into the ten effect wrappers with managed BEGIN/END markers. A targeted caco-cli source test now verifies those wrappers remain in sync with the shared fragment.
- Context: runtime behaviour of the wrappers stays the same for now; the improvement is that there is now one authoritative fragment to edit and one sync/check path to keep the wrappers aligned.

## Diff summary

- Commits: `1a99bb8aa`
- Files touched: `.cacophony/tts/effects/_shared_sender_prefix.shfrag`, `.cacophony/tts/effects/alien-transmission.yaml`, `.cacophony/tts/effects/arcade-glitch.yaml`, `.cacophony/tts/effects/cyberdeck.yaml`, `.cacophony/tts/effects/haunted-cathedral.yaml`, `.cacophony/tts/effects/helmet-comms.yaml`, `.cacophony/tts/effects/mission-control.yaml`, `.cacophony/tts/effects/noir-tape.yaml`, `.cacophony/tts/effects/numbers-station.yaml`, `.cacophony/tts/effects/subway-pa.yaml`, `.cacophony/tts/effects/walkie-talkie.yaml`, `scripts/sync-tts-effect-prefix.py`, `crates/caco-cli/src/audio_cmd.rs`
- Tests: `python scripts/sync-tts-effect-prefix.py --check`; YAML command-template `bash -n` pass across 10 effect wrappers; `caco config validate --project-config-dir .cacophony`; `cargo test -p caco-cli tts_sender_prefix_wrappers_are_synced_from_shared_fragment_bd_fb7474 -- --nocapture`
- Behavioural delta: the sender-prefix logic remains the same at runtime, but future edits now have one source-of-truth fragment and an automated sync/check path instead of ten hand-maintained copies.

## Operator-takeaway

This is a maintenance hardening slice: it does not itself change the spoken prefix semantics, but it removes the copy-paste hazard that made the active TTS prefix regression harder to fix safely. Future prefix fixes can now be applied once and propagated consistently.
