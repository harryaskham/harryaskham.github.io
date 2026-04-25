# Session summary — TTS routing now distinguishes local playback from operator-audible output

## Goal

Address the ms-mac TTS confusion where daemon traces showed `outcome=played`, yet Harry kept repeating that he could not hear the responses. The goal was not to fake hardware audibility from a non-ms-mac node, but to make the first-party CLI explicitly answer the missing operator question: does this route play only on the local machine, or should it be audible on the operator's remote Pulse listener?

## Bead(s)

- `bd-f79294` — ms-mac TTS plays to local-default but may be inaudible to operator listener

## Before state

- Multiple ms-mac-local agents had already verified the Cacophony side of TTS:
  - `caco-tts-daemon` running
  - unmuted
  - output routing `local-default`
  - trace outcomes including `played`
- But `caco tts io output show` only printed raw routing fields (`mode`, optional pulse server, source), leaving operators to infer whether `local-default` meant “Harry should hear this” or merely “the Mac itself is playing it somewhere”.
- `caco tts status` also surfaced output routing, but not the audibility meaning of that route.
- The CLI error text for `caco tts io output set` was stale (`local, remote, disabled`) and did not match the real routing modes.

## After state

- Added a shared CLI interpretation layer for TTS output audibility:
  - `local-default` / `local-device` → `local-node-only`
  - `pulse-default` / named pulse outputs → Pulse-routed / potentially operator-audible
- `caco tts io output show` now reports:
  - `scope:` (`local-node-only`, `pulse-server`, or `named-pulse-output`)
  - `audible:` operator-facing explanation of what `played` means for that route
  - `reachable:` Pulse reachability when a Pulse server is involved
- `caco tts status` now also includes:
  - `scope:`
  - `audible:` explanation
  - Pulse connectivity status when `PULSE_SERVER` / named pulse routing is active
- Updated the stale `caco tts io output set` discoverability hint so operators see the real supported modes:
  - `local-default`
  - `local-device`
  - `pulse-default`
  - named output
- Updated repo docs so operators know that `caco tts io output set ...` changes routing live without restarting the daemon.

## Diff summary

- Files touched:
  - `crates/caco-cli/src/lib.rs`
  - `README.md`
  - `AGENTS.md`
- Validation:
  - `cargo build -p caco-cli`
  - `cargo test -p caco-cli tts_output_audibility -- --nocapture`
  - `cargo test -p caco-cli tts_setting_commands_missing_values_use_discoverability_hints -- --nocapture`
  - attempted live `cargo run -q -p caco -- tts io output show` / `tts status`, but this pocket4 checkout has no local `tts-daemon.port`, which is expected because the active hardware/routing issue is on ms-mac, not here
- Behavioural delta:
  - operators can now distinguish “played on this node's speakers” from “should be audible on the remote listener” using first-party CLI output alone

## Operator-takeaway

The core gap was semantic, not synthesis: `played` did not tell Harry whether the sound was routed somewhere he could actually hear. The CLI now makes that explicit. If ms-mac still reports `scope: local-node-only` while Harry expects remote audibility on his listening sink, the next operational step is to flip output routing with `caco tts io output set --mode pulse-default` or a named pulse output/server on ms-mac — without restarting the daemon.
