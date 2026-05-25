# Session summary — TTS mute audit trail

## Goal

Fix `bd-5ce25b` by making TTS runtime mute and unmute changes inspectable through the first-party command audit log. The operator-facing goal was to answer “who muted this?” for both local daemon runtime mute toggles and scoped runtime mute policy changes, without relying on transient TTS traces or source-code inspection.

## Bead(s)

- `bd-5ce25b` — Audit TTS runtime mute and unmute mutations

## Before state

- Failing tests: none known at session start.
- Relevant metrics: `caco event log --since 12h --command mute` had returned zero events during the controller investigation despite TTS mute/unmute being state-mutating.
- Context: unscoped TTS mute already emitted `tts_runtime_control` feed events for mesh convergence, and scoped mute rules already wrote `$CACOPHONY_DIR/tts-daemon/audio-policy.json`, but neither path guaranteed a command-audit row with caller/scope context.

## After state

- Failing tests: none observed in focused validation.
- Relevant metrics: scoped TTS runtime mute now writes a `tts mute` command-audit row with source, scope, key, mute value, and policy path; daemon-proxied runtime controls now write `tts <action>` command-audit rows with caller, target node, local runtime scope, request, and resulting state.
- Context: the existing `tts_runtime_control` feed event remains in place for convergence, now carrying the caller in the payload as additional provenance.

## Diff summary

- Code/content commits: `124a220c5` (`bd-5ce25b: audit TTS mute mutations`)
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched: `crates/caco-daemon/src/lib.rs`, `crates/caco-cli/src/lib.rs`, `SPEC.md`, `README.md`, `.cacophony/agent/winmini-cacophony-caco-dev-wmi-1/summary/pending/summary.md`
- Tests: extended `tts_scoped_mute_writes_runtime_policy_not_import_wrapper_config_bd_6a1e1d` to assert scoped mute command-audit output; validation ran `git diff --check` and `RUST_MIN_STACK=33554432 cargo test -p caco-cli --lib tts_scoped_mute_writes_runtime_policy_not_import_wrapper_config_bd_6a1e1d` successfully.
- Behavioural delta: operators can search `caco event log --command mute` to see unscoped runtime mute/unmute/toggle and scoped runtime mute/unmute changes, including who called the command and which node/scope was affected.

## Operator-takeaway

TTS mute state is no longer a provenance blind spot: the mesh feed still converges runtime controls, but command-audit history now records mute mutations in the same first-party surface operators already use for other state-changing commands.
