# Session summary — Android pico select_model + sendCommand bridge

## Goal

Close msd-1's KEY remaining gap: the model-picker UI had no way to send the
user's selection (PicoSessionSource only had connect/disconnect/sendPrompt). Add
a generic command-send path + a select_model convenience, designed to also
unblock pending_dialog's reply next (same send-command bridge).

## Bead(s)

- `bd-837bb5` — Android pico: PicoSessionSource select_model + sendCommand bridge
- Consumes the FFI C-ABI `pico_session_send_command` (already exported; keystone
  bd-756234 unchanged)
- Sibling display-path: aur-2's `bd-dd37de` (per-message send_state, landed
  b4e4496df) — plumb next

## Before state

- PicoSessionSource: connect/disconnect/sendPrompt only. No command path.
- PicoFfi: 7 bindings (no sendCommand). The C-ABI had pico_session_send_command
  but no JNI bridge for it.
- Failing tests: 0.

## After state

- `crates/caco-picophony/src/jni_bridge.rs`: `Java_..._PicoFfi_sendCommand`
  wrapping `pico_session_send_command` (additive, cfg jni feature; C-ABI
  unchanged).
- `PicoFfi.sendCommand(handle, commandJson)` external fun.
- `PicoSessionSource`: abstract `sendCommand(commandJson): Boolean` + default
  `selectModel(index): Boolean` (reads `pendingModelPicker`, splits the
  provider/id label on the FIRST '/' like Rust `select_model`, sends a bare
  `SetModel` RpcCommand).
- OkHttp `sendCommand`: wraps the bare RpcCommand with `kind:command` for the WS;
  FFI `sendCommand`: sends bare via `PicoFfi.sendCommand` under the UAF-safe
  `handleLock`.
- `PicoSessionProtocol.setModelCommandJson` = bare `{type:set_model, provider,
  modelId}` (matches RpcCommand::SetModel serde, modelId rename).
- Failing tests: 0. +6 tests (setModelCommandJson shape, selectModel split/guards
  via a FakePicoSource, OkHttp+FFI sendCommand-without-connection guards).

## Diff summary

- Code commit: db68bada7c (final landed squash SHA from the reintegration
  receipt).
- Files: jni_bridge.rs, PicoFfi.kt, PicoSessionClient.kt, FfiPicoSessionSource.kt,
  PicoSessionClientSourceTest.kt, FfiPicoSessionSourceTest.kt.
- Tests: +6 / -0. Validated: queued Kotlin unit test + a host
  `cargo build -p caco-picophony --features ffi,ws,jni` (jni_bridge compiles;
  the device .so rebuilds on device builds via `just pico-android-ndk`).
- Behavioural delta: adds the model-picker ACT path; no change until msd-1 wires
  selectModel(index) into the picker UI.

## Operator-takeaway

This is the ACT path; aur-2's bd-dd37de send_state is the DISPLAY path — together
they complete the model-picker and the real send-failure+retry. The SAME
`sendCommand` bridge is reused for pending_dialog's ExtensionUiReply (the next
slice), so the interactive-reply gap is solved once. The jni_bridge.rs binding is
behind the `jni` feature, so the default cargo merge-queue gate does NOT compile
it — it must be validated via the `--features jni` build (host or NDK).
