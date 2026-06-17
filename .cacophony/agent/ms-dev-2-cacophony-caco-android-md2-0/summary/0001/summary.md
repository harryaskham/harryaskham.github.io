# Session summary — Android pico source contract: lastSendFailure + availableCommands

## Goal

While the bd-257184 FFI runtime-connect capture stayed blocked on an ~8h
ms-dev devbox-recovery storm (too volatile to boot/keep an emulator alive),
put the storm-blocked window to productive code use: extend the shared Android
`PicoSessionSource` contract with the two session-source states that
caco-android-msd-1's next macOS-parity UI gaps need — send-failure surfacing
and slash-command autocomplete — so msd-1 builds that UI against one canonical
contract instead of an interim OkHttp divergence. Coordinated live with msd-1
(transport owner = me; UI = them).

## Bead(s)

- `bd-ee47fa` — Android pico: PicoSessionSource exposes lastSendFailure +
  availableCommands (send-failure surfacing + slash-autocomplete enablement)
- Sibling: `bd-d9de4d` (msd-1's system_prompt bubble — same Rust-emits/
  Android-drops pattern)
- Parked (storm-blocked): `bd-257184` (FFI live-streaming runtime-connect capture)

## Before state

- `PicoAgentViewSnapshot` had no `availableCommands` field, though the Rust
  `AgentViewSnapshot` already emits `available_commands` (pi `get_commands`,
  bd-89db14) for exactly this autocomplete use.
- `PicoSessionSource.sendPrompt` returned `Boolean` but persisted no observable
  send-failure state, so UI could not surface a failed send / offer retry.
- Stale Android test: `PicoSessionClientSourceTest` still asserted the
  pre-slice-c `createPicoSessionSource(): OkHttpPicoSessionSource` (the swap to
  `PicoSessionSource` landed in slice c); the Rust merge-queue gate does not run
  Android unit tests, so this assertion was silently failing on main.
- Failing tests: 1 (the stale assertion above), latent / un-gated.

## After state

- `PicoAgentViewSnapshot.availableCommands: List<String>` parsed from
  `available_commands`; delivered on every snapshot so both OkHttp + FFI
  sources carry it for free.
- `PicoSessionSource.lastSendFailure: PicoSendFailure?(text, reason, atMillis)`
  set on a transport send failure, cleared on a successful send and on connect,
  distinct from `state` (a transient send failure does not mark the session
  Failed). Implemented in both `OkHttpPicoSessionSource` and
  `FfiPicoSessionSource` (FFI preserves its UAF-safe `handleLock`; empty/blank
  input is client validation, not a send failure).
- Stale test assertion fixed to `PicoSessionSource`.
- Failing tests: 0. +5 focused unit tests (availableCommands parse + default;
  OkHttp send-without-connection failure + empty-prompt-not-a-failure; FFI
  send-without-handle failure).

## Diff summary

- Code/content commit: `877cdc9c0e` (final landed squash SHA from the
  reintegration receipt).
- Files touched:
  - `companion/android/app/src/main/java/com/cacophony/companion/ui/pico/PicoAgentView.kt`
    (+availableCommands field, fromJson parse, parseStringArray helper)
  - `.../ui/pico/PicoSessionClient.kt` (+PicoSendFailure data class,
    +lastSendFailure on the interface + OkHttp impl)
  - `.../ui/pico/FfiPicoSessionSource.kt` (+lastSendFailure impl, UAF-safe)
  - `.../test/.../PicoSessionClientSourceTest.kt` (stale assertion fix +4 tests)
  - `.../test/.../FfiPicoSessionSourceTest.kt` (+1 test)
- Tests: +5 / -0 / fixed 1 stale assertion
- Behavioural delta: Android-only; no Rust changes. UI behaviour unchanged
  until msd-1 wires the new fields; purely additive source contract.

## Operator-takeaway

The Rust merge-queue gate does NOT run the Android Kotlin unit tests, so an
Android-only test can silently rot on main (this fixed one such stale assertion
left by the slice-c return-type swap). Android-bead validation must stay on the
queued `companion/android/scripts/queued-unit-test.sh` path. Also: the Rust
`AgentViewSnapshot` is the single source of truth — multiple macOS-parity
"gaps" (availableCommands here, system_prompt in msd-1's bd-d9de4d) are just
fields the Rust snapshot already emits but the Android snapshot dropped; check
`view.rs` first before assuming new transport work is needed.
