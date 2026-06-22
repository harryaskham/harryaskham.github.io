# Session summary — bd-6a578e Android terminal SSH transport decision

## Goal

Define the Android terminal SSH-key consumption decision required before `bd-5a2535`: clarify that current Android live terminals do not use selected SSH identity keys and should remain daemon PTY WebSocket + bearer auth until a separate direct embedded SSH transport is explicitly approved.

## Bead(s)

- `bd-6a578e` — Android: decide/define embedded-SSH terminal transport (prereq for SSH-key terminal auth, bd-5a2535)

## Before state

- `SshIdentitySettings.kt` said key wiring into an embedded SSH/terminal transport was a follow-up, which could be read as implying the current terminal path would soon consume the selected keys.
- Settings copy said keys were preferred “for terminal connections,” which was misleading because the current terminal transport is daemon-owned `/api/v1/agents/<id>/pty` over bearer-token WebSocket.
- There is no Android embedded SSH client dependency in `app/build.gradle.kts`.

## After state

- `SshIdentitySettings.kt` now documents the `bd-6a578e` decision:
  - current Android agent terminals do **not** consume selected SSH keys;
  - live terminal path is daemon-owned structured PTY WebSocket authenticated by daemon bearer token;
  - cross-node forwarding is daemon/cluster-mTLS owned;
  - there is no embedded SSH client today;
  - selected keys are future-facing unless a direct app-to-node SSH transport is separately approved and split.
- Android Settings SSH copy now says keys are for a **future direct-SSH terminal transport** and explicitly notes current live agent terminals use daemon PTY WebSocket with bearer auth.
- Added `AndroidTerminalSshTransportDecisionSourceTest` to pin the decision, UI copy, current WebSocket URL shape, and absence of speculative SSH client dependencies.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched:
  - `companion/android/app/src/main/java/com/cacophony/companion/connection/SshIdentitySettings.kt`
  - `companion/android/app/src/main/java/com/cacophony/companion/ui/settings/SettingsScreen.kt`
  - `companion/android/app/src/test/java/com/cacophony/companion/AndroidTerminalSshTransportDecisionSourceTest.kt`
- Validation:
  - `gradle :app:testDebugUnitTest --tests com.cacophony.companion.AndroidTerminalSshTransportDecisionSourceTest :app:assembleRelease` — BUILD SUCCESSFUL.
  - `git diff --check` — passed.
- Behavioural delta: copy/contract only; no terminal transport code, SSH library, or connection behavior changed.

## Operator-takeaway

The Android terminal auth story is now explicit: current terminal sessions use daemon bearer-auth PTY WebSockets, not selected SSH identities. The SSH-key picker stays as future setup data, and any real direct embedded SSH transport should be a separate, deliberate implementation bead.
