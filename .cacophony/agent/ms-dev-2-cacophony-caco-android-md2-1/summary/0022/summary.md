# Session summary — S6 terminal transport-mode resolver (bd-a1a358)

## Goal
The ws-vs-SSH terminal transport selection logic: a pure mode enum + parse +
resolver the terminal uses to pick its transport, with a safe fallback to the
existing websocket transport when SSH is requested but unavailable.

## Bead(s)
- bd-a1a358 S6 (SSH-pool terminal transport; the Settings toggle + Pane wiring
  exposing SSH land with the S5 session — this is the decision logic).

## Before/After state
- Before: no transport-mode model/selection. Failing tests: none.
- After: TerminalTransportMode{WebSocket,Ssh} + TERMINAL_TRANSPORT_MODE_PREF_KEY +
  parseTerminalTransportMode (defaults WebSocket) + resolveTerminalTransportMode
  (SSH only when requested AND sshAvailable, else WebSocket fallback).
  TerminalTransportModeTest 2/2 green. Failing tests: none.

## Diff summary
- Code/content commits: pending reintegration receipt SHA.
- Files: connection/TerminalTransportMode.kt (new), test TerminalTransportModeTest.kt (new).
- Tests: +2, -0. Behavioural delta: none yet (decision logic; toggle/Pane wiring with S5).

## Embedded artefacts
- None. Pure enum + resolver, fully unit-tested.

## Operator-takeaway
The transport-mode selection (always-working-fallback to websocket) is pure +
tested. Remaining SSH terminal work: S5 (wire SshShellHandle -> SshTerminalSession
+ Termux TerminalView I/O bridge, mirror TermuxAgentPtyController — the meatier
emulator-gated slice) + the Settings toggle + Pane transport selection using this
resolver + the optional Android-runtime BC confirm. SSH connect stack + transport
decision logic all landed+validated.
