# Session summary — PicoAgentView richer connecting/empty/failed states (bd-838d98)

## Goal

Bring the Android pico view's connecting/empty/failed states toward macOS/iOS
parity: replace the single plain centered line with a state-appropriate icon or
spinner, a clear title + detail, and a Reconnect affordance for the ended/failed
states. First slice of the pico-push UX parity work (coordinated with md2-0).

## Bead(s)

- `bd-838d98` — Android PicoAgentView: richer connecting/empty/failed states
  (parent pico push; siblings bd-42e726 header chips, bd-4dd127 thinking styling)

## Before state

- Failing tests: none.
- `PicoUnavailableBody` rendered one centered `Text` for all states
  (Connecting/Exited/Failed/Unavailable/Waiting) — no icon, no spinner, no retry.

## After state

- Failing tests: none. New `PicoUnavailablePresentationTest` 6/6 green;
  `:app:testDebugUnitTest` build SUCCESSFUL (compiles PicoAgentView + both call
  sites) on ms-dev-2 in the Android Nix devshell.
- New pure `picoUnavailablePresentation(state) -> (title, detail, showSpinner,
  isError, showReconnect)` mapping (unit-tested), and an enhanced
  `PicoUnavailableBody` that renders a spinner while Connecting/Waiting, an error
  icon for Failed, an info icon otherwise, the title + optional detail, and a
  `Reconnect` button for the ended/failed states.
- `onReconnect` threaded through `PicoAgentView`; wired in `AgentDetailScreen`
  (re-resolve + picoSource.connect) and `PicoStandaloneActivity` (source.connect).

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `ui/pico/PicoAgentView.kt` — pure presentation mapping + enhanced
    `PicoUnavailableBody` + `onReconnect` param.
  - `ui/agents/AgentDetailScreen.kt`, `ui/pico/PicoStandaloneActivity.kt` —
    wire `onReconnect`.
  - test `PicoUnavailablePresentationTest.kt` (new) — 6 state-mapping tests.
- Tests: +6, -0, flipped 0.
- Behavioural delta: pico unavailable states now show an icon/spinner + title +
  detail + Reconnect (failed/ended) instead of one plain line.

## Embedded artefacts

- None. Pure state->presentation mapping unit-tested; visual confirmation of the
  icons/spinner/button needs the emulator (deferred — ms-dev-2 build-storm). The
  Compose rendering uses standard Material 3 components.

## Operator-takeaway

The state->presentation decision is a single pure tested function, so the pico
view's connecting/empty/failed UX is verifiable without a Compose runtime, and
the visual layer just renders it. This is the first of three coordinated
PicoAgentView parity slices (header chips bd-42e726, thinking styling bd-4dd127
next). Reconnect is now reachable from the failed/ended state, which pairs with
the bd-bcc919 ws-robustness + bd-636fd1 snapshot-parse fixes for a recoverable
pico session.
