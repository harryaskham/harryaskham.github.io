# Session summary — bd-04d343: deterministic pico-pane scenario (fix the steer-timing flake)

## Goal

Fix the real test-reliability defect that had been blocking all caco-web Pico
live validation: the `pico-pane` main scenario's outbound-command assertion
flaked under load (steer-while-streaming raced AgentEnd; late commands like
compact raced pi_exited). Make the mock session backend deterministic.

## Bead(s)

- `bd-04d343` — caco-web-observe pico-pane main scenario: flaky steer/late-command outbound assertion under load

## Before state

- The mock fired AgentEnd and pi_exited on fixed wall-clock timers. Under load
  the eval pace drifted: steer sometimes ran after AgentEnd (became a prompt),
  and late commands (compact) sometimes ran after pi_exited (lost). Failed on
  nearly every run during a loaded window.

## After state

- Client-driven lifecycle ORDERING in `start_mock_pico_server_with_frames_and_close_delay`:
  the send loop emits all its own streaming events first (preserving the natural
  AgentStart -> deltas order), THEN waits to have RECEIVED the client's `steer`
  before sending AgentEnd, then waits for `compact` before pi_exited. This
  guarantees steer-while-streaming and prompt-after-streaming and that no late
  command is lost, regardless of eval pace. Gated on the frames actually
  containing AgentEnd/pi_exited (bounded 30s waits), so every other mock is
  unaffected. `PICO_ASSERT_EVAL` now polls for `session ended` (client-driven
  pi_exited has a round-trip after compact).
- caco-web-observe bin 12; clippy clean; caco-web `--lib` 649. The full
  pico-pane scenario ran **7/7 clean** consecutively under the same load that
  previously failed nearly every run.

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `crates/caco-web/src/bin/caco-web-observe.rs` — send-loop-driven AgentEnd/
    pi_exited ordering; PICO_ASSERT polls for session ended.
- Tests: behaviour of an existing scenario made deterministic; no new test files.
- Behavioural delta: test-harness only (no product change).

## Embedded artefacts

- `web/deterministic-run.log` — a clean full pico-pane run after the fix.

## Operator-takeaway

The root cause was event ORDERING, not finalize_streaming: an early client-driven
AgentEnd was being overridden by the send loop's own later AgentStart. The fix is
to let the send loop finish its events first, then react to the client's commands
(steer -> AgentEnd, compact -> pi_exited). This unblocks adding deterministic live
pico-pane subscenarios again (the widget-placement / scroll subscenarios that had
to fall back to static guards can now be re-added live).
