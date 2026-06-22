# Session summary — agent-to-agent TTS read-aloud runtime toggle (backend + API)

## Goal

During the operator's overnight bead-burn, take an operator-fresh, in-lane bead
(Harry filed it minutes before the directive) and deliver a safe, validated
increment from headless ms-dev. The bead asks for TUI settings controls for TTS
daemon agent-to-agent speech (toggle on/off, speed, persisted, survive restart).
This chunk lands the contract-safe runtime-toggle BACKEND + API; the TUI
settings row is a documented follow-on.

## Bead(s)

- `bd-bb079c` — Implement TTS TUI settings for daemon controls with
  agent-to-agent speech configuration (feature, P2). Left OPEN: backend+API
  landed; TUI speech-popup settings row remains.

## Before state

- Failing tests: none. `speech.tts.agent_dms.read_aloud` was config-only,
  loaded once at TTS-daemon startup and consulted at the single suppression hook
  (tts_daemon.rs `message_sent` arm). No runtime toggle, no persistence.

## After state

- Failing tests: none. `cargo test -p caco-cli bb079c` (compiles caco-cli +
  caco-daemon + caco-tui) passes; 2 new unit tests green.
- Agent-to-agent (agent DM) read-aloud is now a persisted, restart-surviving
  runtime override following the established mute/speed pattern, reachable via
  `POST /api/v1/tts/agent-dms` (TTS daemon) and the main-daemon proxy.
- Contract-safe: the override defaults to None and falls back to the config
  default at the speak-loop call site, so existing operators are unaffected
  unless they toggle.

## Diff summary

- Code commit: `feb4bed861` (pre-rebase); final landed squash SHA from the
  reintegration receipt.
- Files: `crates/caco-tui/src/speech.rs` (persisted field, serde-default
  back-compat); `crates/caco-cli/src/tts_daemon.rs` (runtime field +
  restore/persist + endpoint + `effective_agent_dms_read_aloud` helper applied
  at the call site, no suppression-fn change + 2 tests + 4 test-constructor
  updates in lib.rs); `crates/caco-daemon/src/lib.rs` (proxy route).
- Tests: +2 (effective-value resolution; persisted round-trip + back-compat).
- Behavioural delta: the agent-to-agent speech toggle exists end-to-end as a
  persisted runtime control; default behavior unchanged.

## Operator-takeaway

The bead is titled "TUI settings" but is really a cross-crate TTS-daemon
feature; I landed the safe, fully-unit-testable backend+API increment from
headless ms-dev (the actual audio effect can't be verified here) and left a
precise TUI-row follow-on spec on the bead. The overnight restart-storm
(repeated operator `caco update --restart`) plus the heavy caco-daemon compile
fan-out made validation take several retryable infra-killed attempts before a
clean pass — a real friction for landing any caco-cli/caco-tui change.
