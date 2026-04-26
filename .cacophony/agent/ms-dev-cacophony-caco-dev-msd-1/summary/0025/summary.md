# Session summary — Pi wrapped runtime spawn fix

## Goal

Unblock the persistent transcription-agent launch by fixing the agent-spawn runtime verification path that left Pi agents in `starting` or caused `caco agent new` to time out after 30 seconds.

## Bead(s)

- `bd-4d9cc4` — [agent-spawn] Persistent transcription runtime launch times out / peer forwards fail
- Blocked follow-up: `bd-3d4cdf` — [persistent-loops] Kick off a persistent transcription-improvement agent

## Before state

- Failing tests: none directly; the failure was operational.
- Relevant metrics: `caco agent new --profile caco-transcription` returned `request timed out after 30s`; local diagnostic launches left Pi agents in `starting` with ready sentinels written but dead/no attachable tmux by the time the CLI checked. Manual reproduction showed Pi launches run through the Nix wrapper process `.pi-wrapped`, while daemon runtime verification expected the foreground command to normalize to `pi`.
- Context: the `caco-transcription` profile and persistent declaration had already landed, but `bd-3d4cdf` could not honestly close until a transcription runtime could start.

## After state

- Failing tests: none observed.
- Relevant metrics: `cargo test -p caco-daemon normalize_runtime_command_pi_wrapped -- --nocapture` passed; `cargo check -p caco-daemon --tests` passed; `cargo test-small` passed; `git diff --check` passed.
- Context: daemon runtime-command normalization now treats both `pi` and the Nix wrapper `.pi-wrapped` as the canonical `pi` runtime, so post-create launch verification should not wait until the generic request timeout for otherwise healthy Pi sessions.

## Diff summary

- Commits: `ecf1e14e4`.
- Files touched: `crates/caco-daemon/src/agent/health.rs`, `crates/caco-daemon/src/agent/tests.rs`, `.cacophony/agent/ms-dev-cacophony-caco-dev-msd-1/summary/0025/summary.md`.
- Tests: added targeted coverage for `pi`, `.pi-wrapped`, and `/nix/store/.../.pi-wrapped` normalization.
- Behavioural delta: Pi agents launched via the Nix wrapper are recognized as Pi during daemon post-create runtime verification.

## Operator-takeaway

The blocker was not the transcription profile itself; it was daemon launch verification failing to recognize the wrapped Pi runtime. After this lands and the daemon picks up the new binary, retrying the caco-transcription launch should be the next step before closing `bd-3d4cdf`.
