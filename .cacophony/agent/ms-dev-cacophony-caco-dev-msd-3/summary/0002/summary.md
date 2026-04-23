# Session summary — bd-49724c: stop failing restarts that are merely slow

## Goal

Operator complaint (bd-49724c): `caco agent restart` almost always shows
a failure in the TUI/CLI even though the agent itself comes up moments
later and is fine. Operator's prescription: "agent start.timeout =
remove, just show starting... and let logs / attach reveal any slow
start." Make resumes/restarts stop surfacing spurious failures.

## Bead(s)

- `bd-49724c` — restarts always show as failures but agent comes up anyway

## Before state

- `AgentManager::resume_inner()` in `crates/caco-daemon/src/agent/lifecycle.rs`
  used `wait_for_non_shell_runtime_on()` with `runtime_launch_timeout_secs()`
  (2s for claude, 10s for codex/pi) and, on `RuntimeLaunchState::NotConfirmed`,
  hard-failed: killed the tmux session (`kill_tmux_session_on`), set
  `state = Failed` + `resume_blocker = RuntimeLaunchFailed`, and returned
  `DaemonError::Other("resume bootstrap completed, but runtime launch failed:
  tmux session did not settle ...")`.
- TUI consumed that as `ActionResult::AgentRestartFailed` and toasted
  "✗ agent restart failed for ..." even when the user could attach into
  a live, healthy session a second later.
- Failing tests (relevant subset): none — touched code path was not
  asserted by unit tests beyond the generic `ResumeBlocker` enum
  classification (still passing).

## After state

- Same probe still runs and the timeouts are unchanged. But on
  `NotConfirmed` (after the existing `bd-96af85` / `bd-8bfa4d`
  normalisation+interpreter acceptance), the path now branches on
  tmux liveness:
  - Tmux still alive → log a `bd-49724c` warning and let resume
    continue (the agent settles into `Running` like normal). Watchdog
    / heartbeat is the safety net for genuinely-stuck runtimes.
  - Tmux genuinely dead → keep the hard failure, but with
    `ResumeBlocker::TmuxSessionExited` (more accurate than
    `RuntimeLaunchFailed` for that case).
- Create path (initial spawn) is unchanged: a brand-new agent that
  never brings its runtime up is still a real failure surfaced
  immediately.
- `cargo build -p caco-daemon` clean. `cargo clippy -p caco-daemon`
  clean. `cargo test-small` clean. Targeted
  `resume_blocker_*` tests still pass (8/8).

## Diff summary

- Commit: `161d0c1c` on
  `agent/ms-dev/cacophony/ms-dev-cacophony-caco-dev-msd-3`.
- Files touched:
  `crates/caco-daemon/src/agent/lifecycle.rs` (resume runtime-launch
  verification branch, ~+35/-13 lines).
- Tests: no test changes. The generic `ResumeBlocker` /
  `is_retryable` / `Display` tests still pass; no test asserted the
  resume-NotConfirmed-on-alive-tmux failure semantics, so no test
  needed to flip.
- Behavioural delta: persistent + manual restarts of `claude` /
  `codex` / `pi` agents that take longer than the runtime probe
  timeout to settle no longer flap to `Failed` / kill their tmux
  sessions. The TUI / CLI will continue to show "starting…" until
  the agent reports activity, exactly as the operator requested.
  Real failures (tmux died) still surface, with a more accurate
  blocker.

## Embedded artefacts

(none — small textual diff)

## Operator-takeaway

The "always-failed restart" was caused by a fixed 2s/10s probe in
`resume_inner` that killed the tmux session on the first
NotConfirmed observation. The session was virtually always still
alive at that point. Now the probe is advisory: if tmux is alive we
keep the session and let the watchdog do the eventual liveness
arbitration. Net effect: fewer false-failed restarts, no loss of
real-failure detection, no change to create-path semantics.
