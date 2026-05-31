# Session summary — Process-group kill for output_bounded timeouts (bd-5dfb6e)

## Goal

Close a second instance of the bd-e4e93c grandchild-leak class, found by the
bd-e9164a follow-up audit: the generic `output_bounded` bounded-command helper
killed only the direct child on timeout, so network `git fetch` grandchildren
(ssh/git-remote) could be orphaned.

## Bead(s)

- `bd-5dfb6e` — [health] output_bounded timeout kills only direct child: run_git network fetches can orphan ssh grandchildren (bd-e4e93c sibling)
- (audit origin: `bd-e9164a` draft; sibling of `bd-e4e93c`)

## Before state

- `BoundedCommandOutput::output_bounded` (agent/health.rs) on timeout did
  `child.kill()` + `child.wait()` — direct child only.
- `run_git` uses output_bounded and is called for network fetches at
  lifecycle.rs:7968/7987/8063/8074 (`git fetch origin <agent_branch>` during
  reintegration/resume), with the short agent-control timeout (default 5s).
  A slow ssh remote could time out and orphan the transport grandchild.

## After state

- `output_bounded` now spawns the child as its own process-group leader
  (`process_group(0)` on Unix) and, on timeout, SIGKILLs the whole group via
  a new `bounded_command_kill_process_group` helper, then reaps the direct
  child. Non-Unix falls back to `child.kill()`.
- Safe for the other ~50 callers: tmux clients keep server-owned processes in a
  different group; pgrep/ps/du and local git rev-parse/status/diff have no
  grandchildren.
- Mirrors the bd-e4e93c state-branch-warmup fix.

## Diff summary

- Code/content commit: pending final squash SHA from reintegration receipt.
- Files touched: `crates/caco-daemon/src/agent/health.rs`.
- Tests: none added — output_bounded reads its timeout from a process-global
  env var, so a deterministic fast-timeout test would require env mutation
  (parallel-test contamination risk); validated via `cargo check`, `clippy`,
  and the 42 existing `agent::health` tests (all green). Same rationale as
  bd-e4e93c.
- Behavioural delta: timed-out bounded commands (notably reintegration/resume
  network git fetches) no longer strand ssh transport grandchildren.

## Operator-takeaway

The bd-e4e93c process-group fix had a sibling: the generic agent-control
`output_bounded` helper had the same single-child-kill gap, and `run_git` uses
it for network fetches in the reintegration/resume path. Both daemon-side
timeout-kill git paths now group-kill on timeout. The bd-e9164a draft's
suggestion to centralize one process-group-timeout helper remains the broader
follow-up.
