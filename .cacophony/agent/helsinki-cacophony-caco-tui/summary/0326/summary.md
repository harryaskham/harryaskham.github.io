# bd-5a3e51 — caco agent resume outcome classification

## Problem
`caco agent resume` could return a misleading apparent success while leaving
the agent stopped and without a tmux target:

```
agent ms-mac-cacophony-log-monitor resumed (method: backgrounded)
  state: stopped
  tmux: unknown
```

The daemon reported any `Ok(ResumeResult)` from the lifecycle layer as a flat
200 success, and the CLI unconditionally printed "resumed", so a
backgrounded/preserved relaunch was indistinguishable from a real recovery.
There was no operation id / pending state / failure reason.

## Change
- `ResumeResult::outcome_class(tmux_alive) -> ResumeOutcomeClass`
  (`crates/caco-daemon/src/agent/types.rs`):
  - `Recovered` — Running with a verified live tmux session, or a non-relaunch
    method (`Unpause` / `AdoptLiveSession`) whose session was already live.
  - `Pending` — relaunch requested but runtime not confirmed live
    (`starting`/`pending`/`retrying`/`recovering`, or `running` bookkeeping
    with no live tmux). Running with an empty/dead session downgrades to
    Pending so we never claim a runtime we cannot see.
  - `Failed` — terminal/non-progressing after the attempt
    (`stopped`/`failed`/etc).
- `handle_agent_resume` (`crates/caco-daemon/src/lib.rs`) probes the resulting
  tmux session once (`verify_tmux_alive_pub_on`) and branches:
  - Recovered -> 200 `resumed:true`
  - Pending -> 202 `resumed:false, pending:true` with message + `observe` hint
    (`caco agent status` / `caco agent logs`)
  - Failed -> 409 `agent_resume_no_runtime` structured error with
    `agent_state`, `resume_method`, `tmux_session`, `tmux_alive`,
    `resume_blocker`, and `observe` hints.
- CLI `dispatch_agent_resume` (`crates/caco-cli/src/lib.rs`):
  - renders pending distinctly ("resume pending" + note + observe targets,
    never "resumed"); empty tmux normalized to "unknown".
  - dedicated human-readable branch for `agent_resume_no_runtime`.
- README agent-resume contract updated.

## Acceptance criteria (met)
- resume no longer presents backgrounded/stopped/tmux-unknown as success.
- truly queued/backgrounded resume returns a structured pending state plus
  where to observe progress/errors.
- cannot-start returns a structured semantic error without requiring
  operators to infer failure from `state: stopped`.
- regression test for a stopped persistent whose tmux/session is absent
  (`resume_outcome_class_stopped_relaunch_is_failed_bd_5a3e51`), plus pending
  and recovered cases.

Related broader status bug bd-ceb7d9 (out of scope; this bead is the narrow
resume command response/operation semantics).

## Validation
Queued on shared host (caco test/build run):
- `cargo check -p caco-daemon --tests` — ok (exit 0)
- `cargo check -p caco-cli --tests` — ok (exit 0)
- `cargo test -p caco-daemon --lib resume_outcome_class` — 3 passed
- `cargo test -p caco-daemon --lib auto_retry_resume` — 4 passed (no regression)
- `cargo clippy -p caco-daemon -p caco-cli --tests` — ok (exit 0), no new
  warnings attributable to this change.

## SPEC areas
Agent lifecycle / resume semantics (SPEC 16.x agent lifecycle). No spec
contradiction; tightens operator-facing resume reporting.

No visual TUI surface, so no screenshots apply.
