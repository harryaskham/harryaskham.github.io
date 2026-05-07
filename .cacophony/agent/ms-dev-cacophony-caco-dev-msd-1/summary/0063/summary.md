# Session summary — explicit cleanup before destructive checkout pruning

## Goal

Implement the bd-dcafee outage safety fix so transient bad config renders, especially `projects: []` on ms-mac, cannot cause Cacophony to automatically delete or reclone project checkouts or prune managed-agent evidence. The operator-facing intent was for destructive cleanup to require an explicit first-party manual cleanup path after inspection.

## Bead(s)

- `bd-dcafee` — Require explicit caco gc before destructive project checkout cleanup

## Before state

- Failing tests: none at session start for this bead, but the outage report described ms-mac project checkouts being wiped/rebuilt after a bad config made projects logically empty.
- Relevant metrics: targeted validation had not yet existed for `projects=[]` hot reload, checkout identity-drift fencing, or absent-project agent-prune preservation.
- Context: this agent was explicitly assigned bd-dcafee after standing down from release/runner work; checkout was clean at `origin/main` before edits.

## After state

- Failing tests: targeted bd-dcafee validation passed.
- Relevant metrics: queued `tj-f5e47997` passed daemon checkout-fencing tests (`manual_cleanup`, `projects_empty`, `configured_project_filter`); queued `tj-a4210c98` passed `RUST_MIN_STACK=33554432 cargo test -p caco-cli prune --lib` after fixing a prune-delete/cache race.
- Context: checkout identity mismatches now return unhealthy status preserving the existing checkout; `projects=[]` hot reload preserves the existing checkout manager map; automatic prune/retention filters out agents whose project is absent from current valid config.

## Diff summary

- Commits: current head commit `bd-dcafee: fence checkout cleanup behind explicit gc`.
- Files touched: `crates/caco-daemon/src/checkout.rs`, `crates/caco-daemon/src/lib.rs`, `crates/caco-daemon/src/agent/mod.rs`, `crates/caco-cli/src/audio_cmd.rs`, `crates/caco-cli/src/lib.rs`, `SPEC.md`, `README.md`, `AGENTS.md`.
- Tests: +5 focused daemon/lib tests or retargeted assertions around checkout fencing and absent-project pruning; caco-cli prune regression suite retained and now passes with delete mode suppressing async size-cache writers.
- Behavioural delta: daemon config reconciliation refuses to replace a non-empty project map with `projects=[]`, checkout identity drift is fenced until explicit manual cleanup, automatic scheduled/emergency/completed-checkout retention ignores absent-project agents, and prune preview/run surfaces explain absent-project preservation.

## Operator-takeaway

The outage class is now fenced in code: transient missing/changed project config can make checkouts unhealthy, but it is no longer treated as authorization to destroy project repos or agent evidence. Actual destructive cleanup remains an explicit operator action after inspection.
