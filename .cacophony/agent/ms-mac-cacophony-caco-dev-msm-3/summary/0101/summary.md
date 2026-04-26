# Session summary — stale worker liveness diagnostics

## Goal

Fix `bd-2efa9c`, where live helsinki workers could remain operator-visible as `stale` with a misleading `no tool activity for >65535s` error even after fresh liveness evidence arrived.

## Bead(s)

- `bd-2efa9c` — Active workers remain marked stale with impossible no-tool-activity age

## Before state

- Failing tests: none known for this bead; unrelated broken-on-main work was announced by other owners during the session.
- Relevant metrics: live statuses for `qsywr0jnhsu62rxz`, `dz2hmhkeee2xjl9t`, and `g6ifgs3zoxe52n6s` showed `state=stale` and `last_error="no tool activity for >65535s — worker marked stale"` despite recent progress/liveness timestamps.
- Context: daemon post-restart liveness refresh could update `last_tool_activity` for a verified-live tmux session without clearing an already-stale state/error.

## After state

- Failing tests: targeted validation passed.
- Relevant metrics: stale diagnostics now report the measured idle seconds and the effective stale threshold, including per-agent thresholds, instead of formatting the global sentinel as the apparent idle age.
- Context: if a post-restart live-tmux refresh sees an agent already in `Stalled`, it now revives it to `Running`, clears the stale error/resume blocker, and lets ordinary stale detection re-fire later only if the worker remains idle.

## Diff summary

- Commits: `384183821`
- Files touched: `crates/caco-daemon/src/agent/lifecycle.rs`, `crates/caco-daemon/src/agent/tests.rs`
- Tests: `cargo test -p caco-daemon reconcile_preserves_live_tmux_agents_after_restart -- --nocapture`; `cargo test -p caco-daemon reconcile_uses_per_agent_stale_timeout -- --nocapture`; `cargo check -p caco-daemon --tests`; `cargo fmt --all -- --check`.
- Behavioural delta: verified-live tmux sessions no longer remain in the contradictory `stale` plus fresh-liveness state after a restart refresh, and stale error text is actionable.

## Operator-takeaway

The stale-worker display should now recover automatically after the daemon proves the worker tmux session is alive, instead of requiring manual interpretation of impossible `>65535s` idle ages.
