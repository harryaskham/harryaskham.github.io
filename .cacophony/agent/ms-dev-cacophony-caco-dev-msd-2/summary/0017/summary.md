# Session summary — Pi persistent startup retry after tmux death

## Goal

Fix `bd-a671e1`, where a persistent Pi agent that died during post-create tmux stabilization could leave a non-terminal managed-agent row that blocked clean replacement/recovery and surfaced ambiguous provider-ready handoff diagnostics.

## Bead

- `bd-a671e1` — Fix Pi persistent startup retry after tmux dies during stabilization

## Work performed

- Inspected the beelink technical-writer evidence. The recorded stderr showed the runtime tmux server died during post-create stabilization, e.g. `tmux session died during post-create stabilization probe 4/4`.
- Updated `cleanup_stale_persistent_agent` so persistent managed-agent rows in `Retrying` with a dead tmux session are treated as stale/replaceable, just like the existing `Starting` plus dead-tmux case.
  - Retrying rows with live tmux are still protected as genuine live runtimes, so the launcher does not duplicate viable sessions.
  - Forensic files remain preserved; only the stale live-inventory/index row is discarded before replacement launch.
- Added an agent summary health cause for post-create stabilization tmux exits: `post_create_stabilization_tmux_exit`.
- Updated `SPEC.md` to codify that `Starting` or `Retrying` persistent managed-agent rows with already-dead tmux sessions may be replaced in the same persistent identity.

## Validation

Passed:

- `cargo test -p caco-daemon cleanup_stale_persistent_agent_allows_retrying_dead_tmux -- --test-threads=1`
- `cargo test -p caco-daemon cleanup_stale_starting_persistent_agent_succeeds -- --test-threads=1`
- `cargo test -p caco-daemon agents_summary_classifies_post_create_stabilization_tmux_exit_bd_a671e1 -- --test-threads=1`
- `cargo check -p caco-daemon --lib`
- `git diff --check`

## Outcome

The persistent launcher can now recover from the observed dead-tmux retry shape without injecting text before provider readiness and without treating an already-dead retrying row as a live duplicate forever. Operators also get a clearer summary classification for post-create stabilization tmux exits.
