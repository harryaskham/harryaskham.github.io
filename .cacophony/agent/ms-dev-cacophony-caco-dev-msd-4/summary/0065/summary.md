# Session summary — lifecycle restart storm gate

## Goal

Stop Home Manager's first-party foreground lifecycle loop from accidentally flapping an authority node by stacking self-healing `caco restart` attempts while the previous daemon startup is still in progress. This session focused on `bd-6d1de3`, filed after Helsinki's beads-primary daemon repeatedly moved through restart windows during the 1.2.563 deployment.

## Bead(s)

- `bd-6d1de3` — P0: gate lifecycle restarts to prevent beads-primary daemon flapping

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: router/journal evidence showed repeated `caco restart` invocations in the Helsinki user service over the deploy window; several restarts reached API-ready while `full_state_sync` was still progressing, and authority reads intermittently failed.
- Context: the lifecycle loop already ignored non-critical TTS stopped state, but supervisor self-healing restarts were not centrally gated by active restart/startup maintenance, cooldown, or a short-window circuit breaker.

## After state

- Failing tests: none in targeted validation; `nix flake check` did not complete within 1200s but had no observed failure before timeout.
- Relevant metrics: supervisor self-healing restarts now have a 300s cooldown and a 3-attempt / 1800s circuit breaker, and are suppressed while status reports active beads-primary maintenance or startup progress.
- Context: explicit operator `caco restart` and config-driven restarts remain available, but health-loop self-healing restarts now leave the node degraded/inspectable instead of repeatedly restarting the daemon.

## Diff summary

- Commits: source branch commit `b61b5bc81` before final recorded-summary normalization; reintegration will squash this into a mainline commit.
- Files touched: `flake.nix`, `crates/caco/tests/deploy_assets.rs`, `SPEC.md`, `README.md`, `AGENTS.md`, `docs/nix.html`.
- Tests: added/updated deploy-asset regression checks for the supervisor restart gate and escalated restart path.
- Behavioural delta: the Home Manager supervisor loop now routes status-triggered and repair-streak-triggered restarts through `run_supervisor_restart`, logging self-healing provenance and suppressing overlap/cooldown/circuit-breaker cases with explicit messages.
- Validation: `cargo fmt --all -- --check`; `cargo test -p caco root_flake_supervisor_restart_gate_blocks_overlap_cooldown_and_circuit --test deploy_assets`; `cargo test -p caco root_flake_escalated_restart_is_gated --test deploy_assets`; `docs/validate-pages.sh`; `git diff --check`. `nix flake check` was attempted because `flake.nix` changed, but timed out after 1200s during dependency/build activity without an observed failure.

## Operator-takeaway

The fix does not ban deliberate operator refresh loops; it prevents the supervised self-healing loop from adding extra restart pressure on top of an already-active startup/restart window. That should keep future authority-node failures observable long enough to diagnose instead of being obscured by repeated first-party restarts.
