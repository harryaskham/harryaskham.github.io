# Session summary — daemon-down restart provenance

## Goal

Make first-party status surfaces explain why the daemon is down and who/what requested recent restarts, so operators do not have to reconstruct restart cause from raw systemd journals during authority-node outages.

## Bead(s)

- `bd-3c480d` — P0: expose daemon-down cause and restart provenance in first-party status

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: Helsinki incident evidence showed authority reads failing while `caco status` could report daemon unavailable, sidecar stopped, launcher drift, or maintenance, but did not expose the actor/provenance for restart attempts.
- Context: Harry clarified an intentional `just sync-install-restart` loop was running every fifteen minutes, making it especially important that status distinguish operator refreshes from lifecycle self-healing, manual restarts, deploy helper restarts, crashes, and native supervisor failures.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: `caco restart` now records structured events in a bounded 200-entry lifecycle decision ring and mirrors the active restart event into `.restart-pending`; `caco status --json` exposes recent `lifecycle_decisions` plus `daemon.down_reason` when the daemon is unreachable.
- Context: Home Manager supervisor restarts, config-change restarts, and justfile refresh helpers now tag `CACO_RESTART_PROVENANCE` / `CACO_RESTART_REASON`, so downstream status can show operator-sync, supervisor, or config-change provenance.

## Diff summary

- Commits: source branch commit `ee42c178a` after rebase; reintegration will squash this into a mainline commit.
- Files touched: `crates/caco-cli/src/lib.rs`, `crates/caco/tests/deploy_assets.rs`, `flake.nix`, `justfile`, `SPEC.md`, `README.md`.
- Tests: added a focused caco-cli unit test for lifecycle decision retention/down reason and deploy-asset coverage for justfile/flaked restart provenance markers.
- Behavioural delta: restart paths now write structured provenance; status JSON and text status render recent lifecycle decisions, and status JSON includes a concise daemon down reason when local daemon liveness is false.
- Validation: `cargo fmt --all -- --check`; `cargo test -p caco-cli status_lifecycle_decisions_explain_daemon_down_bd_3c480d --lib`; `cargo test -p caco justfile_restart_helpers_tag_restart_provenance --test deploy_assets`; `cargo test -p caco root_flake_supervisor_restart_gate_blocks_overlap_cooldown_and_circuit --test deploy_assets`; `cargo check -p caco-cli`; `docs/validate-pages.sh`; `git diff --check`.

## Operator-takeaway

Future daemon-down investigations should have first-party breadcrumbs: status can now point at the latest lifecycle decision and provenance instead of making controllers infer cause from journal tails that may contain unrelated TTS retries or startup chatter.
