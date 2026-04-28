# Session summary — config-change restarts opt in

## Goal

Stop the Home Manager lifecycle supervisor from automatically restarting Cacophony whenever `config.yaml` changes on disk, because repeated config-change restarts contributed to today's Helsinki/ms-mac restart loops and service flapping.

## Bead(s)

- `bd-c7f663` — `[home-manager] disable automatic caco restart on config.yaml changes`

## Before state

- Failing tests: none specifically for this behavior; deploy-asset tests expected config changes to trigger `caco restart` directly.
- Relevant metrics: live log-monitor evidence showed repeated restart windows attributed to config-change / Home Manager supervisor provenance, with daemon/TTS/web/beads interruptions.
- Context: `SPEC.md` still required the supervisor loop to trigger local `caco restart` on config changes, which made routine config writes too dangerous for authority nodes.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: `docs/validate-pages.sh` passed `1861/0/0`; queued `cargo test -p caco --test deploy_assets root_flake -- --nocapture` passed as `tj-f2c3f819`; queued `nix eval .#homeManagerModules.default --apply 'x: builtins.typeOf x'` passed as `bj-66b25522`.
- Context: the supervisor now logs config drift and records the new hash without restarting by default. Automatic config-change restart remains available only through the explicit `services.cacophony.restartOnConfigChange = true` opt-in.

## Diff summary

- Commits: `a4f3f83b0`
- Files touched: `flake.nix`, `crates/caco/tests/deploy_assets.rs`, `SPEC.md`, `README.md`, `docs/nix.html`, `AGENTS.md`
- Tests: updated deploy-asset assertions for the opt-in config-change restart contract.
- Behavioural delta: routine config file changes no longer flap the daemon through the Home Manager supervisor unless the operator deliberately enables the opt-in.

## Operator-takeaway

The restart loop class is reduced at the source: Home Manager-managed nodes still self-heal real local service/launcher failures, but config writes now require an explicit operator `caco restart` before they can restart the daemon by default.
