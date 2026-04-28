# Session summary — Supervisor restart trigger classification and startup warning aggregation

## Goal

Resolve `bd-fcb229` by making Home Manager supervisor restart decisions name the concrete trigger instead of logging the ambiguous `critical service or launcher drift` reason, and keep the related `bd-bfdb1f` warning-diagnostics recurrence in scope by reducing startup reconciliation warning spam to an aggregate diagnostic.

## Bead(s)

- `bd-fcb229` — `[lifecycle] supervisor restart reason ambiguous when only TTS is non-critical stopped`
- `bd-bfdb1f` — `daemon-crash.log receives steady warning-only startup reconciliation diagnostics`

## Before state

- Failing tests: none caused by this branch; `nix flake check --no-build` failed on an existing invalid dev-shell drvPath issue, and full queued `nix flake check` hit the queue runtime limit after 30 minutes.
- Relevant metrics: Helsinki log-monitor observed healthy `caco 1.2.582` with repeated same-version restarts whose log reason was `unhealthy critical service or launcher drift` while only `caco-tts-daemon=Stopped [non-critical]` was visible nearby. Later sweeps reported no new restart but `bd-bfdb1f` warning-only startup reconciliation diagnostics remained high and flat at roughly 64-66 daemon.log lines, not new daemon-crash.log bytes.
- Context: The supervisor shell loop already intended to ignore non-critical services for restart decisions, but the reason string hid whether launcher drift or critical services were actually responsible.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: Supervisor restart reasons now render `launcher_drift=true detail=...`, `critical_services=[...]`, or both. Non-critical failures remain in `unhealthy services:` context but do not become the restart reason. Startup reconciliation no longer emits one structured warning per failed historical agent; it keeps one aggregate warning with sampled agent/bead/cause detail.
- Context: `CACO_RESTART_REASON` receives the concrete supervisor trigger, so `caco status --json` lifecycle provenance and logs can distinguish update/drift restarts from critical-service self-healing and from non-critical TTS context.

## Diff summary

- Commits: implementation commit `bd-fcb229 bd-bfdb1f: classify supervisor restarts and aggregate startup warnings` plus this summary commit.
- Files touched: `flake.nix`, `crates/caco/tests/deploy_assets.rs`, `crates/caco-daemon/src/lib.rs`, `README.md`, `SPEC.md`, `docs/nix.html`.
- Tests: updated deployment asset tests for concrete restart reasons and updated startup reconciliation tests to assert aggregate-only warning diagnostics.
- Behavioural delta: supervisor self-healing logs are actionable, stopped non-critical TTS is context-only, and healthy restarts with many historical failed agents produce one aggregate warning instead of high-volume per-agent warning events.
- Validation: `cargo fmt --all -- --check`; `docs/validate-pages.sh`; queued `cargo test -p caco --test deploy_assets root_flake -- --nocapture`; queued `cargo test -p caco-daemon startup_reconciliation_reports_per_agent_failures -- --nocapture`; queued `nix eval .#homeManagerModules.default --apply 'x: builtins.typeOf x'`.
- Validation caveats: queued `nix flake check --no-build` failed on an existing invalid dev-shell drvPath issue; queued full `nix flake check` exceeded the 1800s build-job runtime limit.

## Operator-takeaway

The next deployed Home Manager supervisor should make restart cause analysis explicit: launcher drift and critical-service faults are named, non-critical TTS stays context-only, and the noisy startup reconciliation warning class is aggregated instead of flooding daemon.log/feed.
