# bd-8fdb05 progress summary

Implemented the startup-readiness split for the ms-mac daemon warming regression:

- Limited `daemon_startup_warming` aggregate-route backpressure to the pre-bind local readiness window. Once the loopback API is serving, `/api/v1/ui/snapshot` and other aggregate routes are no longer held hostage by background full-state sync, beads sync fan-out, checkout initialization, or repair tasks.
- Expanded `/api/v1/node` startup task telemetry with phase records for `config_load`, `local_api_bind`, `checkout_initialization`, `full_state_sync`, `beads_sync`, `auxiliary_services`, and existing agent/profile/repair tasks.
- Added per-task `duration_secs` for completed startup phases so operators can see where startup time was spent.
- Marked background beads sync and web/TTS service supervision as startup progress phases without making them local readiness gates.
- Updated `SPEC.md`, `README.md`, and `AGENTS.md` to clarify daemon-local readiness versus background mesh/sync convergence.

Runtime evidence folded into the audit:

- Original controller evidence showed daemon process bind in roughly 4–8s, but `/api/v1/ui/snapshot` stayed in `daemon_startup_warming`/`full_state_sync` for minutes, with slow `POST /api/v1/state/full` and `POST /api/v1/projects/*/beads/sync` calls.
- Follow-up controller self-check reported the initial jq wrapper failed because local `jq` aborted on dylib/code-signature, and a paired `caco ops check` timed out at 420s.
- Retrying without jq, `caco msg inbox` took 8.995s and `caco ops check --project cacophony --json` completed but took 146.107s.
- Nearby log-monitor sweeps saw `daemon reachable=false` / HTTP 502s around 22:50Z, while current ops still reported board routing authoritative/sync fresh and ms-mac stable on 1.2.666.
- The implemented fix targets the UI/CLI bootstrap-specific symptom: once local bind is complete, slow mesh/full-state/beads/auxiliary convergence remains visible as background progress/degradation instead of keeping local aggregate startup surfaces in `daemon_startup_warming` for minutes. The long `ops check` duration is still useful follow-up evidence for broader ops/readiness path cost, but it should not re-block local snapshot bootstrap under startup-warming backpressure.

Validation:

- `git diff --check`
- `caco build run --project cacophony --cwd "$PWD" --wait --command "cargo check -p caco-daemon --lib"` → `bj-76cff5fe` succeeded after rebase.
- `caco test run --project cacophony --cwd "$PWD" --wait --command "cargo test -p caco-daemon startup_backpressure_only_covers_pre_bind_readiness_bd_8fdb05 -- --test-threads=2"` → `tj-81469e35` passed after rebase.

Earlier retryable infrastructure noise:

- `tj-a1116964` and `tj-c658fa69` ended as retryable `daemon_restart_recovered` while the daemon was being restarted; not test failures.
- One remote build queue invocation failed because it omitted `--cwd`, so it ran from `/Users/harryaskham` instead of this checkout.
