# Session summary — add `caco web status`

## Goal

Add a first-party read-only status surface for the managed `caco-web`
dashboard service itself. The key requirement was to avoid conflating that
with `caco service status`, which inspects the native lifecycle supervisor,
not the dashboard process.

## Bead(s)

- `bd-591e18` — `Add status subcommand to caco web service`

## Before state

- `caco web` existed only as a launcher.
- No `caco web status` subcommand existed.
- Operators could inspect the lifecycle supervisor via `caco service status`
  and general node health via `caco status`, but there was no dedicated
  read-side CLI for the dashboard service itself.
- `caco-web` already exposed a local `/health` endpoint in the web server,
  and the lifecycle layer already treated it as a pid-only managed service.

## After state

- `caco web status` is now a first-party CLI surface.
- It reports, in both text and JSON:
  - current node
  - whether `caco-web` is configured on that node
  - whether the tracked PID is alive
  - configured bind/port when available
  - configured daemon proxy URL when available
  - the local health probe URL
  - whether `/health` responded successfully
  - returned health payload / health error when applicable
- Probe logic is config-aware:
  - `0.0.0.0` is normalized to `127.0.0.1`
  - `[::]` / `::` is normalized to `[::1]`
  - raw IPv6 binds are bracket-normalized for URL construction
- The surface is explicitly scoped to the dashboard service itself, not the
  lifecycle supervisor.
- Documentation updated in:
  - `README.md`
  - `AGENTS.md`
  - `SPEC.md`

- Live sanity check on the current node succeeded:
  - `cargo run -q -p caco -- web status --json`
  - current node reported `configured: false`, which is accurate for this
    node and still proves the surface works cleanly in the unconfigured case
- Smoke validation passed:
  - `cargo build -p caco-cli`
  - `cargo test-small`

## Diff summary

- Files touched:
  - `crates/caco-cli/src/lib.rs`
  - `README.md`
  - `AGENTS.md`
  - `SPEC.md`
- Tests added / updated:
  - registration test for `caco web status`
  - health URL normalization test for wildcard / IPv6 binds
- Behavioural delta:
  - operators now have a dedicated service-local status surface for the web
    dashboard, separate from supervisor status and general node status

## Operator-takeaway

This is a small but important command-surface cleanup: `caco service status`
and `caco web status` now have distinct meanings. One is for the lifecycle
supervisor; the other is for the dashboard service. That makes service triage
less guessy and gives the web service a first-party inspection path consistent
with the rest of the system.
