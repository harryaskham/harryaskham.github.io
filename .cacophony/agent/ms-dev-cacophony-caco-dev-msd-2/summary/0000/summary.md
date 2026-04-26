## Goal

Stop `caco up`/`caco restart` from flapping when a stale `caco web`
process is still holding the dashboard bind port (e.g. 11180). Per the
bead: each managed service should "take over configured service ports
decisively, killing stale old web processes or other stale service
processes for that service before starting the managed instance."

## Bead(s)

- `bd-f23bd6` — Harden caco up/restart to take over stale service ports.

Related context: ms-mac caco-web (msm-2) shared duty-cycle evidence
that the managed dashboard was still serving the old build because
the new spawn could not take over the port.

## Before state

`caco-web` was registered as a `pid_only: true` service whose
`service_addr` pointed at the **daemon** port (parent liveness probe),
so neither converge nor stop ever swept the actual web bind port. A
stale `caco web` process from a prior crash, build, or manual launch
would keep holding port 11180; the next spawn raced into EADDRINUSE
and the supervisor flapped.

`caco-tts-daemon` shares the same shape (pid_only, service_addr =
daemon address) but binds a separate runtime-allocated port that's
already swept via its own port-file machinery. Only caco-web had a
configured fixed external port with no sweep.

## After state

- New `external_port: Option<u16>` field on `ManagedService`.
  Documented as: "Configured external listen port for pid-only services
  whose `service_addr` points at the parent daemon (e.g. caco-web);
  convergence and stop both ensure this port is free before
  spawning / after stopping, so stale processes from prior builds or
  crashed instances cannot block startup with EADDRINUSE."
- Caco-web registration sets `external_port: Some(web.port)`. All
  other ManagedService constructions (daemon, tts variants, beads-host,
  test fixtures) carry `external_port: None`.
- `converge` (deferred-pid-only branch): before spawning, calls
  `ensure_port_free(ext_port)`. On `Ok(Some(stale_pid))` the kill is
  reported via the service's `failure_target` (warnings for
  non-critical, failed for critical) so operators can audit the
  takeover. On `Err` the spawn is **refused** rather than allowed to
  flap — surfaced with a `bd-f23bd6: cannot free … refusing spawn to
  avoid EADDRINUSE flap` reason.
- `stop_services` (paired stop branch): after the PID-file kill,
  pid_only services with `external_port` get the same sweep so
  `caco down`/`caco restart` leaves the port free for the next `caco
  up`. Reuses the same `bd-c638be`/`bd-4db921` reporting pattern.

## Diff summary

- `crates/caco-sidecar/src/lifecycle.rs`:
  - `ManagedService` gained `external_port: Option<u16>` (8 sites
    updated to set `None`, caco-web sets `Some(web.port)`).
  - `converge()` deferred-pid-only branch: pre-spawn `ensure_port_free`
    sweep on the external port, with critical/non-critical-aware
    failure routing.
  - `stop_services` paired stop branch: post-kill external-port sweep
    for pid_only services.
  - 2 new tests:
    - `caco_web_managed_service_carries_external_port` — proves
      `external_port = Some(web.port)` is wired from `NodeWebConfig`.
    - `non_web_services_have_no_external_port` — guards against
      accidentally setting `external_port` on services where it would
      double-sweep their primary listener.
- `cargo test-small` 268 passed; `cargo clippy -p caco-sidecar
  --all-targets -- -D warnings` clean.

## Operator-takeaway

A stale `caco web` from a crash or prior build will no longer block
the next `caco up`. Convergence sweeps the configured web port before
respawning and reports the kill as a warning for non-critical
services. If the port cannot be freed (e.g. permissions), spawn is
refused with an explicit `bd-f23bd6` message rather than silently
EADDRINUSE-flapping. Same path covers `caco restart` since it
composes stop + converge.
