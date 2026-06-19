# Session summary — caco doctor flags world-exposed caco-web bind

## Goal

Close the recurrence-guard gap surfaced by bd-7712b6: the doctor world-exposed-
bind check scanned only the daemon listener binds, not `NodeWebConfig.bind`. Now
that the caco-web dashboard bind is established as a real exposure vector (the
supervised launch passes `--bind <NodeWebConfig.bind>`), extend the guard so an
explicit `services.caco-web.bind: 0.0.0.0` can't silently re-open the bd-83db05
exposure undetected.

## Bead(s)

- `bd-a37724` — caco doctor world-exposed-bind guard should also flag explicit
  0.0.0.0 NodeWebConfig.bind (spec by msm-3; consolidates bd-47fed6)
- follow-up to `bd-764efa` slice 1 / `bd-7712b6`

## Before state

- Failing tests: none. `doctor_world_exposed_bind_checks` (caco-cli) flagged
  world-exposed `NodeDaemonListener.local_bind_host` / `.bind_host` but NOT
  `NodeWebConfig.bind`.

## After state

- Failing tests: none. `cargo test -p caco-cli --lib bd_a37724` green (2/2);
  `cargo check --workspace --tests` succeeded.
- The guard now also emits a `network` warning when a node's explicit
  `services.caco-web.bind` is world-exposed (0.0.0.0 / ::), reusing
  `bind_host_is_world_exposed`. Only explicit world binds match (default is
  loopback via `default_web_bind`). Embedded mode is skipped (the daemon owns the
  dashboard port; the standalone web bind is unused). The detail notes caco-web is
  a token-injecting proxy with no request auth yet (more exposed than the
  auth-gated daemon listeners) and points remediation at loopback/tailnet + the
  authenticated Funnel (bd-d1eeff).

## Diff summary

- Code commit: pending final squash SHA from the reintegration receipt.
- Files: `crates/caco-cli/src/lib.rs` — extend `doctor_world_exposed_bind_checks`
  with a per-node caco-web bind check + remediation; +2 tests (predicate +
  function-level integration: exposed node flagged, embedded node not).
- Tests: +2; diagnostic-only, zero blast radius (same shape as the daemon-listener
  checks).
- Behavioural delta: `caco doctor` now surfaces a world-exposed caco-web bind.

## Embedded artefacts

None (a diagnostic check + tests).

## Operator-takeaway

The recurrence guard now covers the caco-web dashboard bind — the exact exposure
class bd-7712b6 closed by default — so a future explicit world bind in config is
caught by `caco doctor` instead of slipping past silently. msm-3 specced this and
offered review; landed on ms-dev-2 (calm) while ms-mac was re-saturated (load 38+)
/ msm-3 in transit, with a post-land feedback invite for the embedded-skip /
wording (trivially adjustable). Diagnostic-only, zero blast radius.
