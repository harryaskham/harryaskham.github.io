# Session summary — bd-090890 Slice 1: the 0600 secure-write primitive for credential spawn-env forwarding

## Goal

Advance and begin implementing bd-090890 (secure-forward a `required_spawn_env` credential to a stale persistent tmux server on revival, without exposing the secret value in argv). First I took it from decision-blocked to decision-ready by analyzing the three candidate mechanisms and recommending one; after ms-mac-ctrl approved that recommendation (per Harry's routine-security-decision delegation), I implemented the foundational, tested security primitive as Slice 1 and decomposed the remaining wiring into clear follow-up slices — deliberately not rushing a multi-part credential change at once.

## Bead(s)

- `bd-090890` — bd-a5a29d follow-up: secure-forward credential spawn-env to a stale persistent tmux server on revival (non-argv mechanism). Parent: `bd-a5a29d` (landed the explicit revival warning).
- (Also this session: advanced bd-090890 to decision-ready + filed reflect-session draft `bd-e78091` for `caco msg send --body-file`.)

## Before state

- Failing tests: none.
- bd-090890 was a decision-blocked P3: it needed an operator/ctrl decision on the secret-forwarding mechanism (the only in-process way to push a secret into a live tmux server's global env is `tmux set-environment -g <NAME> <VALUE>`, which exposes the value in argv — which bd-a5a29d deliberately avoided).
- No secure-storage primitive existed for the approved approach.

## After state

- Failing tests: none. New unit test `write_agent_spawn_secret_0600_locks_secret_at_mode_0600_bd_090890` (caco-daemon is test-small-excluded, so I validate this lib test via the queue: tj-fb4fd85c).
- bd-090890 decision-cleared: ms-mac-ctrl approved Option 2 (0600-file secure inject) + all 4 sub-recommendations (lifetime/location/value-only handling), recorded on the bead.
- Slice 1 implemented: `write_agent_spawn_secret_0600(agent_dir, name, value)` in `crates/caco-daemon/src/agent/health.rs` — writes the secret value to `<agent_dir>/.spawn-secret-<name>` and locks it to 0600 *before* the bytes are written (re-asserts 0600 even over a pre-existing wider-mode file, so there is no readable-by-others window on rotation). Mirrors the in-tree `release_play.rs` `run_sops_decrypt` 0600 + `_FILE` precedent.
- Slices 2 (daemon-side forward + wiring of `required_spawn_env` through the spawn path) and 3 (agent `env.sh` read-and-export for value-only consumers) are decomposed and recorded on the bead.

## Diff summary

- Code commit(s): pending final squash SHA from the reintegration receipt.
- Files touched: `crates/caco-daemon/src/agent/health.rs` (new `write_agent_spawn_secret_0600` helper), `crates/caco-daemon/src/agent/tests.rs` (new unit test).
- Tests: +1 (the 0600-mode/round-trip/re-lock test).
- Behavioural delta: none in production — the helper is not yet wired into the spawn path (Slice 2). Zero behavior change; pure additive foundation. Security correctness (0600 before write, re-lock over wider mode) is tested.

## Operator-takeaway

The security-critical core of bd-090890 (the approved 0600 secure-write primitive) is implemented and tested, and the mechanism deliberately reuses an existing in-tree pattern (release_play.rs's 0600 NamedTempFile + `_FILE` env) rather than inventing one — which de-risks it. The remaining work (threading `required_spawn_env` through the spawn/revival path, and the env.sh read-and-export for value-only secrets) is cleanly decomposed on the bead, so it can proceed incrementally without rushing a credential-handling change. bd-090890 stays in progress (Slices 2/3 remain).
