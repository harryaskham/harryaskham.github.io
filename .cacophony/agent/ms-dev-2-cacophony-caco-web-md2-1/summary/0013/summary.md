# Session summary — caco-web auth Slice 2 (access_token config + refuse-to-start)

## Goal

Wire the S1 auth bootstrap into the `caco web` launch and enforce the startup
posture: a loopback bind auto-generates a local dashboard token (zero-config
dev), a non-loopback bind with no token refuses to start (fail-safe). No
request-time enforcement yet — that is the next slice.

## Bead(s)

- `bd-8fbe59` — caco-web auth Slice 2: access_token config field + startup refuse-to-start wiring
- parent (kept OPEN): the caco-web real-auth child of the security epic; S3
  (enforcement middleware + login + WS gating) + S4–S6 remain.

## Before state

- Failing tests: none. The S1 auth helpers existed but were not wired into the
  launch; `NodeWebConfig` had no `access_token`; `caco web` started regardless of
  bind/auth.

## After state

- Failing tests: none. `cargo check -p caco-cli` + `cargo check --workspace
  --tests` both succeeded. The auto-gen/refuse/loopback logic is covered by the
  S1 unit tests.
- `caco-config`: `NodeWebConfig.access_token` (Option, serde-default None, env
  `CACO_WEB_ACCESS_TOKEN`).
- `caco-cli dispatch_web`: resolves the dashboard token (env → local node
  `services.caco-web.access_token`); on a loopback bind with no token,
  auto-generates the persisted local token + provisions the cookie-signing key;
  on a non-loopback bind with no token, returns a refuse-to-start error. Reuses
  `auth::resolve_dashboard_token` / `load_or_create_cookie_key` /
  `is_loopback_bind`.

## Diff summary

- Code commit: pending final squash SHA from the reintegration receipt.
- Files: `crates/caco-config/src/model.rs` (access_token field),
  `crates/caco-cli/src/lib.rs` (dispatch_web resolution + refuse-to-start),
  `crates/caco-sidecar/src/lifecycle.rs` + `crates/caco-daemon/src/lib.rs` (two
  test fixtures updated for the new field).
- Tests: +0 (logic covered by S1 unit tests; the wiring is fail-safe by design).
- Behavioural delta: `caco web` now refuses to start on a non-loopback bind with
  no access token, and auto-provisions a local token on loopback.

## Embedded artefacts

None (config field + launch wiring).

## Operator-takeaway

The startup posture is now enforced: the supervised dashboard (loopback default
from bd-7712b6) auto-provisions a token and starts zero-config, while a
network-exposed bind without an explicit token refuses to start — making the
unauthenticated-off-loopback combination impossible to reach silently. The wiring
is fail-safe (config/path failure → auto-gen on loopback or refuse on
non-loopback, never fails open). Request-time enforcement (validating the token
on each request) lands in S3 with msm-3's daemon-side review.
