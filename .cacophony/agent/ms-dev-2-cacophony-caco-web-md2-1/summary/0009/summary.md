# Session summary — caco-web secure default bind (0.0.0.0 → 127.0.0.1)

## Goal

Close the unauthenticated-exposure class for the caco-web dashboard by making its
**default bind loopback (127.0.0.1)** instead of all-interfaces (0.0.0.0). caco-web
is a token-injecting daemon proxy with no auth on its read+write endpoints, so an
all-interfaces default exposed the full daemon read+write API on the LAN/tailnet.
This is the focused, independent "loopback-default" slice of the security epic
(bd-764efa) that can land before the heavier auth work.

## Bead(s)

- `bd-ddace5` — caco-web: change default bind 0.0.0.0 → 127.0.0.1 (secure loopback default) + fix pinned test
- parent: `bd-e5f22f` (default bind off 0.0.0.0 + auth) — **kept OPEN**; the auth-on-endpoints piece remains its scope
- epic: `bd-764efa` (SECURITY AUDIT+FIX: services bind 0.0.0.0; owned by msm-3)

## Before state

- Failing tests: none.
- caco-web's standalone server defaulted its bind to `0.0.0.0`:
  - `caco web` CLI `--bind` fallback (caco-cli ~31580): `None => "0.0.0.0"` — the
    ACTUAL default the supervised/standalone service uses (it builds WebConfig
    directly from the flag, not WebConfig::default).
  - `WebConfig::default()` (caco-web/src/server.rs:216): `bind_addr: "0.0.0.0"`.
  - `--bind` help text said "default: 0.0.0.0"; `tests.rs:1302` pinned "0.0.0.0".
- Operator had already taken caco-web DOWN on all nodes (acute mitigation), so
  nothing was live on 0.0.0.0 at change time.

## After state

- Failing tests: none. Validated on ms-dev-2 (calm; NOT ms-mac which was
  re-saturated): queued `cargo test -p caco-web --lib` (exit 0, updated
  `web_config_defaults` passes) and queued `cargo check -p caco-cli --lib`
  (exit 0, compiles).
- caco-web default bind is now `127.0.0.1` in both real paths (CLI fallback +
  WebConfig::default), the help text, and the pinned test. The bind stays fully
  configurable via `--bind` / `services.caco-web.bind`, so this flip breaks
  nothing now and just makes the secure default secure.

## Diff summary

- Code commit: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `crates/caco-cli/src/lib.rs` — `caco web` `--bind` fallback `0.0.0.0` →
    `127.0.0.1` (the exposure-closing default) + a security-rationale comment;
    WEB_ARGS `--bind` help text default updated.
  - `crates/caco-web/src/server.rs` — `WebConfig::default` bind_addr `0.0.0.0` →
    `127.0.0.1`; doc comment updated to describe the secure loopback default and
    the Funnel/config remote-access path.
  - `crates/caco-web/src/tests.rs` — pinned default assertion → `127.0.0.1`.
- Tests: 0 added / 0 removed; existing default test updated and green.
- Behavioural delta: launching caco-web without an explicit bind now listens on
  loopback only, not all interfaces.

## Embedded artefacts

None (pure default-value + doc/test change; no UI surface).

## Operator-takeaway

The code default is now the *secure* default (loopback) regardless of operator
access needs — per msm-3's reframing, remote/non-tailnet device access is a
separate operational choice that returns via the **authenticated Tailscale Funnel
(bd-d1eeff)** or an explicit `--bind` / `services.caco-web.bind` override, NOT an
all-interfaces bind. The real fix required changing the `caco web` CLI `--bind`
fallback (caco-cli), not just `WebConfig::default()` — the serve command builds
WebConfig directly from the flag, so server.rs alone would have been cosmetic.
`bd-e5f22f` stays OPEN for the remaining real-auth-on-read/write-endpoints work.
