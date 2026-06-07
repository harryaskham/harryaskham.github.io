# Session summary — Tailnet local API keeps loopback

## Goal

Fix the `local_bind_host: tailnet` semantics so exposing the bearer-token daemon API to Tailnet clients no longer breaks same-node loopback consumers such as sidecars, TTS, and local feed clients, while keeping the cluster mTLS listener separate and unchanged.

## Bead(s)

- `bd-83391f` — Make daemon `local_bind_host: tailnet` preserve localhost clients

## Before state

- The landed `local_bind_host: tailnet` support resolved the local API bind host to the node Tailnet/public host.
- That made the local API reachable from Tailnet clients, but moved the single daemon listener away from `127.0.0.1:11100` and broke local loopback consumers.
- The immediate live workaround was `local_bind_host: 0.0.0.0`, which preserves loopback but is broader than the semantic name `tailnet` suggests.

## After state

- Static and dynamic daemon local bind resolution treat `local_bind_host: tailnet` as an inclusive local API bind (`0.0.0.0`) so loopback and Tailnet-addressed clients can both reach the bearer-token API.
- Same-node CLI/local clients resolve their daemon base URL back to `127.0.0.1` when the bind is wildcard, so client commands do not switch away from loopback.
- Cluster mTLS bind/public address resolution remains on the existing `bind_host`, `cluster_port`, and `public_cluster_port` path.
- SPEC and config schema text now describe that `tailnet` preserves loopback while exposing the local API on Tailnet-reachable interfaces.

## Diff summary

- Code/content commits: `2085d4ffe0` (local agent commit; final landed squash SHA will come from the reintegration receipt)
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched: `SPEC.md`, `crates/caco-cli/src/lib.rs`, `crates/caco-config/src/model.rs`, `crates/caco-config/tests/config.rs`
- Tests: updated/added focused coverage around daemon tailnet bind aliasing and config listener fixtures.
- Behavioural delta: `local_bind_host: tailnet` now preserves local loopback clients while enabling Tailnet-local API access; cluster mTLS remains unchanged.
- Validation: `cargo test -p caco-cli daemon_tailnet_bind_alias_resolves_for_local_and_cluster_bd_2d3c6a -- --test-threads=2`; `cargo test -p caco-config node_daemon_listener -- --test-threads=2`; `cargo test -p caco-config effective_service_listener_prefers_node_scoped -- --test-threads=2`; `git diff --check`.

## Operator-takeaway

The fix intentionally chooses an inclusive bind for the `tailnet` local API mode and keeps clients loopback-first, which matches Harry's expectation that Tailnet exposure should not break daemon-local consumers.
