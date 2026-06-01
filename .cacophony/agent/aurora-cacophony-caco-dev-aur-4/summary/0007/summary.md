# Session summary — bd-a16cf6: connect_timeout hardening pass for daemon/CLI network clients

## Goal

Close the systemic unbounded-connect hang class that the bd-0282ac peer-probe
wedge was one instance of. Several reqwest clients across the daemon and CLI set
only a whole-request `.timeout(...)` and never a `.connect_timeout(...)`, so a
stalled TCP connect / TLS handshake under host load is not promptly cut and can
hang far longer than the request budget implies — affecting beads proxying, LLM
calls, model discovery, and CLI status probes, not just peer probing.

## Bead(s)

- `bd-a16cf6` — Systemic: reqwest clients lack connect_timeout + network
  fan-outs lack per-future bounds (sibling class to bd-0282ac). P3 bug,
  daemon/hardening/networking/replication.
- Filed follow-up `bd-b6abb8` for the remaining recommendations (outer
  per-future `tokio::time::timeout` on fan-outs; a shared cross-crate
  `build_bounded_client` helper) — lower urgency now the connect bound is set.

## Before state

- Only `auto_restart.rs` and `caco-tui/src/client.rs` set `connect_timeout`. The
  cluster mTLS client (`build_cluster_client`, root of bd-0282ac), beads local
  proxy, both LLM clients, model_discovery, the daemon lib.rs probe, and the CLI
  bead-count probe all set only `.timeout(...)` (or, historically, nothing).
- Failing tests: none relevant.

## After state

- Failing tests: none. New `tls::tests::network_connect_timeout_is_small_and_nonzero_bd_a16cf6`
  passes; `beads::tests` 138/0 green; clippy `-p caco-daemon -p caco-cli --lib`
  clean.
- Added `pub(crate) const NETWORK_CONNECT_TIMEOUT: Duration = 5s` in
  caco-daemon/src/tls.rs and applied `.connect_timeout(...)` to: build_cluster_client
  (tls.rs), the beads local proxy client (beads.rs), both LLM clients (llm.rs),
  model_discovery (model_discovery.rs), and the daemon lib.rs probe client. The
  caco-cli `query_local_bead_count_async` probe (used inside `join_all`) gained a
  matching 3s `connect_timeout` alongside its existing 3s request timeout.

## Diff summary

- Code commit: pending final squash SHA from the reintegration receipt.
- Files touched: crates/caco-daemon/src/{tls.rs, beads.rs, llm.rs,
  model_discovery.rs, lib.rs}, crates/caco-cli/src/lib.rs (+40/-0 net).
- Tests: +1 (connect-timeout invariant).
- Behavioural delta: every cited network client now fails a stalled connect in
  ~5s (3s for the CLI probe) independently of the request timeout. No change for
  healthy connects. The request timeouts are unchanged; this is a strictly
  additive outer connect bound.

## Embedded artefacts

- none.

## Operator-takeaway

The acute symptom (bd-0282ac fleet-liveness freeze) was the loud instance of a
quiet systemic gap: reqwest's request timeout does NOT bound a stalled TCP
connect / TLS handshake, and most daemon/CLI clients never set
`connect_timeout`, so the same under-load hang could silently stall beads, LLM,
model-discovery, and status calls. This pass sets a short connect bound on all of
them via a single daemon-side const. The remaining defense-in-depth work
(wrapping the few network fan-outs in an outer per-future timeout, and a shared
bounded-client helper so the inconsistency can't recur) is split to bd-b6abb8 —
deliberately deferred because the clients are now bounded, so it's consistency
polish rather than an open hang.
