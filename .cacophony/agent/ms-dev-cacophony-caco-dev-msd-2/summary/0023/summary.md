# Session summary — standalone beads-daemon routing audit

## Goal

Make standalone `caco-bd-daemon` service-object routing safe to reason about after the bd-dcafee outage by proving and surfacing the split between the authority node's loopback local port and peer nodes' mTLS cluster endpoint.

## Bead(s)

- `bd-b59c23` — Standalone beads daemon routing uses wrong endpoint/port from peer nodes

## Before state

- Failing tests: none reproduced locally before editing; the incident evidence was operational, from peers/agents attempting the wrong standalone beads endpoint during ms-mac recovery.
- Relevant metrics: service-object example uses `port: 11101` for the authority-local listener and `cluster_port: 12101` for remote peer routing.
- Context: existing code already selected the standalone cluster port for remote peers, but proxy targets did not carry endpoint mode/active-primary metadata and error messages still said only “failed to reach authoritative daemon,” making port-mismatch incidents hard to diagnose.

## After state

- Failing tests: targeted queued validation passed before rebase (`tj-a2a53931`) and after rebase (`tj-f84f9bfa`).
- Relevant metrics: remote standalone proxy target test asserts `https://100.103.121.27:12101`, no local bearer auth, and no `:11101`; local authority proxy still asserts `http://127.0.0.1:11101` with bearer auth.
- Context: proxy targets now record `LocalStandalone`, `RemoteStandalone`, or `RemoteDaemon` mode plus active primary; all request-failure paths use endpoint-aware messages naming the active primary and resolved endpoint.

## Diff summary

- Commits: the bead-aware code commit on this agent branch plus this summary artefact commit
- Files touched: `crates/caco-daemon/src/beads.rs`, `SPEC.md`
- Tests: targeted daemon tests passed via `tj-a2a53931` and post-rebase `tj-f84f9bfa`; no tests removed.
- Behavioural delta: remote peers continue to route service-object beads-primary traffic to the standalone cluster endpoint, authority-local main daemon continues to use the loopback local endpoint, and failures now identify which endpoint was attempted.

## Operator-takeaway

`bd-b59c23` did not require re-enabling standalone beads mode; it tightened the routing contract and diagnostics so the next standalone-mode rollout can distinguish a local-port outage from a remote cluster-port routing error quickly.
