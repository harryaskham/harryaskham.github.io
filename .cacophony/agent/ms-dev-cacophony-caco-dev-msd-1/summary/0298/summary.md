# Session summary — bd-b3ce53 cert re-issue drift detection (fix 3 v2)

## Goal

Implement the recommended fresh-worker slice of the silent cert-partition P1:
give an operator a way to detect, before the mesh degrades, that the PKI
authority re-issued a node's certificate under a new serial that never reached
the node (or that the daemon never reloaded after a pull). This is "fix (3) v2"
from the bead's hand-off: an authority endpoint that exposes a node's
currently-issued cert serial, plus a `caco doctor` drift comparison of the local
serial against the authority's current serial. Diagnostic-only — no change to the
live TLS identity path.

## Bead(s)

- `bd-b3ce53` — Cert re-issue not distributed -> stale node cert -> silent
  cluster mesh partition. This session lands fix (3) v2 only; fixes (1)
  distribution reliability and (2) daemon hot-reload remain open on the bead.

## Before state

- Failing tests: none introduced.
- `caco doctor` (fix 3 v1, msm-2) already surfaced the LOCAL node cert serial,
  but there was no way to compare it against the authority's currently-issued
  serial — the exact stale-serial drift that caused the live ms-mac partition was
  still invisible. msm-1 flagged fix (3) v2 as "blocked on a missing authority
  cert-record endpoint returning the currently-issued serial for a node".
- Authority already stores each node's issued cert at `paths.node_cert(node)`,
  but only `/v1/cert/pull` (full cert + private key) exposed it.

## After state

- Failing tests: none. Added 2 caco-cert unit tests (serde defaulting +
  metadata extraction), both pass.
- New authority endpoint `POST /v1/cert/serial` returns metadata only
  (serial/fingerprint/expiry, NO private key) for a requested node, authed like
  `/v1/cert/pull`.
- `caco doctor` now runs a best-effort drift probe on the local non-authority
  node: it fetches the authority-current serial and emits a `node '<n>' cert
  drift` WARNING (plus `caco cert pull` + restart hint) when the local serial
  differs. The probe uses a short 6s timeout, stays quiet when the authority is
  unreachable / bootstrap surface unconfigured, and never stalls doctor.
- Validation: `cargo check --tests` clean on caco-cert, caco-daemon, caco-cli;
  `cargo clippy` introduced no new lints in the touched code; new caco-cert
  tests pass.

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `crates/caco-cert/src/bootstrap.rs` — new `BootstrapCertMetadataRequest` /
    `BootstrapCertMetadataResponse` types + 2 unit tests.
  - `crates/caco-daemon/src/bootstrap.rs` — new `handle_bootstrap_cert_metadata`
    handler + `/v1/cert/serial` route + import.
  - `crates/caco-cli/src/lib.rs` — `send_bootstrap_authority_request_with_timeout`
    (timeout-parameterized variant; old fn delegates), `CERT_DRIFT_CHECK_TIMEOUT_SECS`
    const, and the doctor drift comparison wiring.
  - `SPEC.md` — normative bullet for the re-issue drift diagnostic under the
    `caco cert` semantics (§7.4).
- Tests: +2 (caco-cert metadata serde + extraction).
- Behavioural delta: `caco doctor` gains a cert re-issue drift warning on
  non-authority nodes; a new metadata-only authority endpoint is exposed. No
  change to existing pull/issue flows or to the live mTLS identity path.

## Operator-takeaway

A silent cert re-issue partition (the June 11 ms-mac incident) was previously
invisible: a stale-but-valid cert read "ok" in doctor. After this slice, a
non-authority node whose local serial no longer matches the authority's current
serial gets an explicit doctor drift warning telling the operator to
`caco cert pull` and restart — before the stale cert degrades the mesh. The
remaining bead work is the harder durable fixes: (1) push-on-reissue / auto-pull
distribution reliability, and (2) daemon hot-reload of the node cert/key on
change. bd-b3ce53 stays OPEN for those.
