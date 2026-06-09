# Session summary — caco doctor --local-only hang fix (bd-b45966) + broken-on-main file-cache dedupe

## Goal

Fix `caco doctor --local-only` hanging (rc=124, zero output) during intermittent
cross-node flakiness — exactly the degraded window operators run doctor in. While
validating, hit and fixed a broken-on-main: the daemon file-cache content
endpoint was defined twice (two concurrent implementations both landed), so
caco-daemon would not compile.

## Bead(s)

- `bd-b45966` — `caco doctor --local-only` hangs/times out; bound cross-node
  probes and honor `--local-only` (P3 bug; doctor/robustness/cross-node).
  Implemented + landed.
- Broken-on-main dedupe (no separate bead; urgent fix folded in, broadcast +
  follow-up filed): `GET /api/v1/file-cache/{file_id}/content` was defined twice
  — pocket4's `bd-6eaabe` (direct store read) and my own `bd-95eddd` (shell-out)
  both landed. Removed the bd-95eddd duplicate, kept pocket4's more complete
  implementation.

## Before state

- `caco doctor`'s only cross-node-dependent read is the beads-authority probe:
  the local daemon's `/api/v1/beads/status` proxies to the active beads primary
  when this node is not the primary. It used `try_daemon_get` (10s × 3 retries),
  and `--local-only` did NOT skip it — so on a flaky cross-node link the probe
  (and any stacked retries) blew past `timeout 90 caco doctor`, returning rc=124
  with no output.
- caco-daemon would not compile on origin/main: duplicate `handle_file_cache_content`
  + `FileCacheContentQuery` in `file_cache.rs` (E0428/E0119) and a duplicate
  route registration in `lib.rs` (which would also panic axum at startup).

## After state

- `crates/caco-cli/src/msg_cmd.rs`: new bounded single-attempt
  `try_daemon_get_bounded(_async)(url, token, timeout)` — one request with a
  caller-supplied total timeout (no retries), returning `None`/probe-unavailable
  instead of hanging.
- `crates/caco-cli/src/lib.rs` (`dispatch_doctor`): added
  `doctor_should_probe_beads_authority(daemon_reachable, local_only, is_beads_host)`
  and `DOCTOR_CROSS_NODE_PROBE_TIMEOUT` (5s). The beads-authority probe is now
  (a) skipped under `--local-only` unless this node is itself the primary (then
  `/api/v1/beads/status` answers locally and never proxies cross-node), and (b)
  bounded by the 5s timeout in full mode. When skipped, the "beads primary" row
  reports local-view `info` (never a false `error`, never a hang) with an
  explicit "(cross-node authority probe skipped: --local-only)" detail.
- Broken-on-main dedupe: restored `crates/caco-daemon/src/file_cache.rs` to the
  `bd-6eaabe` version (pocket4's direct store-read content + list + helpers;
  header-injection-safe `sanitize_header_filename`; no subprocess/base64), and
  removed the duplicate route registration in `crates/caco-daemon/src/lib.rs`.
  The `/api/v1/file-cache/{id}/content` route and the bd-95eddd/bd-d812ad
  download contract (incl. `?project=` scoping and 404 on unknown/unavailable)
  are preserved by pocket4's implementation.

## Diff summary

- Code commit: see the reintegration receipt for the landed squash SHA.
- Files touched:
  - `crates/caco-cli/src/msg_cmd.rs`: `try_daemon_get_bounded(_async)`.
  - `crates/caco-cli/src/lib.rs`: `doctor_should_probe_beads_authority`,
    `DOCTOR_CROSS_NODE_PROBE_TIMEOUT`, wired the beads-authority probe + skip
    state/detail, +2 tests.
  - `crates/caco-daemon/src/file_cache.rs`: reverted to bd-6eaabe (removes
    bd-95eddd duplicate).
  - `crates/caco-daemon/src/lib.rs`: removed duplicate content route.
- Tests: +2 (`doctor_should_probe_beads_authority_honors_local_only_bd_b45966`,
  `try_daemon_get_bounded_returns_on_slow_peer_bd_b45966`).
- Behavioural delta: `caco doctor --local-only` returns within a bounded time
  even when cross-node connectivity is flaky, emitting a partial "skipped" row
  instead of hanging; full doctor bounds the same probe at 5s. caco-daemon
  compiles again (single file-cache content endpoint + single route).

## Validation

- `cargo check -p caco-daemon --lib` (queued, PASSED — confirms broken-on-main
  fixed).
- `cargo test -p caco-cli --lib bd_b45966` (queued, PASSED — 2/2; slow-peer
  bounded probe + local-only skip predicate).
- `cargo clippy -p caco-cli --lib` (queued, PASSED; new code warning-clean).
- `git diff --check` clean; msg_cmd.rs + my lib.rs additions rustfmt-clean;
  pre-existing whole-file rustfmt drift in the big lib.rs files left untouched.

## Follow-ups

- File a bead on the merge-queue gate gap that let two concurrent
  implementations of the same endpoint both land on main (the gate validated
  each prospective merge but the textual merge of two non-conflicting additions
  produced duplicate symbols that no gate re-checked).

## Operator-takeaway

`caco doctor --local-only` is now safe to run during cross-node degradation — the
single cross-node-dependent probe is skipped under `--local-only` and bounded
otherwise. The file-cache content endpoint now has one canonical implementation
(pocket4's bd-6eaabe direct read); my bd-95eddd shell-out duplicate was removed
to unbreak main, with the route/contract preserved.
