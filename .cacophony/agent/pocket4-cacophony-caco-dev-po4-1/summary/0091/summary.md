# Session summary — stop transport-retrying non-idempotent create POSTs

## Goal

A broken-on-main caco-cli test
(`bd_create_transport_failure_after_persistence_reports_indeterminate_create_bd_ae014b`)
was failing on clean origin/main: its mock daemon expects exactly ONE create
POST (dropped to simulate a post-server transport loss) followed by a recovery
GET so the CLI can report `indeterminate_create`, but the mock received a SECOND
POST. Root cause: the blocking daemon-read retry helper re-sent the
non-idempotent create POST at the transport layer. Beyond the test break this is
a latent duplicate-bead-creation risk. This session scoped transport retries to
idempotent verbs only, fixing the test and removing the duplicate-create hazard.

## Bead(s)

- `bd-856ffd` — [broken-on-main] caco-cli indeterminate-create test fails:
  create POST retried at transport layer (P2 bug). Discovered/handed off during
  bd-36d913 validation; confirmed pre-existing on clean origin/main.

## Before state

- Failing test (clean origin/main):
  `cargo test -p caco-cli --lib bd_create_transport_failure_after_persistence_reports_indeterminate_create_bd_ae014b`
  panicked: `unexpected recovery request: POST .../beads` (2nd POST instead of
  the recovery GET).
- `send_blocking_request_with_daemon_read_retries` (added bd-b6f7ef for the GET
  `caco agent nudge` path) looped `DAEMON_READ_RETRIES` for ANY verb, re-POSTing
  dropped create requests.

## After state

- Failing test now passes. +1 new idempotency unit test passes. 27 transport
  tests + 3 indeterminate tests green; clippy clean.
- New `http_method_is_idempotent` (GET/HEAD/OPTIONS/TRACE) gates a per-request
  `max_attempts`: idempotent verbs retry up to `DAEMON_READ_RETRIES`;
  POST/PUT/DELETE/PATCH and uncloneable/unbuildable requests get a single
  attempt. Mirrors the existing `async_send_request` msg-send single-attempt
  pattern (bd-78224c). bd-b6f7ef nudge read-retry posture preserved (helper
  still uses DAEMON_READ_RETRIES, backoff, sleep, try_clone — source guard test
  still passes).
- Validation (queued on shared host):
  - `cargo test -p caco-cli --lib bd_create_transport_failure...bd_ae014b` — passed (was failing).
  - `cargo test -p caco-cli --lib blocking_read_retries_only_for_idempotent_methods_bd_856ffd` — passed.
  - `cargo test -p caco-cli --lib agent_nudge_uses_blocking_daemon_read_retries_bd_b6f7ef` — passed.
  - `cargo test -p caco-cli --lib transport` — 27 passed; `--lib indeterminate` — 3 passed.
  - `cargo clippy -p caco-cli -- -D warnings` — exit 0.

## Diff summary

- Code commit: `6044896a8` (final landed squash SHA will come from the
  reintegration receipt).
- Files touched: `crates/caco-cli/src/lib.rs` (single file, +81/-2).
- Tests: +1 (idempotency classifier + source guard). Previously-broken test
  flipped to passing.
- Behavioural delta: non-idempotent daemon mutations (notably `caco bd create`)
  are no longer transport-retried by the blocking read-retry helper, so a
  dropped-after-server create is left to the `indeterminate_create` recovery
  probe instead of being silently re-issued.

## Operator-takeaway

`caco bd create` (and other mutating POSTs through the blocking path) can no
longer be silently re-sent on a transport hiccup, closing a real
duplicate-bead-creation hole and unblocking the caco-cli `--lib` test lane. Read
paths like `caco agent nudge` keep their restart-window retry resilience. This
was the broken-on-main test bd-36d913's validation surfaced; it is now green.
