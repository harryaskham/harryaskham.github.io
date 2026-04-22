# Session summary — bd-e1eab8: caco msg send --require-ack

## Goal

Slice 2 follow-up of the msg state-machine: ship
`caco msg send --require-ack[=Ns]` which blocks until the target's
inbox row reports a non-null `delivered_at`, failing with a
structured timeout error after Ns (default 5s).

## Bead(s)

- `bd-e1eab8` — `[bd-91a14c follow-up] caco msg send --require-ack`

## Before state

- `caco msg send` returned as soon as the daemon accepted the request
  for queueing — no signal that the target had actually materialised
  the row. Operator coordination required out-of-band polling of
  `caco msg status` or wishful thinking.
- `bd-91a14c` shipped `delivered_at` distinct from `read_at` on the
  inbox row but no CLI surface consumed it.

## After state

- `caco msg send --require-ack` (no value) waits up to 5s.
- `--require-ack=5`, `--require-ack=5s`, `--require-ack=250ms`
  parse to explicit timeouts; zero and garbage rejected.
- After the send POST returns the message id, the CLI polls
  `GET /api/v1/messages/{id}/status` every 100ms until the row
  reports a non-null `delivered_at`, or the timeout expires.
- Polling is tolerant: 404s before the row materialises, network
  blips, and JSON parse errors sleep+retry until timeout. Only
  persistent failure surfaces.
- Outcome surfacing:
  - text + delivered: `(acked: delivered_at=..., waited=Nms)` appended
  - text + timeout: `CliError` so exit code is non-zero
  - JSON: `data.ack = { delivered_at, waited_ms, timed_out }` so
    callers can discriminate without parsing text. JSON envelope
    keeps `ok: true` because the send itself succeeded; callers
    inspect `ack.timed_out`.

## Diff summary

- Commit: `90a20654`
- File: `crates/caco-cli/src/lib.rs` (+199 / -6)
- New helpers: `parse_require_ack_timeout`, `poll_msg_delivered`,
  `MsgAckOutcome`. Validation extracted so the parser is unit-tested
  without spinning up a daemon.
- Tests: +4 (empty default, bare-integer seconds, Ns/Nms suffixes,
  zero/garbage rejections).
- `dispatch_msg_send` grew to 8 args; `#[allow(clippy::too_many_arguments)]`
  added with comment matching the sibling-dispatcher convention.
- `cargo build -p caco-cli` + `cargo clippy -p caco-cli --no-deps`
  clean. Lib test suite green (895 + 4 = 899 tests).

## Operator-takeaway

Cross-agent coordination flows that previously had to chain
`caco msg send` → `sleep` → `caco msg status` can now do the wait
in one call: `caco msg send --target X --body Y --require-ack=10s`.
JSON callers get structured `ack` data without text-scraping.
Broadcast/speak `--require-ack` and `--cc operator` forwarding remain
as `bd-6c5939` / `bd-64f914` follow-ups.
