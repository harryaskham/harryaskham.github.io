# Session summary — bd-63203d: outbox list payload projection

## Goal

Reduce `/api/v1/outbox` list response size by omitting heavy JSON payloads by
default while preserving an explicit opt-in path for tooling that genuinely
needs the full payload body.

## Bead(s)

- `bd-63203d` — Apply bd-eb1b56 projection pattern to `/api/v1/outbox`

## Before state

- `GET /api/v1/outbox` returned full `OutboxEntry` objects, including the full
  JSON `payload` for every entry.
- Outbox payloads can be multi-KB serialized mutation bodies, so list calls
  were heavier than necessary.
- `caco outbox list` had no CLI flag for opting back into payload-inclusive
  JSON/API output.

## After state

- `/api/v1/outbox` now returns `OutboxListItem` projections instead of raw
  `OutboxEntry` values.
- Default list responses omit `payload` entirely and surface `payload_len`
  instead.
- `?include_payload=true` restores the full JSON payload for compatibility.
- `caco outbox list` now accepts `--include-payload` to wire the opt-in through
  the CLI surface.
- Added daemon projection tests and a CLI command-spec test for the new flag.

## Diff summary

- Files touched:
  - `crates/caco-daemon/src/lib.rs`
  - `crates/caco-cli/src/lib.rs`
- Tests:
  - `cargo test -p caco-daemon outbox_list_item_ -- --nocapture`
  - `cargo test -p caco-cli outbox_list_accepts_include_payload_flag_bd_63203d -- --nocapture`
- Behavioural delta:
  - Default outbox list JSON becomes cheaper and less noisy.
  - Retry/replay tooling can still recover the full payload explicitly.
  - Text-mode `caco outbox list` output stays unchanged.

## Operator-takeaway

This is the same projection pattern already used elsewhere in the repo, now
applied to one of the highest-payload list endpoints. Operators still have an
escape hatch for full payload inspection, but the default path is lighter and
better suited to routine listing.
