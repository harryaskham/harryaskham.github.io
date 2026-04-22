# Session summary — bd-bf1e86 cycle 11: format_bytes consolidation

## Goal

De-duplicate two view-local copies of the bytes->human-readable
helper (`views::prune::format_bytes` and
`views::status::format_bytes`), promote to `common::format_bytes`
as the canonical home, and quietly fix the status-view copy that
rounded sub-KB values to `0K` because it lacked a B tier.

## Bead(s)

- `bd-bf1e86` — Permanent: caco-tui subtle UX polish (cycle 11)

## Before state

- `views/prune.rs::format_bytes` — full B/K/M/G tiers.
- `views/status.rs::format_bytes` — only K/M/G; sub-KB values
  rendered as `0K`.
- No shared canonical helper; future authors had two divergent
  options to copy from.

## After state

- `common::format_bytes(bytes: u64)` is the canonical implementation
  with full B/K/M/G tiers and a doc comment recording the rounding
  contract.
- `views::prune::format_bytes` and `views::status::format_bytes` are
  now one-line delegations preserving call-site signatures.
- status.rs implicitly gains the B tier — sub-KB values now render
  as e.g. `512B` instead of `0K`.
- Locked with `format_bytes_tiers` covering 0B / 512B / 2K / 5M /
  2G; existing prune and status tests stay green.

## Diff summary

- Commits: `4072955f`
- Files: `crates/caco-tui/src/views/common.rs`,
  `crates/caco-tui/src/views/prune.rs`,
  `crates/caco-tui/src/views/status.rs`
- +48 / -23 lines, +1 test.
- Build + clippy clean on caco-tui.

## Operator-takeaway

Byte-formatting is now centralised next to the other staleness /
elapsed / pluralisation helpers in `views::common`. The status
surface no longer rounds small daemon byte readings down to zero.
Future authors needing byte rendering have one canonical home.
