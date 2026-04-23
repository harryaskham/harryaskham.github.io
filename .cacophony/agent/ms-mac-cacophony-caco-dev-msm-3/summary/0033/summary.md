# Session summary — bd-551b43: --lines 0 sweep for journalctl-backed log surfaces

## Goal

Reject `--lines 0` at `caco service logs`, `caco log tail`, and
`caco log stream` so the misuse fails loudly instead of silently
returning 'no entries' (exit 0). Sibling miss of bd-a08f85's
`--limit 0` sweep.

## Bead(s)

- `bd-551b43` — caco service logs --lines 0 silently returns no entries

## Before state

- `caco service logs --lines 0` → `-- No entries --` (exit 0)
- `caco log tail --lines 0` → `(no log lines found)` (exit 0)
- `caco log stream --lines 0` → `showing last 0 of N lines:` (exit 0)
- `caco event log --limit 0` already errored cleanly (bd-a08f85), so
  the inconsistency was the bug.

## After state

All three surfaces now print:
  `error: --lines must be >= 1 (use --lines 1 for a single result, or omit --lines for the default)`

`--lines 1` and other valid inputs unchanged. `--lines bogus` still
errors with the existing string-validation message.

## Diff summary

- 1 commit, 1 file (`crates/caco-cli/src/lib.rs`)
- Net: +43 lines (3 dispatcher reject blocks + 2 unit tests + module
  doc)
- `cargo test-small`: 57 pass; 2 new tests in `lines_zero_reject_tests`.
- `cargo clippy -p caco-cli --tests`: clean.

## Operator-takeaway

`--lines 0` no longer looks like a healthy empty result on
journalctl-backed surfaces. Bead notes 1-2 more sites may still
exist; they should follow the same dispatcher-side reject pattern
when discovered.
