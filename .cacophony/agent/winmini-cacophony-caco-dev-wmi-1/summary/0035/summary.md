# Session summary — caco cron list: strip newlines from COMMAND cell (bd-a37dab)

## Goal

`caco cron list` table output was leaking newlines from
multi-line bash scripts in the COMMAND column into adjacent
rows/columns, garbling the human-readable output.

## Bead(s)

- `bd-a37dab` — caco cron list table renderer leaks newlines (P3 bug)

## Before state

- `cmd_str` truncation took 50 chars but kept embedded `\n`,
  causing a multi-line script to render as multiple visual
  rows with content bleeding into the NODES column.

## After state

- New helper `format_cron_command_cell(raw, max_chars)`:
  - Strips `\r`.
  - Replaces `\n` with " ⏎ " (Unicode RETURN SYMBOL).
  - Truncates to `max_chars` chars; appends `…` if over.
  - Returns "-" for None.
- `dispatch_cron_list` calls helper instead of inline truncate.
- 1 unit test covers multi-line, long-single-line, short, None.

## Diff summary

- Files touched (+38 / −7):
  - `crates/caco-cli/src/lib.rs`: helper + call site +
    1 test.

## Verification

- `cargo test -p caco-cli --lib cron_command_cell`: 1 pass.
- `cargo clippy -p caco-cli --lib --tests -- -D warnings`: clean.

## Operator-takeaway

`caco cron list` is now safe to dump in any terminal width.
The `--json` output remains unchanged (full multi-line
command preserved); only the human table is sanitized.
