# Session summary — bd-6b8ccb: concurrent `caco link add` corrupts links.jsonl

## Goal
Fix concurrent `caco link add` corrupting the link index `links.jsonl` (interleaved appends produce
lines with multiple JSON objects concatenated + missing newline separators), which broke the entire
`caco link list` / `caco link search` read with a whole-index parse failure.

## Bead(s)
- bd-6b8ccb (P2 bug, concurrency/data-integrity, file-cache/link). caco-android oracle profile is a
  discovered-via-agent misattribution; it's caco-cli file_cmd work.

## Before state
- `append_jsonl_record` used `writeln!(file, "{row}")` over an O_APPEND file — `writeln!` emits the
  row and the `\n` as SEPARATE writes, so a concurrent appender could insert its record between
  them → "row-A row-B \n \n" (records concatenated, missing newline separators).
- Both readers (`read_link_index`, `read_link_index_file`) propagated the first line's parse error
  with `?` → ONE corrupted line failed the WHOLE list/search ("parse link index … trailing
  characters").

## After state
- **Atomic append**: `append_jsonl_record` now builds `row + "\n"` and writes it with a SINGLE
  `write_all` — one atomic O_APPEND write for a small (< PIPE_BUF) record, so concurrent writers
  can no longer interleave. Fixes all callers (the file-cache index + the link index).
- **Reader resilience**: a shared `parse_link_index_lines(raw) -> (records, skipped)` skips + counts
  malformed lines instead of failing the whole read (mirrors the feed reader's tolerance); both
  readers use it + emit a bounded `eprintln!` warning when lines were skipped, so `caco link
  list/search` works again even over an already-corrupted index (the immediate operational need).
- 2 unit tests: the tolerant parser skips a concatenated line + returns the valid records; the
  atomic append writes exactly one `\n`-terminated line per record.

## Diff summary
- crates/caco-cli/src/file_cmd.rs only: append_jsonl_record (single write_all), parse_link_index_lines
  helper, read_link_index + read_link_index_file (use the helper + warn), + 2 tests.
(Final landed squash SHA: see the reintegration receipt.)

## Validation
- `cargo test -p caco-cli --lib bd_6b8ccb`: 2/2 pass (clean compile — write_all in scope).
- `cargo clippy -p caco-cli --lib --tests`: my change is clean (the one workspace warning,
  lib.rs:130912 `agent_nudge_client_timeout…` assert-on-constants, is PRE-EXISTING from bd-2ba5ed —
  separate clippy-debt, flagged separately, not in this diff).
- Land discipline (post-escape): rebase fresh → `cargo check --workspace --tests` self-validate on
  the rebased content → land only if green. Contained non-match change → --skip-hooks eligible.

## Operator takeaway
`caco link add` is now concurrency-safe (atomic single-line append) and `caco link list/search`
tolerates pre-existing corruption (skips malformed lines + warns) instead of failing wholesale. A
follow-up could add a `caco link doctor` repair command (split concatenated records back onto their
own lines) analogous to `caco doctor-feed`, and the live links.jsonl may still want a one-time repair
(the read path now tolerates it meanwhile).
