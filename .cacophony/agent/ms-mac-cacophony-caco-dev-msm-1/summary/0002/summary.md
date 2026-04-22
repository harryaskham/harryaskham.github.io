# Session summary — reject --lines 0 across log surfaces

## Goal

Close the sibling miss bd-551b43 noted by the test-user pass: `caco service
logs --lines 0` silently returned `-- No entries --` with exit 0 instead of
erroring like its peers. Also extend the sweep proactively to other
`--lines` sites that share the same shape (`log tail`, `log stream`,
`tts daemon logs`).

## Bead(s)

- `bd-551b43` — caco service logs --lines 0 silently returns '-- No
  entries --' (exit 0); sibling miss of bd-a08f85 sweep

## Before state

- `caco service logs --lines 0` → `-- No entries --`, exit 0
- `caco log tail --lines 0` → empty tail, exit 0
- `caco log stream --lines 0` → empty stream, exit 0
- `caco tts daemon logs --lines 0` → empty, exit 0
- `caco event log --limit 0` correctly errored (bd-a08f85)

## After state

All four sites now produce:

```
error: --lines must be >= 1 (use --lines 1 for a single result, or omit --lines for the default)
```

with exit code 1 — the same wording bd-a08f85 standardised on for
`--limit 0` across `caco event log` / `log exceptions` / `msg inbox` /
`bd list`.

`cargo test-small` 57/57 PASS, `cargo clippy -p caco-cli --lib --tests`
clean. Two new unit tests:
`validate_positive_limit_rejects_zero_for_lines`,
`validate_positive_limit_accepts_positive_lines`.

## Diff summary

- Commit: 45e6fcdc
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: +2 / -0 / flipped 0
- Behavioural delta: four CLI subcommands now error on `--lines 0`
  instead of silently returning empty.

## Operator-takeaway

The `--lines`/`--limit`/`--tail`/`--count` `0` family is a recurring
class of footguns because each subsystem ships its own validator
wrapper. The minimal fix swaps `validate_non_negative_int` for
`validate_positive_limit` at four dispatch sites. A more durable fix
would be a centralised "list-pagination flag" specifier that all
`*-list / log / inbox / tail` commands declare via, but that's a wider
refactor — file as a follow-up bead if a third sibling-miss surfaces.

Out of scope but visible during the work: `caco service logs --lines 1`
shows systemd-coredump entries for git crashes on the test-user host;
that's a host-side journalctl content question, not a caco bug, and
caco-doctor already owns daemon-crash detection.
