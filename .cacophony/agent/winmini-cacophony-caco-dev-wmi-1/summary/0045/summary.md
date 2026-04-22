# Session summary — caco changelog show --since validation + empty-result exit (bd-eb84c8)

## Goal

`caco changelog show --since` silently accepted bogus
versions (returning all releases) and exited 1 on
valid-but-empty future-version queries.

## Bead(s)

- `bd-eb84c8` — caco changelog show --since silent
  acceptance + non-zero empty exit (P3 bug,
  test-user-hel filed)

## Before state

- `caco changelog show --since not-a-version` →
  526 releases (no error, full list).
- `caco changelog show --since 1.999.0` → empty stdout,
  exit code 1. Scripts filtering by future versions saw
  exit-1 and assumed daemon failure.

## After state

- Client-side validation rejects --since values that
  don't look like MAJOR.MINOR[.PATCH[.X]]. Accepts
  optional 'v' prefix. Hard error message lists the
  expected shape with examples.
- Empty-result branch in human-text rendering sets
  `empty_success_override = true`, so even if the daemon
  envelope returned ok:false ("no matching releases"),
  exit code is 0. JSON path unchanged (envelope ok bit
  remains authoritative).

## Diff summary

- Files touched (+33 / −10):
  - `crates/caco-cli/src/lib.rs`: dispatch_changelog_show
    --since validation block + empty-success exit fix.

## Verification

- `cargo build -p caco-cli`: clean.
- Behavioral test deferred (changelog endpoint requires
  full daemon round-trip; the new code paths are
  trivial split-and-check + bool flag).

## Operator-takeaway

`caco changelog show --since BAD` now fails loudly. Empty
queries against future versions now exit 0, making the
flag reliable for shell scripting (e.g.
`caco changelog show --since "$LAST_RELEASED" || exit $?`
no longer false-fails on an unreleased target). Defends
against silent-acceptance papercut family (bd-b76723).
