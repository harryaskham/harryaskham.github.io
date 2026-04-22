# Session summary — bd-9d2051: bd search/bd stalled --limit 0 closed (sibling of bd-a08f85)

## Goal

Probe sweep after bd-a08f85's 5-surface fix found 2
more silent-swallow surfaces:

- `caco bd search --query X --limit 0` → "no beads found"
- `caco bd stalled --limit 0`           → "no beads found"

Same family, same fix. bd search uses
`bd_cli_error` envelope (structured-JSON surface);
bd stalled uses `validate_positive_limit` directly.

## Bead(s)

- `bd-9d2051` — own follow-up. Closed.

## Before state

```
$ caco bd search --query test --limit 0
no beads found matching query across all statuses: test

$ caco bd stalled --limit 0
no beads found
```

## After state

```
$ caco bd search --query test --limit 0
error: --limit must be >= 1 for bd search (use --limit
1 for a single result, or omit --limit for the default)

$ caco bd stalled --limit 0
error: --limit must be >= 1 (use --limit 1 for a
single result, or omit --limit for the default)

$ caco bd search --query test --limit 1
matches for query across all statuses: test           # unchanged
```

## Diff summary

- 1 file touched, +18 / −0:
  - `crates/caco-cli/src/lib.rs::dispatch_bd_stalled`:
    `validate_positive_limit("--limit", v)?` after
    project resolution, before URL construction.
  - `crates/caco-cli/src/lib.rs::dispatch_bd_search`:
    new `Ok(0)` arm in the existing
    `parse::<usize>()` match returns
    `bd_cli_error("invalid_argument", "--limit must be
    >= 1 for bd search ...")`. Stays inside the
    structured-envelope path so JSON callers get the
    same `code: invalid_argument` they get for the
    parse-failure path.

## Verification

- `cargo build --bin caco`: clean.
- bd search --limit 0 → friendly error; --limit abc
  unchanged (regression check pass); --limit 1 → real
  result.
- bd stalled --limit 0 → friendly error.

## Operator-takeaway

`bd_cli_error` (structured) vs `validate_positive_limit`
(plain CliError) coexistence: when a surface already
has a structured-envelope error path for related
shapes (parse-failure here), keep the new shape in the
envelope rather than mixing two error styles. The
small code duplication ('--limit must be >= 1...')
is preferable to two error envelopes in one surface.

After this round: 7 user-visible read surfaces now
validate --limit at the dispatch boundary
(event log, log exceptions, notify list, msg inbox
[both --limit and --tail], bd list, bd search, bd
stalled). Probe surfaces remaining: agent merge-queue
list (already done bd-8c5bda), agent
audit-reintegration (bd-a29677), agent list /
node list / project list / log error all DON'T accept
--limit (caught by the unrecognised-flag warning;
intentional - they're fixed-shape outputs).

The validate_positive_limit helper has reached
saturation: 7 callsites, consistent shape, ready for
the codebase to consider it a stable primitive.
