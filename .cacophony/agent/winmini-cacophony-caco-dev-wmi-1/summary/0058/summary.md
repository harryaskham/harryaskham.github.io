# Session summary — bd-c061d4: caco log exceptions --since filter

## Goal

Add a `--since <dur>` filter to `caco log exceptions` so
operators can find recent exceptions in a 66k-row table
without dumping 5000 entries and grep-by-timestamp.
Match the parser used by `caco event log --since` /
`caco agent logs --since` (Ns / Nm / Nh / Nd).

## Bead(s)

- `bd-c061d4` — P3 bug, test-user-hel filed.

## Before state

- `caco log exceptions` filters: --family, --project,
  --limit, --id only.
- 66,828 rows on helsinki dominated by sync-500 noise.
- Unknown filter flags silently swallowed (bd-b76723
  family) — `--since 1h --severity warning` accepted +
  ignored, header still showed cluster-wide total.

## After state

- New `--since` accepted: `<N>(s|m|h|d)`, parsed by the
  existing `parse_since_duration` helper.
- Invalid value hard-errors client-side:
  `error: invalid --since value: bogus (expected e.g.
   3h, 30m, 1d)`.
- Query path: when --since is set, over-fetches 10×
  `--limit` from the daemon store (since
  `query_exceptions` doesn't accept a ts cutoff yet),
  post-filters by `ExceptionRecord.ts >= cutoff`,
  truncates to `--limit`.
- Header rewritten when --since is active:
  `exceptions on <node> (<total> total cluster-wide,
   <N> after --since filter, showing <K>):`
  Without --since the legacy `(N total, showing K)`
  format is preserved.
- --severity NOT added: the schema is severity-flat
  (only `fatal: bool`); audit-trail bead bd-c061d4
  description acknowledges this. If/when severity grows
  in the schema, --severity can be plumbed through the
  same shape.

## Diff summary

- 1 file touched, +37 / −7:
  - `crates/caco-cli/src/lib.rs`: LOG_EXCEPTIONS_ARGS
    spec, dispatch arm, dispatch_log_exceptions
    signature + cutoff parse + post-filter + header.

## Verification

- `cargo build --bin caco`: clean.
- `caco log exceptions --since bogus`: clean error.
- `caco log exceptions --since 1h --limit 2 --json`:
  returns recent matches as expected.

## Operator-takeaway

Family with bd-eb84c8 (changelog --since) — same client-side
duration-parse pattern; reusable `parse_since_duration`
helper.  CLI honesty pass continues — unknown filter
flags still get bd-b76723 warning treatment, but at
least the documented surface now actually exists for
the most common ask.
