# Session summary — bd-75d5a3 Issue 3: caco event log --json envelope wrap

## Goal

Wrap `caco event log --json` output in the standard `{ok, data,
meta}` envelope so scripts can `jq -e .ok` and consume `count`
consistently with the rest of the catalogued surfaces. Eliminates
one of two remaining no-`ok` surfaces (the other being `caco cert
status` from bd-1625db).

## Bead(s)

- `bd-75d5a3` — caco event log sweep (P3 bug, multi-issue). Pins
  Issue 3 (envelope wrap). Issues 1-2 are POSITIVES (cohort
  observations on cross-namespace --since/--limit validator
  symmetry — strongest positive yet). Issue 4 (--until missing) is
  a feature add for parity with msg stats. Issue 5 (--limit -1
  parser ambiguity) is the cross-cutting parser concern logged
  under bd-9c55aa Issue 5 / bd-754fde Issue 6 / bd-9d3623 Issue 6.
  Issues 6-7 are mild.

## Before state

```
$ caco event log --since 1m --limit 1 --json
{"count": 0, "events": []}
```

No `ok`, no `meta`, no `data` wrapper. Same shape pattern as
`caco cert status` (bd-1625db). 22nd distinct envelope variant in
the bd-5ae1ce catalogue.

## After state

```
$ caco event log --since 1m --limit 1 --json
{
  "ok": true,
  "data": {
    "events": []
  },
  "meta": {
    "count": 0
  }
}
```

Matches `caco bd list --json`, `caco summary --json` (bd-bbcc36),
`caco project list --json` (bd-925e1b), `caco cron list --json`
(bd-2cedd3), and the rest of the standard envelope surfaces.

## Diff summary

- 1 file changed, +24 / -1 (`crates/caco-cli/src/lib.rs`
  `dispatch_event_log` JSON branch).

## Validation

- `cargo check -p caco-cli`: clean.

## Operator-takeaway

**BREAKING for scripts** reading top-level `.count` / `.events`
from `caco event log --json`:
- `.count`  → `.meta.count`
- `.events` → `.data.events`
- `.ok` is now present (always `true` for successful invocations).

Envelope-conformance count: 22 distinct shapes → 21 (minus this
fix). One remaining bare-shape surface: `caco cert status`
(bd-1625db).
