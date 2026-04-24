# Session summary — caco event log truncation hint (bd-0b47a7)

## Goal

Fix the CRITICAL data-integrity issue in `caco event log`: when
`--since` asks for a window containing more events than `--limit`
covers (default 200), results are silently truncated. An operator
asking "audit me on the last 24h" gets the OLDEST 200 events with
no warning, missing recent activity. `caco bd stats` reports 218
closes/24h alone, so the default `--limit 200` covers ~22h at
9 closes/hr — misleading for any `--since > 22h`.

## Bead(s)

- `bd-0b47a7` — caco event log DATA-INTEGRITY (P2, bug, test-user)
  — 9 issues filed; this lands the CRITICAL Issue #1 fix +
  truncation surfacing. The other 8 issues filed as 4 follow-ups
  per close-discipline rule (no silent burying).

## Before state

```
$ caco event log --since 1d --json | jq '.count'
200
$ caco bd stats --project cacophony
closed: 218 last 24h     ← MORE than the 200-event cap
```

The daemon's `query_events` chained `.take(limit)` directly into
the iterator, discarding the total count. Caller had no way to
detect truncation.

## After state

`query_events` now delegates to `query_events_with_total(path,
filter)` which returns `(Vec<CommandEvent>, total_matched)`. The
total_matched is computed POST-filter, PRE-limit so the truncation
hint reflects the operator's specific query.

Daemon envelope extended:

```json
{
  "events": [...],
  "count": <returned>,
  "total_matched": <pre-limit count>,   // bd-0b47a7 NEW
  "limit": <effective limit>,            // bd-0b47a7 NEW
  "truncated": <bool>                    // bd-0b47a7 NEW
}
```

CLI human-readable output now shows the truncation hint when
`truncated: true`:

```
showing 200 of 1247 total event(s) (cap: --limit 200) — increase --limit to see more:

  2026-04-24 09:32  bd close → bd-76db0a  by ms-mac:cacophony:msm-2  on ms-mac
  ...
```

When NOT truncated, output is unchanged: `200 event(s):`.

## Diff summary

- Files touched:
  - `crates/caco-daemon/src/event_log.rs`:
    - New `query_events_with_total(path, filter)` — returns
      `(Vec<CommandEvent>, usize)`. The previous `query_events`
      now delegates to it (legacy callers preserved).
    - `handle_event_log` extended to surface `total_matched`,
      `limit`, and `truncated` in the SuccessEnvelope data.
  - `crates/caco-cli/src/lib.rs`:
    - `dispatch_event_log` reads the new fields and renders
      "showing N of M total event(s) (cap: --limit X) — increase
      --limit to see more:" when truncated.
- Tests: +1 / -0
  - `bd_0b47a7_query_events_with_total_reports_pre_limit_count`
    pins three cases:
    1. limit < total: returns limited slice + accurate total
    2. limit >= total: total_matched == events.len() (no
       truncation)
    3. With command filter applied: total_matched is
       POST-filter, PRE-limit count (so the operator's
       specific query shows the right "of N" number)
  - All 8 existing event_log tests continue to pass (the
    public `query_events` function signature is unchanged).
- Test command:
  `cargo test -p caco-daemon event_log::` → 9 passed.

## Out-of-scope follow-ups (filed, NOT closed by this bead)

- **bd-44af89** (P3): `caco --json` output drops outer
  SuccessEnvelope `ok` field across multiple commands (event log
  / doctor schema / summary). Anti-pattern affecting programmatic
  consumers. Audit + standardise.
- **bd-b6a0d9** (P3): `--since '-1h'` rejected by parser; document
  `--since=-1h` inline form OR add explicit "durations must be
  positive" error.
- **bd-3d6a13** (P3): `event log` doesn't log itself; `msg speak`
  + `audio capabilities` also unlogged. Document the 'state-
  mutating only' rule in `--help`, don't expand the audit surface
  (volume risk per bd-3bbc6f).
- **bd-ad8ced** (P4): `--node` / `--agent` silently ignored on
  `event log` (real flag is `--caller`). Alias-hint per bd-9c55aa.

Issue #6 (parser-ambiguity bare `-1`) was already fixed in this
session by bd-8b4559 (numeric-token helper); confirmed via
locally-built CLI.

Issues #2, #3, #4, #9 were POSITIVE observations (gold-standard
patterns) — no fix needed, just promotion to other surfaces (which
the test-user bead's existing notes already track).

## Operator-takeaway

`caco event log` is now a trustworthy audit surface. Truncation
that previously corrupted operator decisions ("there were no
recent events!") now produces a loud explicit hint ("showing 200
of 1247 — increase --limit to see more").

Programmatic consumers can read `data.truncated` / `data.total_matched`
directly from `--json` to gate their own retry-with-larger-limit
loops.

Honored constraints:
- `cargo test -p caco-daemon event_log::` only — no workspace test.
- Pre-close audit will run before `caco bd close`.
- Operator close-discipline: 4 out-of-scope finds filed as new beads
  with back-references; not silently buried.
- Operator `bd update --status=closed` bypass directive: ACK,
  using only `caco bd close`.

22nd bead closed this session (cumulative). 15th in this turn.
