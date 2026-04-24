# Session summary — bd-44af89: caco doctor schema --json wraps in {ok, data, meta} envelope

## Goal

Address bd-44af89: `caco --json` surfaces dropping the
outer `SuccessEnvelope { ok }` field, leaving programmatic
consumers (jq scripts, dashboards, downstream automation)
unable to distinguish success from soft-error responses.

The bead listed three surfaces:

- `caco doctor schema --json`   — `{databases, summary}` (no ok)
- `caco summary --json`         — pure flat (no ok)
- `caco event log --json`       — `{count, events}` (no ok)

Investigation revealed that **two of the three were already
fixed** earlier in the validator-cohort burndown:

- `caco event log --json` — wrapped via bd-75d5a3 (Issue 3)
  to emit `{ok, data: {events}, meta: {count}}`.
- `caco summary --json` — wrapped via bd-bbcc36 to emit
  `{ok, data, meta: {since, since_iso}}`.

Only `caco doctor schema --json` was still emitting the bare
`{databases, summary}` shape. Fixed in this bead.

## Bead(s)

- `bd-44af89` — `caco --json output drops outer
  SuccessEnvelope ok field — multiple commands surface no-ok
  flat envelope`.

## Before state

```
$ caco doctor schema --json | jq 'keys'
["databases", "summary"]
```

No `ok` field. Operator scripts that do
`caco doctor schema --json | jq -e .ok` to gate on success
would always fail-closed regardless of actual drift state.

## After state

```
$ caco doctor schema --json | jq 'keys'
["data", "meta", "ok"]
$ caco doctor schema --json | jq '.ok, .meta'
true
{ "total_drift_columns": 0, "missing_tables": 0 }
```

`ok` flips to `false` when `total_drift_columns > 0` or
`missing_tables > 0` — semantic ok/not-ok, not just
"command ran". `meta` surfaces the totals at the top
level so a quick health probe can `jq .meta.total_drift_columns`
without descending into `.data.summary.*`. Full schema
detail remains under `.data` unchanged.

This brings the doctor schema surface in line with the
broader `{ok, data, meta}` cohort that already covers
event log, summary, bd graph, build show, scratch list,
test list, and many others.

## Diff summary

- `crates/caco-cli/src/lib.rs`:
  - `dispatch_doctor_schema`: replaced the bare
    `summary` JSON output with an envelope-wrapped form
    that flips `ok = !has_drift` and surfaces totals in
    `meta`. Original `summary` payload is preserved
    under `data` so existing consumers reading
    `.data.databases.*` / `.data.summary.*` still work
    after their `jq` paths get a one-token prefix update.
  - 1 new test:
    `dispatch_doctor_schema_wraps_json_in_ok_envelope`
    — source-greps the function body for the bd-44af89
    marker, the `"ok": !has_drift` literal, and
    `total_drift_columns` so the envelope wrap can't
    silently regress to the bare shape.
- `cargo test -p caco-cli --lib`: focused test passes;
  297 total pass.
- `cargo test-small`: 297 pass; 1 pre-existing failure
  (caco-profile shipped_profiles_html_lists_every_canonical_profile
  — `stale-check` profile added without docs/profiles.html
  update; flagged in bd-b6a0d9 takeaway too).

## Operator-takeaway

The `{ok, data, meta}` envelope cohort grows by one
surface. The 3-surface anti-pattern catalogue from
bd-44af89 is now cleared:

- doctor schema: shipped here.
- event log: bd-75d5a3 (already shipped).
- summary: bd-bbcc36 (already shipped).

Next likely no-ok-flat-envelope candidates (per the
bd-5ae1ce envelope catalogue): `caco config schema`
(bd-36edaa Issue 5), `caco config template-help`
(bd-36edaa Issue 6), `caco mcp` catalog (bd-a66641
Issues 2+3 — deliberately deferred because the catalog
IS the wire format), `caco choices`, `caco notify
prune`. Any of these would be a natural follow-up bead
of the same shape.

The pre-existing `caco-profile` test failure
(`stale-check` not in `docs/profiles.html`) persists
into this drain. Worth opening a small bead to add
the missing `<tr>` row.
