# Session summary — bd-5bd14b: caco prune list --json wraps in {ok, data, meta} envelope

## Goal

Address bd-5bd14b's two implementable items:

- **Issue 3** — `caco notify get --id ''` leaks HTTP 404
  EOF on the same surface that returns gold-standard
  structured failure for `--id bogus` (within-surface
  inconsistency).
- **Issue 4** — `caco prune list --json` emits OK+flat
  envelope (8th flat-drift surface in the bd-5ae1ce
  catalogue).

Investigation: **Issue 3 is already fixed in source** via
bd-d761db Issue 6 (`if id.trim().is_empty()` guard at the
dispatch boundary; both text and JSON paths emit
`invalid_argument` structured error). The repro the bead
shows still hits because the deployed binary on PATH
(1.2.535) lags the source. No source change needed for
Issue 3.

Issue 4 is the actionable item: wrap `dispatch_prune_list`
JSON output in the standard envelope.

Issues 1 + 2 are positive observations (gold-standard
structured-failure envelope + canonical error.code enum
proposal). Issue 5 onwards (--notify-id alias) are
bd-b76723 family / cross-cutting.

## Bead(s)

- `bd-5bd14b` — `caco notify get + prune list — 2ND SURFACE
  CONFIRMS bd-b724cb STRUCTURED FAILURE ENVELOPE PROMOTE...`

## Before state

```
$ caco prune list --json | jq 'keys'
["agents", "ok", "total_reclaimable_bytes", "total_reclaimable_human"]
```

OK+flat: `ok` is at top level (good — passes `jq -e .ok`)
but `agents`/`total_*` siblings instead of nested under
`data`/`meta`. 8th surface in the flat-drift cohort
catalogued by bd-684112, bd-7abbba, bd-0b47a7, bd-20747b
(show + validate), bd-36edaa schema, bd-4df284 (service
show + service status).

## After state

```
$ caco prune list --json | jq 'keys, .data | keys, .meta | keys'
["data", "meta", "ok"]
["agents"]
["count", "total_reclaimable_bytes", "total_reclaimable_human"]
```

Joins the {ok, data, meta} cohort. Totals + count live in
`.meta` as metadata about the agent set; the agent list
itself is under `.data.agents`.

BREAKING for scripts reading top-level `.agents` /
`.total_reclaimable_*`: each gets a one-token jq path
prefix update (`.agents` → `.data.agents`,
`.total_reclaimable_bytes` → `.meta.total_reclaimable_bytes`).
The `ok` field stays at top level so `jq -e .ok` health
probes are unaffected.

## Diff summary

- `crates/caco-cli/src/lib.rs`:
  - `dispatch_prune_list`: replaced the OK+flat JSON
    output with `{ok, data: {agents}, meta: {count,
    total_reclaimable_bytes, total_reclaimable_human}}`.
    Comment notes the breaking-script implication and
    catalogs this as the bd-5ae1ce 8th surface conversion.
  - 1 new test:
    `dispatch_prune_list_json_uses_ok_data_meta_envelope`
    — source-greps the function body for the bd-5bd14b
    marker, the `data` / `meta` envelope keys, and the
    `agents` / `total_reclaimable_bytes` field placements
    so the shape can't silently regress.
- `cargo test -p caco-cli --lib dispatch_prune_list_...`:
  pass.
- `cargo test-small`: 182 pass.

## Operator-takeaway

The `{ok, data, meta}` envelope cohort gains another
surface. Of the 8-surface flat-drift catalogue, two are
now resolved (this bead's prune list + bd-44af89's doctor
schema landed earlier in this drain).

Issue 3 (notify get --id '') is **already fixed in source**
via bd-d761db Issue 6 — the deployed-binary lag will
resolve on next release cut. Verified by reading
`dispatch_notify_get` callsite in the dispatch table:
the `if id.trim().is_empty()` guard is in place and emits
the canonical `invalid_argument` structured error in both
text and JSON modes. No additional source change needed.

Issues 1 + 2 (structured-failure envelope + canonical
error.code enum) are positive observations and a
cluster-wide retrofit roadmap, not point fixes for this
bead. Worth a meta-tracker bead enumerating the 13
candidate surfaces (5 HTTP 404 EOF leaks + 3 HTTP 400
JSON leaks + 5 JSON-broken-on-error surfaces) so the
retrofit can be scheduled in priority order.
