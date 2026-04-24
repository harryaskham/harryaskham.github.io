# Session summary — bd-47df20: caco timeline validator hardening + envelope wrap

## Goal

Pin multiple real bugs in the bd-47df20 sweep of the new
`caco timeline` surface (bd-431d5b v1.2.537):
- **--scope ''**: HTTP-400-leak with daemon URL + raw JSON.
- **--scope bogus**: same HTTP-400-leak path.
- **--limit 0**: silent-clamp-to-1 (drift from USAGE-GUIDANCE).
- **--limit 501**: silent-clamp-to-500 even though --help says
  'max 500' (--help-vs-reality drift).
- **--max-age-hours 0**: silent-accept, returns full timeline.
- **--min-commits 0**: silent-accept, returns empty timeline
  (drift vs --limit 0 which clamped).
- **--json envelope**: top-level `{scope, timelines}` with NO
  `ok`/`meta`/`data` wrapper (7th NO-OK envelope drift surface).

## Bead(s)

- `bd-47df20` — caco timeline sweep (P3 bug, multi-issue). Pins
  the contained validator + envelope work. POSITIVES (Issue 1 NEW
  required-flag GOLD '--scope=project requires --project=<name>',
  Issue 2 ASCII tree gold-standard rendering) preserved as
  promote candidates. --json-on-error 4th JSON-broken is the
  shared HTTP-wrapper (bd-dda312 5-field rich envelope) — defer
  to that landing. Parser-ambiguity --max-age-hours -1 24th cohort
  surface covered by bd-02c404.

## Before state

```
$ caco timeline --scope ''
error: daemon returned HTTP 400 for http://127.0.0.1:11100/api/v1/timeline?scope=: {"error":"...","request_id":"..."}

$ caco timeline --scope bogus
error: daemon returned HTTP 400 for http://127.0.0.1:11100/api/v1/timeline?scope=bogus: {"error":"expected cluster|project","request_id":"..."}

$ caco timeline --limit 0
Timeline (scope=cluster, ... showing 1)        # silent clamp-to-1

$ caco timeline --limit 501
Timeline (scope=cluster, ... showing 500)      # silent clamp despite --help cap

$ caco timeline --max-age-hours 0
Timeline (scope=cluster, ...)                  # silent accept

$ caco timeline --min-commits 0
Timeline (scope=cluster, no projects)          # silent empty

$ caco timeline --json | jq 'keys'
[ "scope", "timelines" ]                       # NO ok / data / meta
```

## After state

```
$ caco timeline --scope ''
error: --scope value cannot be empty (allowed: cluster, project)

$ caco timeline --scope bogus
error: unknown --scope value 'bogus' (allowed: cluster, project)

$ caco timeline --limit 0
error: --limit must be >= 1 (use --limit 1 for the most recent event, or omit for the default of 50)

$ caco timeline --limit 501
error: --limit must be <= 500 (got 501; --help states the cap is 500)

$ caco timeline --max-age-hours 0
error: --max-age-hours must be >= 1 (use --max-age-hours 1 for the most recent hour, or omit for no time bound)

$ caco timeline --min-commits 0
error: --min-commits must be >= 1 (use --min-commits 1 to include single-commit projects, or omit for the default floor)

$ caco timeline --json | jq 'keys'
[ "data", "meta", "ok" ]
$ caco timeline --json | jq '.meta'
{ "surface": "caco timeline", "limit": 50, "scope": "cluster" }
```

## Diff summary

- 1 file changed, +57 / -7 (`crates/caco-cli/src/lib.rs`):
  - `dispatch_timeline`: upfront empty/enum guards on --scope;
    explicit reject of 0-values for --max-age-hours, --min-commits,
    --limit; explicit reject of --limit > 500 (matches --help).
  - --json envelope wrapped in `{ok, data, meta:{surface, limit,
    scope}}`. Catalogue: NO-OK cohort 3→2 (cert status, mcp; log
    exceptions still outstanding).
  - --scope=project + empty/whitespace --project also caught by
    same upfront guard.

## Validation

- `cargo check -p caco-cli`: clean.

## Operator-takeaway

`caco timeline` now rejects bad inputs upfront with USAGE-GUIDANCE
phrasing instead of silent-clamp drift, and honours its own --help
cap. --json envelope joins the canonical {ok,data,meta} shape.
NO-OK envelope cohort shrinks 3→2 this turn.

Push-discipline (post-clarification): own-branch push allowed;
default-branch force-push banned; only reintegrate / complete
land work on main. This session continues to use only local refs
+ daemon-mediated reintegrate.
