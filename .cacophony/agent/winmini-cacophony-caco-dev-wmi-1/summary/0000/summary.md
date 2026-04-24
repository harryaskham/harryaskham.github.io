# Session summary — bd-bbcc36: caco summary --json envelope wrap

## Goal

Wrap `caco summary --json` output in the standard `{ok, data, meta}`
envelope so scripts can `jq -e .ok` and distinguish daemon-error from
valid-but-empty payloads. Eliminates one of three remaining bare-object
surfaces catalogued under bd-5ae1ce.

## Bead(s)

- `bd-bbcc36` — caco summary --json emits BARE flat object (no
  {ok,data,meta} envelope) — 11th distinct JSON envelope shape
  catalogued (P3 bug).

## Before state

`dispatch_summary` returned `data` directly:

```
$ caco summary --since 1h --json | jq 'keys'
["agents_completed", "agents_failed", ..., "since", "total_events"]
```

No `ok`, no `data`, no `meta` wrapper. Scripts using `jq -e .ok` to
gate on success broke; operators couldn't distinguish "summary not
yet computed / daemon error" from "summary computed but ok=false".

## After state

`dispatch_summary` wraps the daemon's `data` payload in the standard
envelope:

```
$ caco summary --since 1h --json | jq 'keys'
["data", "meta", "ok"]
```

- `ok: true` — present so scripts can `jq -e .ok`.
- `data` — the existing summary payload, unchanged.
- `meta` — carries `since` (the original CLI arg) and `since_iso`
  (the resolved RFC3339 cutoff) so consumers can introspect the
  query window without re-parsing.

## Diff summary

- 1 file changed, +14 / -1 (`crates/caco-cli/src/lib.rs`
  `dispatch_summary`).

## Validation

- `cargo check -p caco-cli --tests`: clean.
- No existing tests assert on the bare-object shape (grep'd
  workspace for `summary --json` / `dispatch_summary` callers — only
  doc references in caco-daemon).

## Operator-takeaway

Anyone scripting against `caco summary --since … --json` will now
see `ok` / `data` / `meta` keys at the top level. The previous bare
keys (`agents_completed`, `total_events`, …) move under `.data`.
This matches `caco bd list --json`, `caco event log --json`, and
the rest of the catalogued envelope shapes. Two bare-object surfaces
remain: `caco agent get --json` (bd-3a6078) and `caco profile show
--json` (bd-548e77).
