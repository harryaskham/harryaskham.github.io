# Session summary — bd-bc52ef: caco release list JSON shape + --status validation

## Goal

Two papercuts on `caco release list`:

1. JSON output uses generic `data` key while every other
   list surface uses a resource-named key
   (`.beads`, `.agents`, `.jobs`). Programmatic consumers
   writing `jq .releases` get nothing and have to special-
   case `.data` for this surface.
2. `--status` accepts unknown values (e.g. the natural-
   but-wrong `running`) silently — the filter just
   doesn't apply, full unfiltered list returned.

## Bead(s)

- `bd-bc52ef` — P3 bug, test-user-hel filed.

## Before state

```
$ caco release list --json | jq -c '.data[0]' | head -1
{ "id": "gha-...", ... }

$ caco release list --status running   # not in enum
queued     gha-... stable     github   —
... full unfiltered list
```

## After state

```
$ caco release list --json | jq -c '.releases[0]' | head -1
{ "id": "gha-...", ... }

$ caco release list --status running
error: unknown --status value 'running'. Allowed:
queued, building, pushing, completed, failed,
canceled, error
```

- `--status` validated client-side against the same enum
  the daemon accepts, before the request goes out.
- JSON `data` field renamed to `releases` client-side
  (the daemon's `SuccessEnvelope` is shared across all
  surfaces, so renaming it server-side would touch a
  cross-cutting type).  The text branch already iterated
  over `result["data"]`; switched to `result["releases"]`
  to use the renamed field.

## Diff summary

- 1 file touched, +37 / −2:
  - `crates/caco-cli/src/lib.rs::dispatch_release_list`:
    `ALLOWED_STATUS` const + validation, JSON key rewrite,
    text-branch lookup updated.

## Verification

- `cargo build --bin caco`: clean.
- `caco release list --status running` → error (was: full list).
- `caco release list --status queued` → still works.
- `caco release list --json | jq .releases` → array (was: null).

## Operator-takeaway

Family with bd-3656ce, bd-2a744a, bd-513fc8, bd-eb84c8,
bd-30fbfb (silent unknown-value acceptance pattern);
this is now the 6th instance closed in the family, all
following the same shape: enumerate the allowed set,
validate at the dispatch boundary, error with the full
allowed list. Worth a follow-up refactor to extract a
common `validate_enum_flag(name, value, allowed)`
helper if a 7th occurrence appears.
