# Session summary — bd-4b0650: dispatch_bd_search_all_projects forwards --status/--type/--priority per-project (bd-163f53 follow-up)

## Goal

bd-163f53 added narrowing filter validation +
single-project forwarding for `caco bd search`.
The --all-projects fan-out path validated the
filters but did not pass them into per-project
requests — silently returning unfiltered bead
sets and only post-merging the substring filter
on test_name. This is exactly the silent-divergence
class the bd-163f53 work was meant to eliminate.

Fix: thread `validated_filters` into
dispatch_bd_search_all_projects, build the same
query string per-project URL.

## Bead(s)

- `bd-4b0650` — own self-source from working on
  bd-163f53 path. Closed.

## Before state

```
$ caco bd search --query reintegrate \
    --all-projects --status open --limit 3
matches for 'reintegrate' across 9 projects (3 hits):
  cacophony   bd-00cda6  [draft]   ...
  cacophony   bd-042fe5  [draft]   ...
  cacophony   bd-04c743  [closed]  ...
                  ↑ --status open silently ignored
```

## After state

```
$ caco bd search --query reintegrate \
    --all-projects --status open --limit 3
matches for 'reintegrate' across 9 projects (3 hits):
  cacophony   bd-2c399b  [open]    ...
  cacophony   bd-526670  [open]    ...
  cacophony   bd-6ff0a0  [open]    ...
                  ↑ filter applied per-project
```

`--status bogus` continues to error consistently
across both paths (bd-163f53 invariant preserved).

## Diff summary

- 1 file touched, +20 / −3:
  - `crates/caco-cli/src/lib.rs`:
    - dispatch_bd_search now passes
      `&validated_filters` through to
      dispatch_bd_search_all_projects.
    - dispatch_bd_search_all_projects signature
      gains `validated_filters: &[(String, String)]`.
    - Per-project URL construction reuses the
      single-project pattern: append `?k=v&k=v`
      from validated_filters before sending.

## Verification

- `cargo build --bin caco`: clean.
- `cargo test-small`: 57 passed.
- `cargo clippy -p caco-cli`: clean.
- 3 live cases:
  - bare --all-projects (all-statuses) returns
    drafts/closed/permanent.
  - --all-projects --status open returns only
    open beads.
  - --all-projects --status bogus errors
    consistently.

## Operator-takeaway

bd-163f53 said "validation BEFORE the
--all-projects branch ensures errors are
consistent". This turn closes the matching
behaviour gap: filters now actually FILTER on
both paths, not just on the single-project path.

The lesson — when a fan-out path is bolted onto
an existing single-target dispatcher, every
parameter must be reviewed for forwarding-vs-
ignoring, not just validation. The original
bd-163f53 fix half-closed the loop; this is the
matching second half.

Pattern reusable: any --all-projects, --all-nodes,
or --fan-out command that accepts narrowing
filters must (a) validate at the top, (b) forward
into each per-target request. Stop at (a) and you
get the most operator-confusing failure mode:
inputs accepted, parsed, then quietly dropped.
