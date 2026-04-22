# Session summary — bd-163f53: caco bd search --status/--type/--priority validated + forwarded (sibling of bd list family)

## Goal

Probe finding (during bd-940284 / bd-6cf4a3 sweep):
caco bd search silently accepted --status, --type,
--priority filter flags. They weren't documented in
BD_SEARCH_ARGS, weren't validated, weren't forwarded
to the daemon. The bd-b76723 unrecognised-flag
warning fired in operator context but was easily
missed; in agent context it suppressed cleanly and
the operator got the full unfiltered match list.

## Bead(s)

- `bd-163f53` — own self-source. Closed.

## Before state

```
$ caco bd search --query foo --status bogus
matches for query across all statuses: foo
[full unfiltered list]
```

## After state

```
$ caco bd search --query foo --status bogus
error: unknown --status value 'bogus'. Allowed: open,
in_progress, closed, deleted, draft, permanent, blocked

$ caco bd search --query foo --type bogus
error: unknown --type value 'bogus'. Allowed: task,
bug, feature, epic

$ caco bd search --query foo --priority bogus
error: invalid --priority "bogus": expected a number
0-4 or one of critical/high/medium/low/lowest

$ caco bd search --query foo --status open --limit 3
[narrowed match set, daemon-side filtering]
```

Also: --all-projects fan-out path now validates
filters BEFORE fanning out (early-error preserves
the dispatch_bd_search_all_projects forwarding-as-
follow-up scope).

## Diff summary

- 1 file touched, +66 / −1:
  - `crates/caco-cli/src/lib.rs`:
    - `BD_SEARCH_ARGS`: 3 new ArgSpec entries
      (--status, --type, --priority) so --help and
      --json help surface the filters.
    - `dispatch_bd_search`: validates filters via
      same primitives bd list uses
      (validate_enum_flag for --type, inline match
      for --status to keep bd_cli_error structured
      envelope, parse_priority_flag for --priority).
      Validation moved BEFORE the --all-projects
      branch so the typo-error is consistent across
      both paths.
    - Single-project path: filters forwarded as
      daemon query params (status/type/priority).
    - --all-projects: validation only this turn;
      daemon-side forwarding per project is a clean
      follow-up (not in scope).

## Verification

- `cargo build --bin caco`: clean.
- `cargo test-small`: 57 passed.
- 5 cases verified live: 3 invalid-value rejections,
  1 valid filter forwarded, 1 --all-projects bogus
  rejected before fan-out.

## Operator-takeaway

bd search now matches bd list's input-honesty:
typos error loudly; documented filter set is
discoverable via --help and --json help.

The pattern noted on bd-4a4b8b (probe a sibling
dispatcher right after fixing one for a different
flag) keeps paying off. Today: bd list and bd search
are mostly the same shape (query the beads endpoint
+ post-filter). bd list had been hardened over many
beads (--status, --type, --priority, --since,
--before, --sort, --limit). bd search inherited
none of that hardening because it predates the
audit.

Cluster status: bd list and bd search now share the
same filter-validation surface area. Remaining
follow-up: dispatch_bd_search_all_projects could
forward the validated filters as query params on
each per-project request to reduce wire bandwidth +
align with single-project semantics. Filed as a
mental TODO if it becomes a complaint.

Self-sourced beads via probe in this session:
bd-4a4b8b (limits), bd-6cf4a3 (since), bd-163f53
(search filters). Pattern: take a freshly-touched
dispatcher, audit ALL its query-string flags for
missing validation. Cheap to do at the same call
site; high signal-to-effort ratio.
