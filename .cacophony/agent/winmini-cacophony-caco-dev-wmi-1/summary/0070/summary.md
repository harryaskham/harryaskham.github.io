# Session summary — bd-93e389: bd list --since/--before validation; log exceptions --since accepts RFC 3339; validate_since_or_rfc3339 helper

## Goal

Two related --since validation gaps + helper extraction:

1. `caco bd list --since notatime` silently returned the
   full result list (no validation at the dispatch site).
   `--before` had the same shape and same gap.
2. `caco log exceptions --since 2025-01-01T00:00:00Z`
   rejected the absolute timestamp form — only accepted
   `<N>(s|m|h|d)` duration shape. The bd-33d37c convention
   is "accept either parse_since_duration OR
   chrono::parse_from_rfc3339" (mirrors daemon
   parse_since shape).

12th in the silent-unknown-value/silent-since-drop
family. Per operator-takeaway from summary 0069
("at 4th --since site, extract validate_since_or_rfc3339
helper"), this is the 4th site (event-log + log-
exceptions + msg-inbox + bd-list/before) — extracting
the helper this round.

## Bead(s)

- `bd-93e389` — own bead. Closed.

## Before state

```
$ caco bd list --since notatime --limit 1
bd-01aba0  P1  bug  closed   ...   # silent fall-through

$ caco bd list --before notatime --limit 1
bd-01aba0  ...                     # silent fall-through (same)

$ caco log exceptions --since 2025-01-01T00:00:00Z
error: invalid --since value: 2025-01-01T00:00:00Z
(expected e.g. 3h, 30m, 1d)        # rejects valid RFC 3339
```

## After state

```
$ caco bd list --since notatime --limit 1
error: invalid --since value 'notatime' (expected e.g.
3h, 30m, 1d or RFC 3339 timestamp)

$ caco bd list --before notatime --limit 1
error: invalid --before value 'notatime' (expected e.g.
3h, 30m, 1d or RFC 3339 timestamp)

$ caco bd list --since 1d --limit 1            (real result, unchanged)
$ caco event log --since 2025-01-01T00:00:00Z   (real result, unchanged)

$ caco log exceptions --since 2025-01-01T00:00:00Z
exceptions on winmini (12083 total cluster-wide, 50
after --since filter, showing 50):
```

## Diff summary

- 1 file touched, +35 / −15:
  - `crates/caco-cli/src/lib.rs`:
    - new `validate_since_or_rfc3339(flag_name, value)`
      helper near `validate_enum_flag`. Wraps
      `parse_since_duration` ∪ chrono RFC 3339 parse;
      mirrors daemon-side parse_since (event_log.rs:198-216).
    - `dispatch_bd_list`: `--since` and `--before` both
      validated via the helper.
    - `dispatch_event_log`: replaced inline 2-call
      validation with the helper.
    - `dispatch_log_exceptions`: extended to accept the
      RFC 3339 form (was duration-only). `since_cutoff`
      derivation handles both shapes — duration -> Utc::now() - dur,
      RFC 3339 -> ts.with_timezone(&Utc).
  - msg-inbox callsite (bd-60c7de) intentionally left as
    RFC-3339-only because its existing API contract per
    --help is "RFC 3339 timestamp" — switching to dual-shape
    would silently broaden the contract.

## Verification

- `cargo build --bin caco`: clean.
- bd list --since notatime / --before notatime → errors.
- bd list --since 1d → unchanged.
- log exceptions --since notatime → error.
- log exceptions --since <rfc3339> → real result with
  --since filter applied.
- event log --since <rfc3339> → unchanged.

## Operator-takeaway

12th in family. helper now landed:
`validate_since_or_rfc3339("--since", value)?;` —
4 callsites, paired naturally with `validate_enum_flag`.

Naming convention now stable:
- `validate_<thing>_flag` for value-shape validators
  (returns `Result<(), CliError>`).
- Co-located near `validate_project_name` so any future
  validator is discoverable.

The `dispatch_log_exceptions` since_cutoff change is
slightly more involved than the other sites because it
ALSO needs the parsed value (not just to validate it) —
the RFC 3339 absolute path skips the `Utc::now() - dur`
math. Future surfaces that need the parsed datetime
should follow this pattern: try duration first, fall
back to RFC 3339, error on neither.

msg-inbox stays RFC-3339-only deliberately — its --help
contract is explicit. Future expansion of that contract
should bump --help text first.
