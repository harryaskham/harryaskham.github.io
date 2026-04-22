# Session summary — bd-a08f85: --limit 0 silent-swallow closed at 5 read-side surfaces; validate_positive_limit helper extracted

## Goal

After bd-a29677/bd-8c5bda closed --limit 0 on the
audit/merge-queue write-side surfaces, a probe of the
read surfaces revealed the same silent-swallow at 5
common commands:

- caco event log
- caco log exceptions
- caco notify list
- caco msg inbox
- caco bd list

All silently returned "no results" for --limit 0.
Operator typos masquerading as empty data.

## Bead(s)

- `bd-a08f85` — own follow-up. Closed.

## Before state

```
$ caco event log --limit 0          → No command events recorded.
$ caco log exceptions --limit 0     → exceptions on winmini (... showing 0):
$ caco notify list --limit 0        → no notifications found
$ caco msg inbox --limit 0          → no unread messages
$ caco bd list --limit 0            → no beads found
```

## After state

All 5 surfaces:

```
error: --limit must be >= 1 (use --limit 1 for a
single result, or omit --limit for the default)
```

`--limit 1` works as before on every surface.

## Diff summary

- 1 file touched, +50 / −10:
  - `crates/caco-cli/src/lib.rs`:
    - new `validate_positive_limit(value: &str)
       -> Result<usize, CliError>` helper near
      `validate_enum_flag` and `validate_since_or_rfc3339`.
      Parses + validates >= 1; rejects 0 with hint.
    - 5 dispatch arms updated to call the helper
      before forwarding the limit downstream
      (event log, log exceptions, notify list, msg
      inbox, bd list — the bd list site lives inside
      `dispatch_bd_list`).

## Verification

- `cargo build --bin caco`: clean.
- All 5 commands × `--limit 0` → identical friendly
  error.
- `caco bd list --limit 1` → real result (unchanged).
- The event-log dispatch arm previously did
  `s.parse::<usize>().ok()` (silent on parse failure
  too); switching to `validate_positive_limit?` also
  closes that smaller silent-swallow.

## Operator-takeaway

3 helpers now landed in the validation cluster:
- `validate_enum_flag(name, value, allowed)` (bd-2c6856).
- `validate_since_or_rfc3339(name, value)` (bd-93e389).
- `validate_positive_limit(value)` (this bead).

Co-located near `validate_project_name` for
discoverability. Pattern is now stable enough that any
new dispatch arm for a `--limit`/`--enum`/`--since`
flag should reach for these helpers first.

The dispatch-arm-pre-validation pattern (vs in-
dispatcher) keeps the shape consistent across the
surface and lets the dispatcher receive an already-
parsed `usize` (or `&str` to forward). Neither shape
is universal here — bd-list parses inside the
dispatcher because the limit interacts with
client-side filters in non-trivial ways. That
exception is fine; the helper still does the
typo-rejection job.

Future audit candidate: `--limit -1` is rejected by
the parser (negative numbers parsed as flags), but
`--limit 99999999999999999` (overflow) silently
clamps in some surfaces. Filing if it ever becomes
visible; for now the typo-zero is the high-impact win.
