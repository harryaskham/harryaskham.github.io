# Session summary — bd-1d6985: msg inbox --tail validates via shared validate_positive_limit; helper takes flag-name parameter

## Goal

Sibling of bd-a08f85. `caco msg inbox --tail` is the
ergonomic alias (bd-1d476a) for `--limit` but had no
client-side validation. Probe revealed:

  --tail 0   → "no unread messages" (silent-swallow)
  --tail abc → ugly daemon HTTP 400

Same family as bd-a08f85; needs the same fix. Helper
generalised to take a flag-name parameter so the error
message is honest about which flag was bad.

## Bead(s)

- `bd-1d6985` — own follow-up. Closed.

## Before state

```
$ caco msg inbox --project cacophony --tail 0
no unread messages

$ caco msg inbox --project cacophony --tail abc
error: daemon response parse failed (HTTP 400 Bad Request):
  expected value at line 1 column 1
```

## After state

```
$ caco msg inbox --project cacophony --tail 0
error: --tail must be >= 1 (use --tail 1 for a single
result, or omit --tail for the default)

$ caco msg inbox --project cacophony --tail abc
error: invalid --tail value: abc (expected a positive
integer)

$ caco msg inbox --project cacophony --tail 5
(real result)
```

## Diff summary

- 1 file touched, +13 / −10:
  - `crates/caco-cli/src/lib.rs`:
    - `validate_positive_limit` signature changed
      from `(value: &str)` to
      `(flag_name: &str, value: &str)`.  Error
      messages now include the actual flag name (so
      `--tail` says "--tail" not "--limit").
    - 5 existing bd-a08f85 callers updated to pass
      `"--limit"` (mechanical sed, all on one line).
    - new caller in msg inbox dispatch arm:
      `validate_positive_limit("--tail", s)?` after
      the existing `--limit` validation.

## Verification

- `cargo build --bin caco`: clean.
- --tail 0 → friendly error.
- --tail abc → friendly error (no longer leaks
  daemon HTTP 400).
- --tail 5 → real result.
- --limit 0 (regression check) → still errors.

## Operator-takeaway

When extracting a validator helper, take the flag
name as a parameter from day one — the second caller
inevitably wants a different name, and rewriting
later is a sed sweep that's prone to introducing
subtle inconsistencies (e.g. forgetting one
mention).  Helper signatures of the shape
`validate_X(flag_name: &str, value: &str)` are now
the convention in this cluster:

- `validate_enum_flag(flag_name, value, allowed)`
- `validate_since_or_rfc3339(flag_name, value)`
- `validate_positive_limit(flag_name, value)`

All three take `flag_name` first and use it
consistently in error messages. Future helpers in
the family should follow.

Probe surface for next sweep: `--lines` (service
logs), `--count` (loop), `--offset` (msg inbox).
`--lines 0` returned `-- No entries --` from
journalctl which may be intentional (journalctl's
own behaviour); `--offset 0` is legitimately the
'start at beginning' value so should NOT be
rejected.  Probe carefully before generalising.
