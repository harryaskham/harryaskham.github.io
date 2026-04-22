# Session summary — bd-b617cc: caco notify list flag validation

## Goal

Two papercuts on `caco notify list`:

1. `--level <bogus>` silently dropped (silent
   unknown-value, bd-b76723 family). Returned plausible
   "no notifications found" when the unfiltered set
   would have matched.
2. `--acknowledged maybe` reached the daemon as
   `acknowledged=maybe`, daemon returned HTTP 400, CLI
   surfaced "daemon response parse failed (HTTP 400 Bad
   Request): expected value at line 1 column 1" — a
   useless error that hides what the operator actually
   typed wrong.

## Bead(s)

- `bd-b617cc` — own bead, 8th in the silent-unknown-value
  family. Closed.

## Before state

```
$ caco notify list --level bogus
no notifications found        # silent

$ caco notify list --acknowledged maybe
error: daemon response parse failed (HTTP 400 Bad Request):
expected value at line 1 column 1
```

## After state

```
$ caco notify list --level bogus
error: unknown --level value 'bogus'.
Allowed: info, warning, error, critical

$ caco notify list --acknowledged maybe
error: invalid --acknowledged value 'maybe'
(expected true or false)

$ caco notify list --level info
  notif-... [info] user [cacophony]: ...
```

## Diff summary

- 1 file touched, +20 / −2:
  - `crates/caco-cli/src/lib.rs::dispatch_notify_list`:
    - `--level` validated against
      `["info", "warning", "error", "critical"]`.
    - `--acknowledged` validated as `"true"` | `"false"`.
    Both error before any HTTP request goes out.

## Verification

- `cargo build --bin caco`: clean.
- `--level bogus` → enum error.
- `--acknowledged maybe` → bool error.
- `--level info` → real result list.

## Operator-takeaway

8th in the silent-unknown-value family
(bd-126b99/bd-a403a1/bd-30fbfb/bd-2886bb/bd-dfc91a/bd-bc52ef
+ bd-c061d4/bd-3656ce/bd-40907c/bd-33d37c/bd-b617cc).
Pattern shape is now stable enough to extract a
`validate_enum_flag(name, value, allowed: &[&str])`
helper next time a 9th appears.

Subtle distinction in this bead: --level is enum
silent-swallow, --acknowledged is bool round-trip via
opaque HTTP 400. Both client-side validation kills two
classes of unhelpful error in one helper-style fix.
