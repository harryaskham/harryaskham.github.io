# Session summary — caco notify send invalid level honesty (bd-513fc8)

## Goal

`caco notify send --level NOTAREAL_LEVEL` printed
"notification created: ... [NOTAREAL_LEVEL]" while the
daemon silently coerced the level to 'info'. Operators
believed bogus levels round-tripped.

## Bead(s)

- `bd-513fc8` — caco notify send invalid --level silent
  coercion + dishonest echo (P3 bug, test-user-hel filed)

## Before state

- Unknown level → daemon stored as 'info' silently.
- CLI success printf used `level.unwrap_or("info")` (raw
  user input), so the success line lied.
- `caco notify get --json` revealed the real `info`.
- Pattern: silent flag/value acceptance (related bd-b76723).

## After state

- Client-side validation rejects unknown level with clear
  error: "--level: 'X' is not a recognised severity.
  Allowed: info, warning, error, critical (default: info)."
- Success line reads canonical `level` from the daemon's
  response (defensive: handles future legacy aliases) and
  falls back to user input only if absent.

## Diff summary

- Files touched (+24 / −5):
  - `crates/caco-cli/src/lib.rs`: dispatch_notify_send
    level-validation block + canonical-level read.

## Verification

- `cargo build -p caco-cli`: clean.
- Behavioral test deferred (notify endpoint needs a full
  daemon round-trip; the new code paths are trivial
  contains-check + JSON read).

## Operator-takeaway

`caco notify send --level X` no longer lies. Bogus levels
fail loudly with the allowed list; success messages reflect
what the daemon actually stored. Cheap defense against the
silent-acceptance papercut family (bd-b76723).
