# Session summary — bd-6cf4a3: --since wording unified across event log / bd list / msg inbox / msg history; msg inbox newly supports duration shortcuts

## Goal

Probe finds 3 distinct --since validators across the
CLI:

  msg history: '--since/--until: expected RFC3339 or
               duration with s/m/h/d suffix; got X'
  msg inbox:   'invalid --since value X (expected
               RFC 3339 timestamp, e.g. 2025-01-01...)'
               (no duration support at all)
  event log:   'invalid --since value X (expected e.g.
               3h, 30m, 1d or RFC 3339 timestamp)'
               (validate_since_or_rfc3339 helper)
  bd list:     same as event log (uses helper)

Three issues:
1. Different error wording for the same flag.
2. msg inbox didn't accept duration shortcuts at
   all — operators reasonably expect '--since 1d'
   to work everywhere it's exposed.
3. msg history accepts duration but used third
   wording.

## Bead(s)

- `bd-6cf4a3` — own self-source. Closed.

## Before state

```
$ caco msg inbox --since 1h
error: invalid --since value '1h' (expected RFC 3339
timestamp, e.g. 2025-01-01T00:00:00Z)

$ caco msg history --since garbage
error: --since/--until: expected RFC3339 or duration
with s/m/h/d suffix; got "garbage"
```

## After state

```
$ caco msg inbox --since 1h
no unread messages              # newly works

$ caco msg history --since garbage
error: invalid --since/--until value "garbage"
(expected e.g. 3h, 30m, 1d or RFC 3339 timestamp)
                                # unified wording

$ caco msg inbox --since garbage
error: invalid --since value 'garbage' (expected
e.g. 3h, 30m, 1d or RFC 3339 timestamp)

$ caco event log --since garbage
[same canonical wording]

$ caco bd list --since garbage
[same canonical wording]
```

## Diff summary

- 1 file touched, +18 / −15:
  - `crates/caco-cli/src/lib.rs`:
    - `dispatch_msg_inbox` `--since`: replaced
      bd-60c7de's RFC3339-only validator with
      `validate_since_or_rfc3339("--since", s)?` +
      `resolve_since_cutoff(s)` to convert duration
      shortcuts to canonical RFC 3339 before
      sending. The inbox daemon endpoint
      deserialises `since` as DateTime<Utc> (RFC 3339
      only), so the conversion must happen
      client-side. URL-encoding logic preserved.
    - `parse_history_when` (msg history): unified
      error wording with the helper. Behaviour
      unchanged; just the message text matches.

## Verification

- `cargo build --bin caco`: clean.
- `cargo test-small`: 57 passed.
- 6 cases verified live across all 4 surfaces:
  - msg inbox: `--since 1h` newly works,
    `--since garbage` errors cleanly.
  - msg history: `--since garbage` errors with
    unified wording.
  - event log + bd list: unchanged behaviour,
    matching wording.

## Operator-takeaway

CLI honesty has two related quality dimensions:
- **Behavioural consistency**: the same flag does
  the same thing on different commands.
- **Wording consistency**: the same error reads the
  same way wherever it appears.

Today's bead was both: msg inbox didn't accept
duration shortcuts (behavioural gap) AND emitted
different error wording (cosmetic gap). Operators
who learn `--since 1h` on `bd list` reasonably
expect it on `msg inbox`. The fix is mechanical:
route through the canonical helper, convert
duration to RFC 3339 client-side where the daemon
only accepts RFC 3339.

The `resolve_since_cutoff(s) -> Option<String>`
helper is the right primitive for any new --since
surface where the daemon only accepts RFC 3339:
client validates with `validate_since_or_rfc3339`,
client converts with `resolve_since_cutoff`,
daemon sees a clean RFC 3339 string.

Cluster status: all 4 user-visible --since surfaces
now share canonical wording. Future audit: any new
--since surface should use the same pair (validate +
resolve) rather than rolling its own RFC3339-only
check.
