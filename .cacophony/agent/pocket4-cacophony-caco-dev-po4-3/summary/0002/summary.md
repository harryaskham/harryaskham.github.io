# Session summary — bd-fda089 close-discipline status-bypass fix

## Goal

Close the `caco bd update --status closed` loophole that bypasses the
close-discipline audit gate (origin/main grep, audit footprint,
admin-override requirement, reason field) which lives only in
`caco bd close`. Reject terminal status transitions from the update
path and point callers at `caco bd close` (with `--admin-override
--reason` for legitimate non-landed closes).

## Bead(s)

- `bd-fda089` — [close-discipline] caco bd update --status=closed
  bypasses the close-audit gate; reject status transitions in update,
  force callers through caco bd close

## Before state

- Failing tests: none.
- Relevant metrics: in `crates/caco-cli/src/lib.rs`,
  `dispatch_bd_update`'s `--status` handler unconditionally PATCHed
  whatever value the caller passed to the daemon. `bd update --status
  closed` and `bd update --status deleted` therefore went around the
  close-discipline gate that lives in `caco bd close` (origin/main
  grep, hourly caco-ctrl audit, admin-override + reason capture,
  bd-id verification on main).
- Context: router used the loophole legitimately (collective/bd-9728bd
  retire) — but the same path is open to anyone, accidentally or to
  dodge the rule. Operator + caco-ctrl filed bd-fda089 as P0 to plug it.

## After state

- Failing tests: none. `cargo test -p caco-cli --lib bd_update`
  reports 12 passed (was 9 — 3 new tests).
- Relevant metrics: `dispatch_bd_update` now rejects `--status closed`
  and `--status deleted` with a structured `invalid_argument` error
  pointing the caller at `caco bd close` and mentioning the
  `--admin-override --reason` escape hatch for legitimate non-landed
  closes. The `--status` arg help text in `BD_UPDATE_ARGS` is updated
  to document the new restriction with the bd-id reference.
- Context: bd-fda089 ACs satisfied: AC1 (closed transition fails with
  helpful error), AC2 (`caco bd close` unaffected), AC3 (admin-override
  escape hatch is mentioned in the error so operators know how to do
  legitimate retires), AC4 (other field flags — title, description,
  priority, dependencies, labels — completely unchanged), AC5 (3 unit
  tests covering closed-rejected, deleted-rejected, open-not-rejected),
  AC6 (--help text updated via BD_UPDATE_ARGS summary). Non-terminal
  status transitions (open, in_progress, draft, permanent) still flow
  through update normally so the reopen / return-to-queue / draft-
  promotion paths are untouched.

## Diff summary

- Files touched:
  - `crates/caco-cli/src/lib.rs`:
    - `dispatch_bd_update` --status handler: 4-line guard + 19-line
      error message (rejects `closed` / `deleted` terminal transitions)
    - `BD_UPDATE_ARGS` --status entry: updated summary to document the
      new restriction
    - 3 new unit tests covering the rejection contract
- Tests: +3 unit tests (`bd_update_rejects_status_closed`,
  `bd_update_rejects_status_deleted`,
  `bd_update_status_open_does_not_hit_terminal_rejection_gate`).
- Behavioural delta: the only path-of-execution change is for
  `--status closed` and `--status deleted` callers, who now receive a
  structured CLI error instead of a silent PATCH. Every other update
  path (other field flags, other status values, --duplicate-of) is
  byte-identical.

## Embedded artefacts

(None — pure code change.)

## Operator-takeaway

The close-discipline gate is now mechanical at the CLI layer for the
two terminal transitions it cared about: nothing reaches the daemon
PATCH path with `status=closed` or `status=deleted` from `caco bd
update` anymore. Combined with the existing `caco bd close` audit
(origin/main grep + admin-override) and the hourly caco-ctrl audit,
this closes the obvious bypass. Other terminal paths to consider for
the same treatment in future: any future `wontfix` or `archived`
status that the daemon may add. Server-side enforcement (daemon
PATCH endpoint refusing terminal transitions) would be the natural
next layer if the close-audit gate ever migrates fully into the
daemon — file as a follow-up if desired.
