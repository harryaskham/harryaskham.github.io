# Session summary — bd-3d6a13: document event-log state-mutating-only policy

## Goal

Per bd-0b47a7 test-user probe discovery: `caco event log` runs
successfully but does NOT appear in subsequent `caco event log`
output. The audit log records state-mutating commands (bd close,
agent stop, etc.) but NOT read-only commands. The bead asked to
either document explicitly OR fix.

Recommended path was (a) document, NOT (b) expand the audit
surface (path b would 10x audit log volume — bd-3bbc6f cron list
COMMAND truncation already shows volume-management is fragile).

## Bead(s)

- `bd-3d6a13` — caco event log doesn't log itself + msg speak /
  audio capabilities also unlogged — document or fix the 'state-
  mutating commands only' rule (P3 task, audit-log/cli/docs).

## Before state

```
$ caco event log --help
caco event log
  Show the command audit event log.

  Args:
    --type      Event type filter ...
    ...

$ caco event log
... (runs but no entry appears in next call)
```

Operators expected 'audit log' to mean ALL commands. Doc gap.

## After state

```
$ caco event log --help
caco event log
  Show the command audit event log. Records state-mutating commands
  only (e.g. bd close, agent stop, config write); read-only commands
  (status, list, query, event log itself) are excluded by design
  — see bd-3d6a13.

  Args:
    --type      Event type filter ...
    ...
```

The summary now ships in `caco --help event log` output and is
threaded through the MCP / agent-safe registry the same as every
other CommandSpec summary string.

## Diff summary

- 1 file changed, +13 / -1 (`crates/caco-cli/src/lib.rs`):
  - `EVENT_SUBCOMMANDS[0].summary` rewritten to document the
    state-mutating-only policy explicitly + cross-reference
    bd-3d6a13 + name canonical examples + name the workaround
    (`caco event record`) for explicit ledger entries.

## Validation

- `cargo check -p caco-cli`: clean.

## Operator-takeaway

The 'why doesn't event log show event log' surprise is closed.
Operators reading `--help` now see the design choice + know that
the omission is intentional (volume-management) + know they can
opt-in via `caco event record` for explicit ledger entries.
Doc-only fix; no behaviour change. msg speak / audio capabilities
fall under the same documented exclusion.

Push-discipline (post-clarification): own-branch push allowed;
default-branch force-push banned; only reintegrate / complete
land work on main. This session continues to use only local refs
+ daemon-mediated reintegrate.
