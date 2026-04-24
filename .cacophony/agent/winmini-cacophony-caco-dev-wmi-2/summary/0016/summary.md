# Session summary — bd-ad8ced: caco event log --node/--agent did-you-mean --caller hint

## Goal

Implement the bd-9c55aa alias-hint family for
`caco event log`. Operators reach for `--node` and `--agent`
first when filtering event log because those are real flags
on sister surfaces (`caco scp/ssh/agent` carries `--node`,
`caco msg` carries `--agent`); the actual filter is
`--caller` (which embeds `node:project:agent-id`). Today
both flags drop silently with the generic bd-b76723 warning;
no operator-actionable hint.

## Bead(s)

- `bd-ad8ced` — `caco event log: --node and --agent silently
  ignored (real flag --caller) — alias-hint opportunity per
  bd-9c55aa`.

## Before state

```
$ caco event log --node helsinki
warning: bd-b76723: `caco event log` received unrecognised
  flag(s): --node. These were ignored by the dispatcher. Set
  CACO_STRICT_UNKNOWN_FLAGS=1 to make this an error.
```

Operator has to consult `caco event log --help` to discover
the correct filter is `--caller`. Same generic wording for
`--agent`.

## After state

```
$ caco event log --node helsinki
warning: bd-b76723: `caco event log` received unrecognised
  flag(s): --node. These were ignored by the dispatcher. Set
  CACO_STRICT_UNKNOWN_FLAGS=1 to make this an error.
  Hint: --node → did you mean --caller? (filters by 'caller'
  field which embeds node:project:agent-id)
```

The bd-b76723 mechanism stays uniform — the hint is appended
as a `Hint:` suffix rather than replacing the generic warning.
Both `--node` and `--agent` get distinct entries; passing both
at once yields a multi-clause hint (`...; ...`).

## Diff summary

- `crates/caco-cli/src/lib.rs`:
  - New helper `alias_hint_for(command_name, unknown)`:
    table-driven (4-tuple `(command, typo, real, why)`),
    returns `" Hint: ..."` suffix or empty string. Initial
    table: 2 entries (event log → --node/--agent → --caller).
    Designed for cluster-wide extension as new operator
    confusion patterns surface during test-user passes.
  - `warn_or_error_unknown_flags_with_strictness`: appends
    `alias_hint_for(...)` output to both the bd-b76723
    (read-only) and bd-4c8fdd (state-mutating) warning
    bodies, so the hint surfaces regardless of which
    refusal/warning path fires.
  - 1 new test:
    `alias_hint_for_event_log_node_and_agent_suggests_caller`
    — covers individual --node hint, individual --agent
    hint, both-at-once, no-entry-for-command (returns
    empty), empty-unknown-list (returns empty).
- `cargo test -p caco-cli --lib alias_hint_for_...`: pass.
- `cargo test-small`: 182 pass.

## Operator-takeaway

The alias-hint table is the new home for known
operator-natural typo → real-flag mappings. To extend, add
a `(command, typo, real, why)` tuple to `ALIASES` in
`alias_hint_for`. The mechanism inherits cluster-wide via
`warn_or_error_unknown_flags_with_strictness`, so any
existing dispatcher that already calls that function gets
the new hint for free with no per-dispatcher changes.

Likely next-victims (follow-up beads of the same shape):

- `caco bd list --agent X` → real flag `--assignee` (cluster
  pattern, --agent on sister surfaces).
- `caco msg --node X` → real flag varies by subcommand
  (`--agent` on speak/inbox, `--target` on snapshot).
- `caco summary --since-version X` → real flag `--since`
  (per bd-7abbba Issue 9).
- `caco changelog show --version X` → real flag `--since`
  (bd-dda312 Issue 9 — dropped because UX rename, not bug).

The conservative "only add entries for documented operator
confusion" stance avoids over-eager guessing; each addition
should reference the bead that surfaced the confusion so
the table stays auditable.
