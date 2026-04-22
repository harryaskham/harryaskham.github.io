# Session summary — bd-cca91a: caco <cmd> --json no longer hides agent-unsafe subcommands in agent context

## Goal

Sibling of bd-f79ad8 (multi-issue test-user bead
being worked by another agent on beelink). Slice
issue 2 of that bead — `--json` help applied the
agent-context subcommand filter, hiding agent-unsafe
subcommands from JSON output:

  $ caco agent teleport --json    (in agent context)
  { ..., "subcommands": [], ... }

…while the human help footer says "2 hidden". Agents
discover the surface via `--json`; with this filter
they had no way to learn that `transmit` and `receive`
exist.

## Bead(s)

- `bd-cca91a` — own self-source slicing bd-f79ad8.
  Closed.

## Before state

```
$ caco agent teleport --json | jq .subcommands
[]
```

## After state

```
$ caco agent teleport --json | jq -r '.subcommands[].name'
transmit
receive
```

(Human help still filters with the correct hidden
count: `(note: 2 additional subcommand(s) hidden in
agent context; ...)`.)

## Diff summary

- 1 file touched, +14 / −2:
  - `crates/caco-cli/src/lib.rs`:
    - `render_json_help`: replaced
      `visible_subcommands(spec)` with
      `visible_subcommands_with(spec, false)` —
      explicitly opt out of the agent-context filter
      for JSON output. Long doc comment explains
      that JSON is the discovery contract.
    - New unit test `json_help_does_not_apply_agent_context_filter`
      asserts `agent teleport --json` lists both
      `transmit` and `receive` (their `agent_safe=false`
      flag should NOT cause them to disappear from JSON).

## Verification

- `cargo build --bin caco`: clean.
- `cargo test -p caco-cli --lib json_help_does_not`: 1 passed.
- `cargo test-small`: 57 passed.
- Live: `caco agent teleport --json` now lists
  transmit + receive in agent context.
- Existing `agent_safe_filtering_hides_daemon_commands`
  test still passes — the human-text filter behaviour
  is unchanged.

## Operator-takeaway

CLI introspection has two consumers with different
contracts:
- Human text help (`--help`): curated for the current
  context; agent-unsafe subcommands hidden with
  honest "N additional hidden" footer.
- JSON help (`--json`): discovery contract for
  agents/MCP/scripts; complete surface, no context
  filtering.

Mixing these — applying the human-context filter to
JSON — silently breaks discovery. The fix here
codifies the contract: `--json` is universal, only
`--help` is contextual.

The off-by-one count complaint in bd-f79ad8 (issue 1)
was NOT reproducible on main — likely fixed between
1.2.513 and 1.2.514. Local probe of choices/agent
teleport/agent/config all show correct
`(note: N hidden)` where N matches `total - agent_safe`.

Pattern: when a single bead reports multiple loosely-
related issues and another agent claims it, file a
focused sibling for any issue you can isolate. Beats
either blocking on the parent or scope-creeping past
your slice.
