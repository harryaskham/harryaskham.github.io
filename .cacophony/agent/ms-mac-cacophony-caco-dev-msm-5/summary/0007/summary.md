# Session summary — bd-34d0b8: caco agent rename CLI alias

## Goal

Make agent renaming discoverable from the operator-facing CLI surface,
so that controllers (and the TUI/web/Android surfaces, eventually)
have a stable verb to call instead of needing to know the
`agent set --field short_name` field-set protocol.

## Bead(s)

- `bd-34d0b8` — Ability to rename agents in the TUI, android app
  webapp. Sets the short-name of the agent. (CLI slice landed; UI
  follow-ups filed.)

## Before state

- Daemon already had `set_field` (bd-3bf18e) accepting
  `field=short_name`, persisting via `persist_agent_json`.
- CLI exposed it as `caco agent set --id X --field short_name
  --value Y`.
- Operators / controllers had no discoverable rename verb —
  `caco agent --help` showed `set` and `get` as opaque generic
  field-mutators.
- TUI / caco-web / Android: no rename UI.

## After state

- New CLI subcommand `caco agent rename --id <id> --name <new>`
  (in `crates/caco-cli/src/lib.rs`):
  - `AGENT_RENAME_ARGS`: `--id` (required), `--name` (required;
    empty string clears short_name).
  - `CommandSpec` entry under `AGENT_SUBCOMMANDS` —
    `mcp_enabled = true`, `agent_safe = true`, `idempotent = true`.
  - Dispatch handler forwards to existing
    `dispatch_agent_set(id, "short_name", name, ...)`.
- New test `agent_rename_subcommand_exposed_in_spec` locks the
  spec contract (subcommand presence, mcp_enabled, idempotent,
  required args).
- Three follow-up beads filed for the multi-surface UI work
  (separated so each surface owner can pick them up):
  - bd-09e8df — TUI: Rename context-menu item + text-input dialog
  - bd-3ae0c6 — caco-web: agent rename on agent detail page
  - bd-3cf67f — android: rename in agent detail
- bd-34d0b8 description updated with status + follow-up links;
  left open as tracking parent.
- `cargo clippy -p caco-cli --tests` clean.

## Diff summary

- Commit: `7b83fc4b`
- Files touched: `crates/caco-cli/src/lib.rs` (+66 lines: args, spec
  entry, dispatch handler, test).
- Tests: +1.
- Behavioural delta: `caco agent rename` now exists as an alias for
  `caco agent set --field short_name`. No semantic change to the
  existing field-set surface.

## Out of scope (deferred to follow-ups)

- TUI Rename context-menu item + text-input dialog (bd-09e8df).
- caco-web agent rename UI (bd-3ae0c6).
- Android app rename in agent detail (bd-3cf67f).
- Investigation into why many agents land with `short_name=None`
  despite the configured `short_name_strategy` — recommended this
  be filed as a separate bug with concrete agent-id examples (the
  resolver needs to be debugged with real failure cases).

## Operator-takeaway

`caco agent rename --id <id> --name <new>` is the new discoverable
verb. Empty name clears the short_name (reverts to auto-generated
label). MCP-driven controllers can use it directly. The three UI
follow-ups will provide point-and-click rename in TUI, web, and
Android.
