# Session 0021 — bd-340a4f (caco agent introspect --show tools)

## Goal

Final slice of the bd-5d2d83 introspect family (slices: profile,
hooks, env, **tools**). Renders composed `Profile.mcp_servers` with
source-mixin attribution so an operator can answer "which mixin
contributes the github mcp server?".

## Bead(s)

- **bd-340a4f** — primary (self-filed bd-5d2d83 follow-up).
- **bd-cba8b4** — filed retro: 3 pre-existing broken-on-main issues
  in `crates/caco-cli/src/lib.rs` absorbed inline.

## Before state

- `caco agent introspect --show tools` returned `not yet implemented`.
- Main was broken in 3 ways:
  1. dispatcher arm at lib.rs:10147 called `dispatch_operator_actions_list(project, json_requested)` but that fn was deleted upstream (replaced by `dispatch_bd_operator_actions(json, flags, co)`); cargo check failed.
  2. clippy::doc_lazy_continuation at lib.rs:50841-50843: `+ logs, ...` interpreted as a doc list item without indentation.
  3. clippy::too_many_arguments at lib.rs:56218: `dispatch_choices_present` has 8 args.

## After state

- `AGENT_INTROSPECT_ARGS` `--show` summary updated to enumerate all four sections.
- `dispatch_agent_introspect` accepts `tools` in addition to existing sections.
- New `derive_tools_attribution(composed_servers, profiles)` helper:
  symmetric to `derive_hooks_attribution` from bd-4b2a82. Returns
  `Vec<(server_name, Vec<source_mixin>)>` preserving composed order.
- `--show tools` rendering:
  - text: padded `server-name <- mix-a, mix-b` lines.
  - json: `{ agent_id, profile_chain, mcp_servers: { name: { source_mixins: [...] } } }` (nested object so future server metadata can grow without breaking the schema).

## Inline broken-on-main absorbed (bd-cba8b4 filed)

- Routed `operator-actions list` arm to `dispatch_bd_operator_actions`.
- Replaced `+` with `and` to break markdown-list-item interpretation in `dispatch_fleet_disk` doc comment.
- Added `#[allow(clippy::too_many_arguments)]` to `dispatch_choices_present`.

## Tests

- New: `derive_tools_attribution_attributes_servers_to_source_mixins` — locks per-server source-mixin attribution + ordering across two fake profiles.
- New: `derive_tools_attribution_empty_servers_returns_empty` — locks the trivial path.

## Validation

- `cargo test -p caco-cli --lib derive_tools_attribution`: 2/2 PASS.
- `cargo test-small`: 4257+ tests across 8 binaries, 0 failures.
- `cargo clippy -p caco-cli -p caco-daemon -p caco-beads -p caco-web -p caco-profile --all-targets -- -D warnings`: clean.

## Diff summary

```
crates/caco-cli/src/lib.rs                       | ~+150 / -10
.cacophony/agent/.../summary/0021                | (new)
```

## Operator-takeaway

```bash
caco agent introspect --id <id> --show tools
# mcp_servers (composed) <- source-mixin:
#   github      <- claude-code-default, dev
#   bead-store  <- caco-dev

caco agent introspect --id <id> --show tools --json | \
  jq '.mcp_servers | to_entries[] | select(.value.source_mixins | length > 1)'
  # find servers contributed by multiple mixins (potential override hotspot)
```

Introspect family complete: profile, hooks, env, tools — every operator-relevant Profile section has a `--show` view. Future expansions (memory, banned_modes, etc.) can be filed individually.

## Coordination

- Spoke claim with planned scope.
- Filed bd-cba8b4 retro for the 3 broken-on-main issues.
- Will speak completion + reintegrate.

## Notes for next time

- The introspect family pattern is clean enough I could write a generic `derive_attribution<T>(composed: &[T], extract: impl Fn(&Profile) -> &[T])` if a 4th attribution helper appears. Not worth it for 2 (hooks, tools).
- `dispatch_operator_actions_list` was deleted+replaced **between my local rebase** and another agent's reintegrate. Race-condition pattern: a function-rename on main can leave call sites in unrelated dispatchers stale. Watch for this on future rebases — `cargo check -p caco-cli` after every rebase is cheap insurance.
- msd-2 sent a useful bead-close hint (`--validate-on-main false` for stuck closes from bd-845653 era). My closes succeeded normally so I didn't need it; logged in toolkit.
