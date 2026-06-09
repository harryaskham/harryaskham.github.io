# Session summary — TUI agent groups navigation

## Goal

Implement the TUI display slice for agent groups so project navigation can show `Agents > Groups > <group>` using the first-party group configuration/read model, without taking over config-schema semantics or group-scoped chat delivery.

## Bead(s)

- `bd-b87cdd` — Display agent groups in TUI under Agents > Groups

## Before state

- The active TUI navigation model (`crates/caco-tui/src/nav.rs`) exposed `Agents`, `Persistent`, visiting agents, status groups, and machine groups, but had no `Groups` subsection.
- `caco-config` already defined cluster/project agent-group maps and project-level override semantics from adjacent work, but the daemon UI snapshot and TUI state did not carry resolved project-visible groups.
- Group chat fanout and mobile group UX were owned by separate beads, so this slice needed to stay display/navigation-only.

## After state

- The daemon UI snapshot now includes each project's resolved `agent_groups` map, using top-level `agents.groups` overlaid by `projects.<name>.agents.groups`.
- TUI state and nav-tree project data carry that group map into navigation rebuilds.
- Project navigation renders `Agents > Groups > <group>` with a `Chat` affordance, resolved live-agent rows, resolved persistent-declaration rows, and explicit unresolved-member rows for configured IDs that are not currently in the agent inventory.
- The group Chat row currently opens the existing project chat pane, preserving a stable navigation affordance while group-scoped chat semantics remain owned by the separate group-chat slice.

## Diff summary

- Code/content commits: `e076b62e2` (local post-final-rebase commit), final landed squash SHA pending reintegration receipt
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched: `crates/caco-daemon/src/ui_stream.rs`, `crates/caco-tui/src/app.rs`, `crates/caco-tui/src/app/benchmark_support.rs`, `crates/caco-tui/src/nav.rs`, `crates/caco-tui/src/state/mod.rs`, `crates/caco-tui/src/state/tests.rs`, `crates/caco-tui/src/views/nav_tree.rs`, plus test fixture literals in adjacent TUI view modules that construct `ProjectState` / `ProjectSnapshot`.
- Tests: added focused nav-tree coverage for `Agents > Groups` rendering, member resolution, unresolved-member visibility, and current group-chat pane mapping.
- Validation: `tj-5534893e` passed the focused nav test after final rebase; `tj-d10046db` passed `cargo check -p caco-tui -p caco-daemon --lib`; `git diff --check` passed. `crates/caco-tui/src/app.rs` remains intentionally unformatted because its HEAD baseline has pre-existing rustfmt drift, and the narrow one-line change there was kept without broad churn.

## Operator-takeaway

The TUI now has a concrete first-party navigation shape for configured agent groups while keeping semantics clean: group membership is read-only display data, live/persistent members open existing detail surfaces, unresolved configured members remain visible for debugging, and future group chat delivery can attach to the existing `Groups > <name> > Chat` row without changing the tree contract.
