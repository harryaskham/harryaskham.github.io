# Session summary — bd-45d1f7 missing argument guidance

## Goal
Bring several terse CLI missing-required-argument errors in line with the newer discoverability template that tells operators which sibling `list` command to run next.

## Bead(s)

- `bd-45d1f7` — caco missing-required-arg error template inconsistent across CLI subtrees

## Before state

- `caco action run`, `caco cron run`, `caco notify get`, `caco scratch show`, agent id resolution, and `caco bd info` had missing-argument messages that lacked a next-step list command or alias detail.
- The release-log and related fixes had established the expected pattern but these sibling surfaces still drifted.

## After state

- `action run` now points to `caco action list`.
- `cron run` now points to `caco cron list`.
- `notify get` now accepts/declares `--notification-id` as an alias and points to `caco notify list`.
- `scratch show` now points to `caco scratch list`.
- Generic agent id resolution now points to `caco agent list`.
- `bd info` now has its own missing-id wording with a `caco bd list` pointer instead of reusing `bd show` wording.

## Diff summary

- Commits: `979aaec22`.
- Files touched: `crates/caco-cli/src/lib.rs`, `crates/caco-cli/src/scratch_cmd.rs`.
- Tests: added `bd_45d1f7` unit coverage for action/bd info guidance and notify alias registration.
- Validation: `cargo test -p caco-cli bd_45d1f7 --lib`; `cargo clippy -p caco-cli --all-targets -- -D warnings`; `cargo check --workspace --tests`.

## Operator-takeaway

A set of low-friction CLI errors now tell the operator exactly what to run next, reducing dead-end “X is required” messages across action, cron, notify, scratch, agent, and bd info surfaces.
