# Session summary — ssh and mosh missing-node wording

## Goal

This session polished two remaining old usage-style CLI errors: `caco ssh` and `caco mosh` with no node argument.

## Bead(s)

- `bd-7ee8fd` — [CLI polish] ssh and mosh missing-node discoverability wording

## Before state

- Failing tests: no exact regression covered missing-node wording for `caco ssh` or `caco mosh`.
- Relevant metrics: both commands emitted old multi-line `usage: caco ...` errors ending with `A node name is required.`
- Context: recent CLI polish beads established canonical required-argument wording plus discovery pointers.

## After state

- Failing tests: none in scoped validation before replay.
- Relevant metrics: `cargo test -p caco-cli ssh_and_mosh_missing_node_use_discoverability_pointer --lib`, `cargo check -p caco-cli --lib`, and `cargo test-small` passed before replay; focused regression is rerun after replay.
- Context: both commands now name the missing node positional and point to `caco node list` to discover configured nodes.

## Diff summary

- Commits: `48a0edcf3`
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: added exact regression `ssh_and_mosh_missing_node_use_discoverability_pointer`.
- Behavioural delta: missing-node ssh/mosh invocations now follow the same discoverable error style as `caco node show` and the outbox missing-id surfaces.

## Operator-takeaway

The SSH convenience commands no longer leave users at a generic usage block; they now tell users what is missing and how to find valid node names.
