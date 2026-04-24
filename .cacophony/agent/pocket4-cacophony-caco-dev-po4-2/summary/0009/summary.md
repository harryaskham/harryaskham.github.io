# Session summary — fix broken-on-main `bd0502bc_node_disk_rejects_non_local_node`

## Goal

Unbreak the `caco-cli` broken-on-main regression where the unit test
`bd0502bc_node_disk_rejects_non_local_node` stack-overflowed before its
assertions ran. The failure was in test execution / stack budget, not
in the user-facing `dispatch_fleet_disk` contract itself.

## Bead(s)

- `bd-d52923` — `[broken-on-main] bd0502bc_node_disk_rejects_non_local_node failing`

## Before state

- Exact repro on default stack:
  - `cargo test -p caco-cli --lib bd0502bc_node_disk_rejects_non_local_node -- --nocapture`
  - crashed with `thread ... has overflowed its stack`
- Same test passed with a larger stack (`RUST_MIN_STACK=33554432`),
  indicating a pre-existing stack-budget issue rather than a semantic
  regression in the node-disk dispatcher.
- Existing caco-cli test suite already had precedent for this pattern:
  other deep dispatch/help tests run inside `thread::Builder` with
  `RUN_DISPATCH_STACK_SIZE`.

## After state

- `bd0502bc_node_disk_rejects_non_local_node` now runs inside a
  `thread::Builder` using `super::RUN_DISPATCH_STACK_SIZE`, matching
  the established caco-cli convention for deep dispatch tests.
- The exact repro now passes on the default stack.
- `cargo test-small` green: 190 / 190.

## Diff summary

- File touched:
  - `crates/caco-cli/src/lib.rs`
- Tests run:
  - `cargo test -p caco-cli --lib bd0502bc_node_disk_rejects_non_local_node -- --nocapture`
  - `cargo test-small`
- Behavioural delta:
  - no change to CLI/runtime behavior
  - broken-on-main test no longer aborts the suite via stack overflow

## Operator-takeaway

This was a test-harness stack-budget fix, not a product-behavior fix.
The node-disk guard contract was already correct; the regression was
that this one dispatch-heavy test had not yet been moved onto the same
large-stack wrapper pattern that other caco-cli deep-dispatch tests
already use. That pattern is now consistent here too.
