# Session summary — hoist test wait helpers to tests/common (bd-e7ea4b)

## Goal

Eliminate the duplicated `wait_for_canonical_checkout`
helper across `acceptance_agent.rs` and `integration_tui.rs`
by hoisting to `crates/caco/tests/common/mod.rs` per the
canonical Rust integration-test sharing pattern.

## Bead(s)

- `bd-e7ea4b` — own follow-up to bd-32229d. Closed.

## Before state

- `wait_for_canonical_checkout` defined privately in two
  test files (identical bodies) — primed for a third copy
  the next time test-speedup work touched another file.
- `wait_until` / `wait_until_with_interval` private to
  `acceptance_logs.rs` only.

## After state

- `crates/caco/tests/common/mod.rs` exposes `wait_until`,
  `wait_until_with_interval`, and
  `wait_for_canonical_checkout` as `pub fn`.
- Both `acceptance_agent.rs` and `integration_tui.rs` now
  declare `mod common;` + `use common::wait_for_canonical_checkout;`
  and the duplicate private fns are deleted.
- `acceptance_logs.rs` left as-is for now (its private
  `wait_until` works; migrating it is an obvious next
  cleanup but no urgent caller).

## Diff summary

- Files touched (+85 / −38):
  - `crates/caco/tests/common/mod.rs`: NEW shared module
    with 3 `pub fn` helpers (~70 lines).
  - `crates/caco/tests/acceptance_agent.rs`: `mod common;`
    + delete duplicate fn.
  - `crates/caco/tests/integration_tui.rs`: same.

## Verification

- `cargo build --tests -p caco`: clean.
- The first build attempt failed with E0255 because I left
  the local fns in alongside the import — fixed and
  re-built clean.

## Operator-takeaway

Future test-speedup work has a shared `wait_until` /
`wait_for_canonical_checkout` to reach for instead of
copy-pasting. If a third file ever needs them, the import
is one line; no third private copy.

Also: hit ENOSPC mid-build (winmini disk 100% full,
P1 bd-a167d6 manifesting on this node). Ran `cargo clean`
to recover 31.8GB; spoke escalation. Resumed bd-e7ea4b
build successfully.
