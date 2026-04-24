# Session summary — bd-140660: reject empty --stamp on bd snapshot pin/unpin

## Goal

Test-user pass on cacophony 1.2.527 found a destructive empty-string
bypass: `caco bd snapshot pin --stamp ''` silently created a stray
PINNED sidecar at the snapshots ROOT (rather than inside any
specific snapshot). Bead asks for input validation before any
sidecar write.

## Bead(s)

- **bd-140660** (P2 bug, owned).

## Before state

- `caco_beads::snapshots::pin(root, "", reason)` would compute
  `dir = snapshots_root(root).join("")` which equals
  `snapshots_root(root)` itself. `dir.is_dir()` returned true for
  the snapshots root, the existence guard passed, and `fs::write`
  landed a 0-byte `PINNED` file at the root.
- `caco bd snapshot pin --stamp ''` accepted the empty value, the
  CLI dispatcher passed it straight through to the library, the
  library wrote the stray sidecar, and exit 0 was returned.
- bd snapshot list ignored the stray sidecar (still '-' pin status
  for every snapshot), so the root-level PINNED persisted
  invisibly.

## After state

- `caco_beads::snapshots::pin` and `unpin` now return
  `io::ErrorKind::InvalidInput` up front when `stamp.is_empty()`.
  This is the authoritative library-layer guard inherited by every
  consumer (CLI, daemon, future MCP tool).
- `dispatch_bd_snapshot_pin` and `dispatch_bd_snapshot_unpin`
  reject empty `--stamp` at the CLI layer with a friendlier
  `--stamp value cannot be empty (bd-140660: ...)` error that
  carries the bead-id breadcrumb (matching the bd-7abbba
  required-flag-with-context template suggested in the bead).
- 4 new tests:
  * `pin_with_empty_stamp_returns_invalid_input_and_does_not_mutate_state`
    (asserts no stray sidecar AND asserts the error kind)
  * `unpin_with_empty_stamp_returns_invalid_input`
  * `bd_snapshot_pin_with_empty_stamp_rejected_bd140660`
  * `bd_snapshot_unpin_with_empty_stamp_rejected_bd140660`
- caco-beads snapshots: 11/11 pass. caco-cli bd_snapshot: 5/5 pass.
- Clippy clean on touched code.

## Diff summary

- Commit `72fc538c`: bd-140660: reject empty --stamp on bd snapshot
  pin/unpin.
- Files touched:
  - `crates/caco-beads/src/snapshots.rs` (~+45: empty-stamp guards
    on pin/unpin, 2 new tests).
  - `crates/caco-cli/src/lib.rs` (~+60: empty-stamp guards on the
    two dispatchers, 2 new tests).
- Tests: +4 / 0 flipped / 0 ignored.
- Behavioural delta: the destructive empty-string bypass is gone;
  exit code becomes non-zero with a clear message naming the rule
  and the bead-id.

## Operator-takeaway

Issue 1 (the destructive bug, P2 candidate) from the bd-140660
omnibus is now closed. The remaining items in the bead title are
non-destructive ergonomics or schema-shape complaints:

- `pin --json` BROKEN exit 2
- `snapshot list --limit 0 / -1 / bogus` all bd-b76723 (already
  has a tracking bead)
- `mode show --project gold-standard` "Defined:" output line
- `mode show --json gold-standard` envelope shape
- `bd snapshot list` novel pure-flat envelope (no data/meta wrap)
- 2 more required-flag-with-context patterns

Each of those is independently small and worth its own scoped
bead so they don't get lost in a 7-issue omnibus. Recommend filing
them as a follow-on cluster if the operator wants them swept.
