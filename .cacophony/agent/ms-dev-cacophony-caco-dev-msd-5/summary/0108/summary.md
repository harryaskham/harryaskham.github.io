# Session summary — code-enforce the --skip-hooks match/enum gap (bd-ade262)

## Goal

`caco agent reintegrate --skip-hooks` skips the cacophony fast-test gate
(cargo check/test-small/clippy). That gate's cargo exhaustiveness check is the
only thing that catches the bd-346e5d class: a change that adds a `match` over an
extensible enum (HostMessage/HostRequest/…) becoming non-exhaustive against a
variant added concurrently on `main` — exactly the v1.2.1330 broken-on-main.
This bead code-enforces that `--skip-hooks` cannot be used for match/enum changes.

## Bead(s)

- `bd-ade262` — [reint-reliability] Code-enforce --skip-hooks post-rebase
  recompile + match/enum scoping (Part 1, the PRIMARY match/enum-refuse guard).

## Before state

- Failing tests: none.
- The `if skip_hooks {` branch in `dispatch_agent_reintegrate`
  (crates/caco-cli/src/lib.rs) required only a non-empty `--reason`, then warned,
  emitted `reintegration_hooks_skipped`, and skipped the gate — for ANY diff,
  including a match/enum change that could land non-exhaustive on the merged tip.

## After state

- Failing tests: none.
- `--skip-hooks` is now REFUSED (clear error, force `--mode direct`) when the
  agent diff adds/modifies a `match` or an `enum`. The operator `--reason`
  escape hatch survives for genuinely logic-free (docs/shell) changes.
- Record shape of `reintegration_hooks_skipped` is unchanged (the guard runs
  before the skip; bd-d3e519's distinct auto-skip event stays coherent).

## Diff summary

- File: `crates/caco-cli/src/lib.rs` only (no reintegration.rs).
- Added pure fns `diff_line_contains_word`, `skip_hooks_block_reason_for_diff`
  (scans ADDED diff lines + hunk-header context for `match`/`enum`, word-boundary
  so `enumerate`/`rematch` don't false-match), `skip_hooks_match_enum_block_reason`
  (computes the agent diff vs the target, best-effort fail-open), and the refusal
  guard in the skip-hooks branch.
- Tests: +3 (refuses_added_match, refuses_added_enum_variant, allows_logic_free_diff) — all pass.
- Final landed squash SHA from the reintegration receipt.

## Operator-takeaway

`--skip-hooks` can no longer bypass the gate for a match/enum change — the exact
class that broke v1.2.1330. Parts 2 (post-rebase cargo check for non-exempt
skip-hooks) + 3 (bd-fd2c76 logic-free exemption) from msd-1's plan remain as the
secondary gap-closer; this lands the PRIMARY guard first per the sequencing.
