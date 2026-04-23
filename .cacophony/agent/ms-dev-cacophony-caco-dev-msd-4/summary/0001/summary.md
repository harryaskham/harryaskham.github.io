# Session summary — bd-343e4f caco agent prune visibility for excluded candidates

## Goal

Surface the "invisible to retention" cohort in `caco agent prune --dry-run`
so operators can discover terminal checkouts that fall outside the
planner's view without manually `du -sh`-ing agent dirs.

## Bead(s)

- `bd-343e4f` — caco agent prune: surface invisible-to-policy candidates in dry-run output

## Before state

- Failing tests: bd-c19193 (pre-existing, unrelated).
- `caco agent prune --dry-run` printed the targets it *would* prune plus a `protected` count, but said nothing about terminal checkouts the planner declined to consider. Reasons for declining included `pruned == true`, non-completed terminal states (Stopped / Failed / Discarded) without `--include-discarded`, and `--project` filtering. Each was a distinct silent exclusion.
- The bead reporter spent real debugging time discovering that ~3.6 GiB of stopped/failed checkouts during bd-ff753c sat outside the planner's view because the only way to find them was source-reading.

## After state

- Failing tests: bd-c19193 (unchanged, pre-existing).
- `dispatch_agent_prune` computes an `excluded` cohort from the full agent inventory before applying the retention filter, grouping by reason: `already-pruned`, `state=<stopped|failed|discarded> (rerun with --include-discarded to consider)`, or `filtered-by-project`. Each entry carries id, project, state, and bytes-on-disk.
- Text output gets a new "Excluded from retention" heading section in `--dry-run` mode, sorted by reason for deterministic diffing, with a per-reason summary line and per-id detail underneath. Production runs stay quiet to avoid noise on routine sweeps.
- JSON output always carries `excluded`, `excluded_total_bytes`, and `excluded_total_human` so machine-readable callers can poll without flipping `--dry-run`.
- Live (non-terminal) agents are intentionally excluded from the visibility list because they are unambiguously not prune candidates; the section is scoped to terminal records only.

## Diff summary

- Commits: `70c8093f bd-343e4f: caco agent prune --dry-run surfaces excluded-from-retention candidates`
- Files touched: `crates/caco-cli/src/lib.rs` (+135 / -2 — new `ExcludedEntry` struct, exclusion-classifier loop, JSON additions, text-output section).
- Tests: 0 new (the function lacks a unit-test surface today; the bead followup could add one). `cargo test-small` passes; `cargo clippy -p caco-cli` clean.
- Behavioural delta: `caco agent prune --dry-run` now prints an additional section when there are terminal candidates the planner declined to consider; `caco agent prune --dry-run --json` always carries the new fields. No change to the actual retention-planning behaviour or to non-dry-run runs (text section is `--dry-run` gated).

## Operator-takeaway

This is a pure visibility win — the planner's behaviour is unchanged.
Operators debugging "why is this checkout still here?" can now run
`caco agent prune --dry-run --include-discarded=false` and see the
exact reason each terminal checkout sits outside the policy view,
along with the recommended escape hatch in the reason string. JSON
consumers (TUI, scripts) get the same data without any flag flip.
A small followup could land a unit test for `dispatch_agent_prune`
that constructs a fixture inventory and asserts the excluded
classification behaviour — the function lacked one before this
change, so the new behaviour is also untested at the unit level.
