# Summary 0013 — bd-e4ef0c: accept reconciled-no-op direct reintegration outcomes

## Bead
bd-e4ef0c (P3, bug, [bd-274c2d follow-up]) — `caco agent reintegrate
--mode direct` was rejecting permanent-bead clean-probe cycles with
"direct reintegration reported success but no merge commit was
recorded" when the cycle had no main delta AND no cacophony-state
artefact to commit (typically because the summary was already
published in a prior cycle, or rebase ate the squash-merged content).

## Root cause

`reintegrate_direct_on_main` returns a "reconciled no-op"
`ReintegrationOutcome { success: true, merge_commit: None,
artefact_commit: None, message: "reconciled: agent branch X had
nothing to land on Y or cacophony-state" }` in the (None, None)
match arm of the push step.

`verify_direct_outcome` already handled artefact-only outcomes
(merge_commit None + artefact_commit Some) per bd-f76c81, but had no
arm for the (None, None) case, treating it as a hard false-success.

## Fix

`crates/caco-daemon/src/reintegration.rs::verify_direct_outcome`:

When `merge_commit` is None AND `artefact_commit` is None, accept
the outcome iff `outcome.message.starts_with("reconciled:")` — the
helper's own marker for legitimate no-op reconciliation. Falls
through to the existing error path for any other (None, None)
combination, preserving the safety net for true verification
failures.

## Tests

`crates/caco-daemon/src/reintegration.rs::tests::`:

- `verify_direct_outcome_accepts_reconciled_noop_success` — happy
  path: success=true with both commits None and "reconciled:"
  message returns Ok.
- `verify_direct_outcome_still_rejects_unmarked_double_none_success`
  — regression guard: success=true with both commits None and a
  generic ("completed direct merge") message still errors. We're
  not opening the floodgates on legitimate verification failures.

## Drive-by

`crates/caco-daemon/src/ui_stream.rs`: 3 `AgentSnapshot { ... }`
test fixtures were missing the `tmux_history_limit` and
`tmux_history_size` fields added by bd-7ef076 (msd-1). They blocked
`cargo test -p caco-daemon` outright. Added explicit `None` for both
fields. (caco-tui has ~95 of the same fixture-mismatch errors but
those are msm-5's bd-b69cf3 scope and out of mine.)

## Verification

- `cargo check -p caco-daemon` — clean.
- `cargo test -p caco-daemon --lib reintegration::` — 119/119 green.
- `cargo test -p caco-daemon --lib ui_stream::tests::` — 71/71 green.
- `cargo test -p caco-daemon --lib ui_snapshot` — 5/5 (bd-c70e2e tests
  still green).
- Cannot run full `cargo test-small` because caco-tui breakage on main
  blocks workspace; targeted crate test passes.

## Operational impact

- Permanent bead cycles (bd-274c2d test-suite-health primarily) can
  now reintegrate cleanly when nothing was found to fix. Previously
  agents had to either skip reintegrate (loses summary continuity) or
  touch unrelated files to force a non-empty diff (noise).
- The structural distinction is preserved: "reconciled:" is the
  helper's machine-verifiable signal that the no-op is legitimate.
  Agent-controllable strings can't bypass verification because the
  marker is set by the daemon itself, not the agent.

## Next

Reintegrate direct, close bd-e4ef0c, idle.
