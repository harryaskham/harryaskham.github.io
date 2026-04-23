# Session summary — bd-61c620 Failed-state checkouts reclaimable by prune

## Goal

Make `caco agent prune --include-discarded` actually free the disk
held by Failed-state agent checkouts, so the 63 GiB stranded on
ms-mac (12 Failed agents, largest 26 GiB) can be reclaimed without
manual `rm -rf`.

## Bead(s)

- `bd-61c620` — caco agent prune --include-discarded ignores Failed-state checkouts — 63 GiB stranded on ms-mac

## Before state

- bd-343e4f (this agent, prior cycle) extended `dispatch_agent_prune` to bring `Failed` into the include-discarded set so the planner *would* see it.
- But the planner saw it and immediately re-protected it: `dispatch_agent_prune` protects every checkout whose bead is non-Closed/Deleted, regardless of agent state. Failed agents typically keep an open / in-progress bead because another agent is expected to retry the work on a fresh checkout, so all 12 Failed agents on ms-mac were marked protected and the dry-run reported `0 candidates / 0 B would free`.
- Failing tests: bd-c19193 (pre-existing, unrelated).

## After state

- New helper `should_protect_terminal_checkout_from_prune(state, bead_status) -> bool` codifies the protection rule per terminal state:
  - `Failed` → never protected. The run ended without success and no recovery remains in the failed checkout itself; the bead's open status protects future work, not the dead target/ tree.
  - `Completed` → protected when bead is open / in_progress / draft / permanent / unreachable; eligible only when bead is closed / deleted.
  - `Stopped` / `Discarded` → same semantics as Completed.
- `dispatch_agent_prune` calls the helper instead of inlining the predicate, keeping the call site small and the rule unit-testable.
- Failing tests: bd-c19193 (unchanged, pre-existing).

## Diff summary

- Commit: `1e86c2da bd-61c620: caco agent prune --include-discarded must reclaim Failed-state checkouts`
- Files touched: `crates/caco-cli/src/lib.rs` (+134 / -10).
- Tests: +7 / -0 / flipped 0
  - `bd_61c620_failed_state_never_protected_by_open_bead`
  - `bd_61c620_failed_state_never_protected_by_in_progress_bead`
  - `bd_61c620_failed_state_never_protected_when_bead_unreachable`
  - `bd_61c620_completed_state_protected_by_open_bead` (regression guard for Completed semantics)
  - `bd_61c620_completed_state_not_protected_by_closed_bead`
  - `bd_61c620_completed_state_protected_when_bead_unreachable`
  - `bd_61c620_stopped_and_discarded_follow_completed_semantics`
- Behavioural delta: with this commit landed, `caco agent prune --project cacophony --include-discarded` will surface Failed-state checkouts as eligible targets for the retention planner. The planner still applies hours/count/bytes policy thresholds, so Failed agents younger than the configured retention window remain untouched.

## Operator-takeaway

Reclaim the stranded 63 GiB on ms-mac with:

```
caco agent prune --project cacophony --include-discarded
```

(Drop `--dry-run` once the operator has eyeballed the dry-run output
which now also lists everything the planner declined to consider per
bd-343e4f.) Failed agents younger than the project's `hours`
retention threshold will still be kept; the policy boundary is
honoured. If the rolling restraint is too aggressive, tighten
retention via the per-project policy stanza; this fix only removes
the all-or-nothing gate that previously hid every Failed checkout
behind its still-open bead.
