# Session summary — skip permanent trackers in auto-claim

## Goal

Fix the live auto-claim regression observed during burn-down where a beadless claim could assign a permanent workspace tracker instead of returning no ready implementation work.

## Bead(s)

- `bd-0755d1` — [bug] beadless claim can assign permanent workspace tracker

## Before state

- Failing tests: none known for this bead; live CLI behaviour assigned `bd-5bfb2c`, a permanent workspace tracker, from `caco bd claim --project cacophony` when no ready implementation beads were available.
- Relevant metrics: targeted regression did not yet cover permanent intent encoded as a title prefix or label.
- Context: `claim_next_ready` already skipped `status = permanent`, but the live tracker was still selected, indicating permanent tracker semantics can exist in other persisted shapes.

## After state

- Failing tests: none in the targeted regression or timed small suite.
- Relevant metrics: `timeout 120 cargo test -p caco-beads claim_next_ready_skips_permanent_beads -- --nocapture` passed; `timeout 180 cargo test-small` passed.
- Context: beadless auto-claim now skips permanent-status beads, `[PERMANENT]` title-prefixed trackers, and beads labeled `permanent` before attempting assignment.

## Diff summary

- Commits: a3cfa9258
- Files touched: `crates/caco-beads/src/store.rs`
- Tests: expanded `claim_next_ready_skips_permanent_beads` to cover status, title-prefix, and label-shaped permanent trackers.
- Behavioural delta: no-id auto-claim no longer assigns permanent tracker beads that are represented as open records with permanent intent encoded outside the status field.

## Operator-takeaway

The burn-down loop exposed a real queue hygiene issue: permanent tracker records could still leak into worker auto-claim. The server-side resolver now filters those tracker shapes before ownership changes, so idle workers should stop bouncing off the workspace umbrella bead.
