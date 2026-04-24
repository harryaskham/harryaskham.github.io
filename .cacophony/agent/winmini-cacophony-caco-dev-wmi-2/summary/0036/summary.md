# Session summary — bd-8a4cf2 prune stale claimed beads on snapshot

## Goal

Fix the TUI agent-detail “Claimed Beads” panel so it stops showing beads as still claimed/in-progress after they have actually closed on the board but disappeared from the trimmed snapshot. The immediate symptom was a stale claimed-bead row surviving after close because the local bead cache kept the old assignee/status around.

## Bead(s)

- `bd-8a4cf2` — TUI agent 'Claimed Beads' panel surfaces closed beads with stale `in_progress` status when assignee field is still populated post-close

## Before state

- `TuiState::beads_for_agent(...)` already filtered out `status == "closed"`, so the panel query itself looked fine on paper.
- The real issue was snapshot hydration:
  - `apply_snapshot(...)` inserts the daemon’s trimmed bead set into `self.beads`
  - but it did not retire previously-claimed local bead entries that disappeared from a fresh snapshot
- Because the daemon trims closed beads off the snapshot payload, a claimed bead that closed without the local UI seeing the exact close event could linger in `self.beads` with stale `in_progress` state and still appear under “Claimed Beads”.

## After state

- `apply_snapshot(...)` now records the set of bead IDs present in the fresh snapshot.
- After hydrating snapshot beads, it prunes any locally cached bead that:
  - is absent from the fresh snapshot, and
  - still has a non-`None` assignee
- Matching `bead_meta` entries are removed alongside those stale claimed beads.
- This keeps the fix narrow:
  - it specifically targets stale claimed-bead leakage
  - it does not try to redesign the full trimmed-bead cache lifecycle for all absent beads
- The claimed-beads panel now stops surfacing claimed rows that no longer exist in the live trimmed snapshot.

## Diff summary

- Commit: `4192b63d0` — `bd-8a4cf2: prune stale claimed beads on snapshot`
- Files touched:
  - `crates/caco-tui/src/state/mod.rs`
  - `crates/caco-tui/src/state/tests.rs`
- Diff vs current `origin/main`:
  - `crates/caco-tui/src/state/mod.rs` — +22
  - `crates/caco-tui/src/state/tests.rs` — +68
- Behavioural delta:
  - stale claimed beads that vanish from the refreshed trimmed snapshot are removed from local TUI state
  - the claimed-beads panel no longer keeps showing them just because an old assignee field lingered locally
- Validation:
  - `cargo build -p caco-tui`
  - `cargo clippy -p caco-tui --all-targets --no-deps -- -D warnings`
  - `cargo test -p caco-tui state::tests::apply_snapshot_prunes_stale_claimed_beads_bd_8a4cf2 -- --exact --nocapture`
  - `cargo test -p caco-tui state::tests::beads_for_agent_returns_matching_beads -- --exact --nocapture`
  - `cargo test -p caco-tui state::tests::beads_for_agent_excludes_closed -- --exact --nocapture`

## Operator-takeaway

This was a genuine status-display lie in the TUI, not just cosmetic wording drift. The fix makes the claimed-beads panel trustworthy again by pruning claimed rows that no longer exist in the fresh trimmed snapshot, without taking on a broader redesign of the bead cache model.