# Session summary — bd-308248: Move Merge Queue to Tools section

## Goal

Relocate the Merge Queue navigation entry from a top-level
sibling at depth 1 (between Tools and Beads) to a child of the
Tools collapsible group at depth 2 (alongside Audio and Prune),
matching the operator's mental model: merge-queue is a
build/release tool, not a primary navigation target.

## Bead(s)

- `bd-308248` — Move merge queue to Tools section in TUI

## Before state

- Cluster nav layout (collapsed Tools): Status, Inbox, Chat,
  Events, Tools, **Merge Queue**, Beads, Agents, ...
- `NavNode::ClusterMergeQueue` returned `depth() == 1`.
- The row was pushed in section "1.3.5 Merge Queue" between the
  Tools block and Beads, so it was always visible regardless of
  whether Tools was collapsed.

## After state

- Cluster nav layout (expanded Tools): Status, Inbox, Chat,
  Events, Tools → [Feed, Logs, Crons, Hooks, Actions, Console,
  Profiles, Audio, Prune, **Merge Queue**], Beads, Agents, ...
- `NavNode::ClusterMergeQueue` returns `depth() == 2`.
- Row is pushed inside `if !cluster_tools_collapsed { ... }` so it
  collapses with the Tools group, consistent with Audio/Prune.
- Existing structural test `tests::cluster_nav_tree_layout`
  still asserts `tree.rows[15] == ClusterMergeQueue` and now also
  pins `depth == 2`.

## Diff summary

- `crates/caco-tui/src/nav.rs`:
  - `depth()` mapping: ClusterMergeQueue 1 → 2.
  - Row push relocated from after the Tools block to inside it,
    after Prune.
  - Test updated to assert depth==2 and updated comment.
- `cargo test -p caco-tui --lib nav::` 70/70 pass.
- `cargo clippy -p caco-tui` clean (3 pre-existing warnings, none
  in the diff).

## Embedded artefacts

(none)

## Operator-takeaway

Small navigation refactor with no functional change to the
merge-queue panel itself. The bead's other acceptance criterion
("old location is removed or deprecated") is satisfied: the
depth-1 push site is replaced by a comment pointer; there is no
duplicate entry, just a relocation.

V2 sibling bead bd-b544e8 ("Add per-workspace merge queue to
TUI") will introduce a Project-section equivalent — keeping
Merge Queue under Tools at the cluster level establishes the
parent-pattern they should mirror under ProjectTools.
