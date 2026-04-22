# Session summary — bd-845653 reintegration commit-message bead-ID harvest

## Goal

Fix the reintegration squash-commit footer so it lists the bead IDs
the agent actually worked on, not arbitrary historical example IDs
scraped from the agent's multi-KB instructions blob. Earlier this
session, my own bd-58ff27 narrator change reintegrated cleanly into
main (commit d18e3cba) but the squash footer listed
`Beads: bd-e91436, bd-3b2e6f, bd-9bc19f, bd-caed3e, bd-ae8de9,
bd-702d5f` — six IDs from boilerplate goal text — and omitted
bd-58ff27 entirely. The downstream `caco bd close --bead-id
bd-58ff27` then refused with "mainline validation against
origin/main failed (bead id not found in the last 1000 commits)".

## Bead(s)

- `bd-845653` — Reintegrate squash commit message footer omits the
  bead ID present in the agent commit subject (bd-58ff27 lost)

## Before state

- `crates/caco-daemon/src/reintegration.rs::build_reintegration_commit_message`
  exclusively called `extract_bead_details_from_goal(agent.goal)`
  which scrapes the entire goal text for `bd-XXXX` tokens.
- For ephemeral / single-bead workers whose goal is a focused bead
  description this works fine. For persistent agents the goal is
  `"Persistent Project agent 'caco-dev-msd-2' (persistent-431f063e)."`
  followed by ~50KB of profile and tool-surface instructions that
  reference many example bead IDs across the document. All of those
  example IDs end up in the squash footer with no relation to the
  work being merged.
- `caco-daemon` reintegration tests: 109 passing pre-change.

## After state

- New pure helper
  `extract_bead_ids_from_commit_messages(text: &str) -> Vec<String>`:
  scans an arbitrary blob for `bd-XXXX` tokens (≥2 hex chars) at
  word boundaries, deduped, in first-seen order. Word-boundary
  guard rejects `xbd-abc` substring matches.
- New runner `harvest_branch_bead_ids(checkout, target, agent_branch)`
  that runs `git log --format=%B {target}..{agent_branch}` in the
  canonical reintegration checkout and applies the pure helper.
  Non-fatal on any git error: returns empty vec so legacy
  goal-only behaviour still kicks in for callers / paths where the
  branch is unavailable.
- New `build_reintegration_commit_message_with_branch_ids(...,
  branch_bead_ids: Option<&[String]>)`: when branch IDs are present
  and non-empty, treats the branch-commit list as authoritative.
  Goal-extracted IDs are kept ONLY when they intersect with branch
  IDs (so their rich title/description payload is preserved).
  Branch-only IDs are appended as bare entries (no payload).
  Goal-noise IDs that are absent from branch commits are dropped.
  When branch IDs are `None` or `Some(empty)`, falls back to the
  original goal-text behaviour (back-compat / non-regression).
- The legacy `build_reintegration_commit_message` wrapper is
  retained as a thin shim used only by tests, gated
  `#[cfg_attr(not(test), allow(dead_code))]`.
- Wired at the only production callsite in `finalize_direct_merge`
  (line ~1764): `harvest_branch_bead_ids(checkout, target,
  agent_branch)` is invoked immediately before commit-message
  assembly, then passed as `Some(&branch_bead_ids)` to the new
  builder.

## Files touched

- `crates/caco-daemon/src/reintegration.rs` (+313 / -2 lines):
  the new helpers, the new builder variant, the wired callsite,
  and 8 new unit tests at the end of the existing test module.

## Diff summary

Single-file Rust change in `crates/caco-daemon/src/reintegration.rs`.
Adds two pure helpers (`extract_bead_ids_from_commit_messages` and
the IO-side `harvest_branch_bead_ids`), a new commit-message
builder variant `build_reintegration_commit_message_with_branch_ids`
that takes an optional branch-bead-ID slice, retains the legacy
no-branch wrapper as a thin test shim under
`#[cfg_attr(not(test), allow(dead_code))]`, and wires the harvester
into `finalize_direct_merge` immediately before commit assembly.
8 new unit tests cover the harvester's first-seen-dedup ordering
and word-boundary discipline, the builder's goal-noise filter
(the regression case), the goal-branch intersection that preserves
rich payload, both `None` and `Some(empty)` legacy fallbacks, and
branch-only-ID emission with bare payload. No schema changes, no
public API changes outside this module, no migration required.

## Operator-takeaway

Future reintegrations of persistent-agent branches will produce
squash-commit footers that list the bead IDs the agent actually
committed against (harvested from the agent branch's own commit
messages), not the noise IDs that previously leaked from the goal-
text scrape. The downstream `caco bd close --bead-id <id>` mainline
validation will therefore find the right bead and accept the close.
Ephemeral workers whose goal already names the right bead see no
change in behaviour — the goal-derived rich title/description is
still preserved when the goal and branch agree. There is no
configuration to flip and no runtime knob: the new behaviour is
always-on for the direct-mode path.

## Validation

- `cargo test -p caco-daemon --lib reintegration::tests::` →
  117/117 PASS (109 pre-existing + 8 new bd-845653 tests).
- `cargo clippy -p caco-daemon --all-targets -- -D warnings` →
  clean (1m10s).
- `cargo test-small` → 718+286+18+2806+51 PASS green.
- Did not run full workspace tests per merge-queue mixin.

## Notes / follow-ups

- bd-58ff27 (the narrator profile change that surfaced this bug) is
  on main as commit d18e3cba but cannot close until either (a)
  operator runs `caco bd close --admin-override` against it, or
  (b) the bead ID is referenced by a future merge-commit footer
  (e.g. this very reintegration, since the commit message above
  references bd-58ff27 in the bug-context paragraph and so the new
  harvester will pick it up). Worth confirming bd-58ff27 closes
  cleanly after this lands.
- The recorded mode path (artefact-only commits without a main
  merge) was not modified. If artefact-only paths exhibit the same
  noise-ID leak, a follow-up could share the harvester there.
- Test reintegrations earlier in this session: bd-eef5da (raced
  by ms-mac mab6fpzek734d79t under bd-34d0b8, dropped) and bd-58ff27
  (landed on main as d18e3cba, bead-close blocked by exactly the
  bug this bead fixes).
