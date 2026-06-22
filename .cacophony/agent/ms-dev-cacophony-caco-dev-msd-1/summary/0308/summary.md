# Session summary — bd-0a9f7c spawn-and-claim assignee read-lag (auto-claim collision fix)

## Goal
Overnight backlog burn (Harry's directive): close the spawn-and-claim assignee-propagation gap.
When a bead is dispatched via spawn-and-claim, the agent runs it, but the bead's assignee can read
None for a window (cross-node/proxy lag) — during which no-ID auto-claim could DOUBLE-STAFF it. The
bead's own recommended FIX is to "surface the spawn-and-claim claim in the ready-filter so auto-claim
skips it" — implemented here.

## Bead(s)
- bd-0a9f7c (in_progress, mine). Daemon-side collision fix landed here.
- Cross-ref: the existing bd-2fbeb6 remote-live-agent ready-exclusion (which deliberately skips the
  LOCAL node — the specific gap this closes).

## Before state
The no-ID auto-claim path (`claim_next_ready_excluding`) excluded only explicit `--exclude-bead-id`
+ recently-rejected beads — NOT live-agent beads. And `collect_remote_live_agent_bead_ids` (the
ready-LIST exclusion) skips the local node. So a spawn-and-claim agent running on THIS (beads-host)
node, whose assignee read lags to None, left its bead in the auto-claimable set: only the 409
claim-contest backstop prevented the double-staff.

## After state (daemon-side: collision + primary ready surface)
- `live_agent_excluded_bead_id(state, bead_id)` (pure): returns the bead id when an agent is
  non-terminal and owns a non-empty bead (mirrors `find_active_agent_for_bead`'s live predicate).
- `collect_local_live_agent_bead_ids(state)`: live (non-terminal) LOCAL agents' bead ids from the
  authoritative IN-MEMORY agent manager (which does not lag like the cross-node assignee read).
- no-ID `claim_bead`: adds LOCAL + REMOTE live-agent bead ids to `exclude_ids`, so
  `claim_next_ready_excluding` skips a bead a running agent already owns even when its assignee read
  is stale — closing the double-staff window robustly (no longer 409-backstop-only).
- `handle_list_beads` (`--ready`): the existing remote-live exclusion now also unions the local-live
  set, so a locally-running spawn-and-claim bead is not surfaced as ready while its assignee lags.
- 2 unit tests for the pure predicate (running/starting/stalled+bead → excluded; terminal /
  no-bead / empty-bead → not). caco-daemon compiles clean.
- Observational w.r.t. claim correctness: it only ADDS exclusions for actively-worked beads; a
  terminal agent's bead is never excluded (stays claimable).

## Diff summary
- crates/caco-daemon/src/beads.rs: `live_agent_excluded_bead_id` + `collect_local_live_agent_bead_ids`;
  the no-ID `claim_bead` live-agent exclusion; the `handle_list_beads` `--ready` union; 2 unit tests.
(Final landed squash SHA: see the reintegration receipt.)

## Operator takeaway
A just-spawned spawn-and-claim bead can no longer be double-staffed by no-ID auto-claim while its
cross-node assignee read lags — the daemon now cross-references the live (local + remote) agent's
bead_id, which doesn't lag. RESIDUAL (focused follow-on, filed separately): the FALSE-STALL — the
router/ctrl/ops monitors that read the bead's assignee directly (the bd-d79bfd scare) need the same
running-agent cross-reference in THEIR logic (non-daemon: router/ctrl profiles + caco-cli ops), plus
the 2 aggregate daemon list surfaces (handle_all_beads / handle_beads_status) for full consistency.
