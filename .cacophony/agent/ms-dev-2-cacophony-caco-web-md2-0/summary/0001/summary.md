# Session summary — caco-web: snapshot-failure empty states no longer mislead (bd-0557e9)

## Goal

During a caco-web duty-cycle observation pass of the dashboard's backend-unavailable
behaviour, I found that when the initial daemon snapshot fails to load, the main
inventory views rendered *healthy-looking* empty states — "No nodes configured",
"No agents match the filters" — that contradict the outage and (for "No nodes
configured") imply a configuration problem rather than a connectivity/auth one.
This session makes those views show an explicit snapshot-unavailable state instead,
closing the per-view half of the bd-b6ab99 auth/scope-drift class.

## Bead(s)

- `bd-0557e9` — caco-web: inventory views show misleading healthy-empty states when the initial snapshot fails on the auth/forbidden (401/403) path (filed + claimed + fixed this session)
- (pattern parent: `bd-b6ab99` — token-scope/auth drift surfacing only as reconnect churn)
- reflect-session draft filed: `renderActiveView()` helper gap (failure paths don't re-render the active view)

## Before state

- Failing tests: none.
- With the initial `/api/v1/ui/snapshot` failing (no snapshot ever loaded), the
  views showed: Nodes → "No nodes configured"; Agents → "No agents match the
  filters"; the only outage signal was the transient top-right toast + the small
  red SSE dot. Evidence: `web/screenshots/before-{nodes,agents,status}-wide.png`.
- Root: `isSnapshotBackendUnavailableError` only flags httpStatus>=500 / network
  fail / timeout, so the 401/403 (`forbidden`) and other non-5xx paths left
  `state.snapshotUnavailable=false`; `isAwaitingInitialSnapshotData()` then
  returned false and the views fell through to the healthy "No X" empty state.

## After state

- Failing tests: none. `cargo test -p caco-web --lib` passes (incl. the new
  `app_js_snapshot_unavailable_on_never_loaded_failure_bd_0557e9` contract test).
- Nodes now shows "Nodes delayed / Waiting for the initial daemon snapshot before
  showing node data"; Agents shows "Active agents delayed" — both consistent with
  the "Dashboard backend unavailable — retrying…" toast. The 401/403 case shows an
  auth-specific message ("…the daemon returned an authorization error…"). Evidence:
  `web/screenshots/after-{nodes,agents}-wide.png`.

## Diff summary

- Code/content commit: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `crates/caco-web/static/app.js`: (1) in loadSnapshot()'s catch, when the
    fetch failed and no snapshot ever loaded (`!initialLoadDone && !lastSnapshotTime`),
    set `state.snapshotUnavailable=true` and re-render the active inventory view
    (nodes/agents/beads) so its empty state reflects the outage; (2) made
    `snapshotDataEmptyState` forbidden-aware (auth message); (3) made the Agents
    snapshot-unavailable branch forbidden-aware.
  - `crates/caco-web/src/tests.rs`: +1 contract test (bd-0557e9).
- Tests: +1 / -0 / flipped 0.
- Behavioural delta: snapshot-failure (auth/forbidden/non-5xx, never-loaded)
  inventory empty states now read as "unavailable/delayed/auth" instead of a
  misleading healthy "No X" state. Connection-status banners unchanged. Only the
  never-loaded-and-empty failure case is affected; stale-data-after-load behaviour
  is untouched.

## Embedded artefacts

- `web/screenshots/before-nodes-wide.png` — pre-fix: "No nodes configured" during outage.
- `web/screenshots/before-agents-wide.png` — pre-fix: "No agents match the filters" during outage.
- `web/screenshots/before-status-wide.png` — pre-fix Status with blank metric values during outage.
- `web/screenshots/after-nodes-wide.png` — post-fix: "Nodes delayed / Waiting for the initial daemon snapshot…".
- `web/screenshots/after-agents-wide.png` — post-fix: "Active agents delayed".

## Operator-takeaway

The dashboard previously told operators "No nodes configured" during a backend
outage or an auth/scope failure — which looks like a real, alarming config state,
not a connectivity problem. It now shows an explicit snapshot-unavailable (or
auth) state in the content panes, matching the outage toast, so a worker-token
403 or a daemon-down startup no longer masquerades as an empty cluster. Follow-up
(filed as a draft): there is no central `renderActiveView()` helper, so failure/
state-change paths don't consistently re-render the current view — the fix had to
add explicit per-view re-render calls in the failure catch.
