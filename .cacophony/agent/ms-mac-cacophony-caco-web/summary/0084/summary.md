# Session summary — persist web notification Clear All

## Goal

Run the caco-web active duty cycle, honor existing web queue work before filing fresh observation beads, and fix the ready notification bug found during the scan. The cycle found no assigned caco-web bead but did find unassigned ready web bug `bd-1056da`, so this chunk claimed and fixed that specific surface rather than running a generic dashboard observation.

## Bead(s)

- `bd-1056da` — Fix notification clear all button on web surface. Claimed and implemented in this chunk.
- `bd-6681ee` — Implement bidirectional terminal for read-only view. Verified closed from the previous cycle; no longer active.
- `bd-3a9c14` — Provide safe summary-index allocation helper. Draft reflection follow-up filed from this session.

## Before state

- Failing tests: none known at start. The checkout was initially behind `origin/main` by one commit and was rebased before claiming `bd-1056da`.
- Relevant metrics: duty scan found no in-progress beads assigned to this agent. Ready open scan found `bd-1056da` with labels `notifications`, `ui`, and `web`; it described Clear All removing rows locally but not persisting after refresh.
- Context: `clearAllNotifications()` only set `state.notifications = []`, re-rendered, and showed a toast. The daemon snapshot hydrates unacknowledged notifications, so rows that were never acknowledged would return on page refresh.

## After state

- Failing tests: none observed in focused validation.
- Relevant metrics: `cargo fmt --all`, `cargo test -p caco-web notifications_clear_all_persists_acknowledgements_bd_1056da --lib`, `cargo test -p caco-web notifications_nav_accessible_name_includes_unread_count_bd_4273ef --lib`, and `cargo check -p caco-web --all-targets` passed, then the focused tests/check were rerun successfully after rebasing onto current `origin/main`. Browser validation used a fetch stub and two synthetic notifications, then verified Clear All emptied local state, hid the badge, rendered the empty state, and issued two `POST /api/v1/notifications/<id>/ack` calls with encoded IDs.
- Context: no new dashboard-observation bead was filed because the active duty scan found and claimed an existing ready caco-web bug. A later rescan confirmed `bd-1056da` remained the only assigned active caco-web bead and no ready open web/notifications beads were unassigned. First reintegration attempt refused safely because the remote caco-web agent branch held an earlier same-bead commit; inspection showed only recorded-summary drift, so the remote commit was preserved as ancestry. A later retry was paused after the technical-writer reported a fresh `bd-1d514b` direct-recorded recurrence; this branch is preserved and intentionally blocked from further autonomous direct-recorded retries.

## Diff summary

- Commits: implementation commit for `bd-1056da` plus this recorded summary.
- Files touched: `crates/caco-web/static/app.js`, `crates/caco-web/src/tests.rs`, and summary artifacts under `.cacophony/agent/ms-mac-cacophony-caco-web/summary/0084/`.
- Tests: +1 focused static contract test for Clear All persistence.
- Behavioural delta: the Clear All button now snapshots the current notification list, optimistically clears the UI, acknowledges each notification through the existing daemon ack endpoint, and reports a visible error toast if persistence fails. Cleared notifications should therefore stay cleared after refresh because the dashboard snapshot excludes acknowledged notifications.

## Embedded artefacts

- `web/board-and-inbox-scan.log` — initial inbox, assigned bead, ready queue, and web-adjacent label scan for this duty cycle.
- `web/board-and-inbox-rescan.log` — follow-up active-bead rescan before landing; confirms `bd-1056da` remained assigned here and no new web/notifications bead was claimed.
- `web/notifications-clear-validation.log` — Playwright validation using a mocked fetch layer to prove Clear All posts ack requests and clears the UI without mutating real daemon notifications.
- `web/final-validation.log` — final post-rebase focused test and `cargo check` record.
- `web/reintegration-refusal.log` — safe refusal details and same-bead remote-branch ancestry recovery plan.
- `web/direct-recorded-hold.log` — final blocker note after the technical-writer `bd-1d514b` recurrence warning.
- `web/blocked-duty-rescan-20260427-161118.log` — follow-up duty-cycle rescan showing `bd-1056da` is still the active assigned caco-web bead, no ready web/notifications beads are available, and `bd-95cda5` remains the live reintegration blocker.
- `web/blocked-duty-rescan-20260427-162053.log` — duty-cycle rescan showing `bd-1056da` is still active/held, no ready web-adjacent beads are available, `bd-95cda5` remains active, and the broken-on-main playback recurrence is owned elsewhere under `bd-00dbf4`.
- `web/blocked-duty-rescan-20260427-163056.log` — duty-cycle rescan showing `bd-1056da` is still active/held, no ready web-adjacent beads are available, `bd-95cda5` remains active, `origin/main` has advanced while this branch is preserved, and `bd-00dbf4` is now closed.
- `web/blocked-duty-rescan-20260427-164051.log` — duty-cycle rescan showing `bd-1056da` is still active/held, no ready web-adjacent beads are available, `bd-95cda5` remains active, `origin/main` advanced again while this branch is preserved, and the `bd-00dbf4` read hit a beads-primary maintenance response rather than a new caco-web action.
- `web/blocked-duty-rescan-20260427-165049.log` — duty-cycle rescan showing `bd-1056da` is still active/held, `origin/main` advanced again while this branch is preserved, early web label scans had no work, later bead reads hit Helsinki beads-primary maintenance, and duplicate `bd-9e4be4` worker coordination remains out of scope for this caco-web agent.
- `web/blocked-duty-rescan-20260427-170052.log` — duty-cycle rescan showing `bd-1056da` is still active/held, no ready web-adjacent beads are available, `bd-95cda5` remains active, `bd-9e4be4` is a separate P0 reintegration redesign bead with duplicate-worker containment, and `bd-00dbf4` remains closed.
- `web/blocked-duty-rescan-20260427-171103.log` — duty-cycle rescan showing `bd-1056da` is still active/held, no ready web-adjacent beads are available, `bd-95cda5` remains active, `bd-9e4be4` containment is finalized with `yuyg5sygj4ums1fj` as sole owner, `bd-00dbf4` remains closed, and no Playwright observation was run because the held active bead still preempts observation.
- `web/blocked-duty-rescan-20260427-172055.log` — latest duty-cycle rescan showing `bd-1056da` is still active/held, no ready web-adjacent beads are available, `bd-95cda5` remains active, `bd-9e4be4` remains assigned to sole owner `yuyg5sygj4ums1fj`, `bd-00dbf4` remains closed, and no Playwright observation was run because the held active bead still preempts observation.
- `web/index-lock-inspection-20260427-172214.log` — inspected a transient `.git/index.lock` commit failure; no lock remained and no checkout-local git process was active, so the amended summary commit was retried successfully without removing a live lock.
- `web/direct-preflight-20260427-1724.log` — operator-directed direct reintegration preflight: backup branch name, rebase conflict handling, rebased `origin/main` SHA, and post-rebase focused caco-web validation results.
- `web/technical-writer-bd-95cda5-recurrence-0077.txt` — copied scratch evidence for the direct-recorded recurrence that caused this caco-web landing hold.
- `web/notifications-clear-server.log` — dev-server request log for the browser validation.
- `web/page-2026-04-27T14-58-05-046Z.yml` — Playwright page snapshot for the Notifications view.

## Operator-takeaway

The web Notifications Clear All control now persists its effect through the existing ack API instead of only hiding rows in memory. This was an existing ready web bead, so the duty cycle correctly fixed it rather than creating duplicate observation work. The code is implemented, validated, backed up, and ancestry-reconciled, but landing is intentionally paused because another agent just reproduced the `bd-1d514b` direct-recorded publish hazard; summary-index friction is separately tracked in draft `bd-3a9c14`.
