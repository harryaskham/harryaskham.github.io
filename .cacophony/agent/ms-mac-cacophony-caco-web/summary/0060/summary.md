# Session summary — caco-web duty cycle filed Workspace narrow overflow

## Goal

Run the caco-web active duty cycle: check inbox and assigned or ready caco-web work, use a lightweight browser observation pass when no active bead could be found for this agent, and file/claim one focused web bead if fresh evidence warranted it.

## Bead(s)

- `bd-771b58` — caco-web Workspace narrow agent pane table overflows horizontally. Filed and claimed by this agent from this cycle's evidence.
- `bd-95cda5` — [docs] recorded direct reintegration partially succeeds then errors on PR URL. Still `in_progress`/P1, assigned to `cacophony:jo2w72j0u3ol2b0x`; safety context only for this cycle.
- `bd-f74047` — Managed caco web serves stale dashboard assets after update. Known existing in-progress web work assigned to `cacophony:ms-dev-cacophony-caco-dev-msd-4`; not duplicated.
- `bd-1cf76a` — Teach caco-web-observe focused delayed-route validation scenarios. Known existing in-progress helper work assigned to `ms-dev:ms-dev-cacophony-caco-dev-msd-4`; not duplicated.

## Before state

- Failing tests: none; this was observation and bead filing only.
- Relevant metrics: local checkout remained ahead of and behind `origin/main` with preserved local summaries under the `bd-95cda5` direct,recorded hold. This cycle fetched refs only and did not rebase, reset, cherry-pick, or reintegrate.
- Context: the initial inbox/board scan was degraded. `caco msg inbox`, assigned-bead reads, ready-open reads, and several label scans timed out or reported the local daemon was restarting or not yet bound to the listener. `bd-95cda5` was readable and remained in progress/P1 under another assignee. A later authoritative retry recovered enough to confirm no in-progress bead assigned to this agent, no ready open beads, and no open `caco-web`, `workspace`, `dashboard`, or `summaries` beads.

## After state

- Failing tests: none; no product code changed.
- Relevant metrics: current preserved-checkout caco-web observation used `v1.2.569`; browser console had 0 messages, 0 errors, and 0 warnings. Observed completed route-critical requests returned 200 OK, and one final `/api/v1/ui/snapshot` was still in-flight at browser close.
- Context: the helper exercised Workspace, keyboard shortcuts, help overlay, Status, Agents, Beads, Feed, Chat, Summaries, and wide/narrow responsive states. Connection recovered to `Connected` / `Live SSE connected`; Status reported `beads: partial` and `Snapshot degraded` after daemon backpressure. Status hero remained unclipped. Summaries used `/api/v1/summaries?limit=10&offset=0&project=cacophony`, returned 200 in about 2.7s, displayed `10 of 979`, and loaded detail `ms-dev-cacophony-caco-dev-msd-3/36` in about 112ms.

## Diff summary

- Commits: local recorded-summary commit for this cycle; not reintegrated because `bd-95cda5` direct,recorded safety hold remains active.
- Files touched: `.cacophony/agent/ms-mac-cacophony-caco-web/summary/0060/*`.
- Tests: no Rust tests run; validation was the lightweight Playwright/current-checkout observation pass plus recovered board retries.
- Behavioural delta: no product-code behaviour changed. Board state gained a focused in-progress visual bug, `bd-771b58`, for the newly observed narrow Workspace agent-pane overflow.

## Embedded artefacts

- `web/board-and-inbox-scan.log` — degraded and recovered inbox/board checks, duplicate checks, filed bead output, and `bd-771b58` confirmation.
- `web/observation.log` — full `caco-web-observe` route/keyboard/console/network transcript.
- `web/server.log` — temporary current-checkout dev-server request log.
- `web/notes.md` — concise metrics, filed-bead rationale, and reintegration-hold context.
- `web/01-page-2026-04-27T10-58-37-358Z.yml` — initial Playwright snapshot.
- `web/screenshots/02-page-2026-04-27T10-58-49-170Z.png` — narrow Workspace screenshot showing the Agents pane table with only `AGENT`, `STATE`, and part of `BEAD` visible.
- `web/screenshots/*.png` — additional bounded screenshots copied from `.playwright-cli`, including Status, Agents, Beads, Feed, Chat, Summaries, and final state.

## Operator-takeaway

This cycle found and filed a real visual browser-app issue: live data makes the narrow Workspace Agents pane fall back to horizontal table scrolling, hiding core row context off-canvas. The fix is tracked as `bd-771b58`, but no implementation or reintegration was attempted while `bd-95cda5` keeps direct,recorded reintegration unsafe.
