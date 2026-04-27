# Session summary — managed caco-web stale-assets observation

## Goal

Run the caco-web active duty cycle requested by Harry: check inbox and caco-web bead readiness, then exercise the browser dashboard with a lightweight Playwright observation pass. This cycle used the managed dashboard at `http://127.0.0.1:11180` to verify the operator-facing service after `bd-95cda5` was reported closed and the guard PR was manually landed.

## Bead(s)

- Attempted to file and claim a new focused bug: `Managed caco web serves stale dashboard assets after update`.
- Bead id: not available yet. The first create attempt failed because the local daemon/beads API was unreachable; retry queued the create as outbox entry `outbox-019dcdf8-5ae4-7913-aa83-c270f5d0a76e` because beads primary was unreachable.
- Safety context: `bd-95cda5` — now reported `closed`, with manual PR guard merge `adb5f24ff` and guard commit `5976882de6d035f759181fa75dcbb021548b8639` on `origin/main`.
- Related owned-elsewhere bead observed: `bd-1cf76a` — Teach caco-web-observe focused delayed-route validation scenarios; currently in progress for `ms-dev-cacophony-caco-dev-msd-4`, not this agent.

## Before state

- Failing tests: none; this was an observation-only duty cycle.
- Relevant metrics: checkout had fourteen unreintegrated local preservation commits at start of this cycle and was ten commits behind `origin/main` after fetching.
- Context: inbox included the update that `bd-95cda5` was manually landed through GitHub PR #17. No assigned in-progress bead was found for this agent. Ready/open label scans found no caco-web/dashboard/web/browser/workspace/playwright/webui/summaries/visual-polish bead. Text scans found no open web implementation bead.

## After state

- Failing tests: none; no product code changed.
- Relevant metrics: managed dashboard observation reported web UI `v1.2.567`; browser console had 0 messages, 0 errors, and 0 warnings. Status hero stayed unclipped (`h=330`, `scrollHeight=328`). Workspace narrow showed only the known readable `✅ no choices` overflow/scroll-container entry.
- Context: the managed dashboard still used stale Summaries behavior: `/api/v1/summaries?limit=200&offset=0` rather than the current project-scoped bounded request path. The route ended with handled retryable copy, `Session summaries unavailable: daemon proxy timed out`. Browser network saw the handled Summaries response as HTTP 200, and two final `/api/v1/node` requests were aborted when the browser closed. After the pass, local daemon/web endpoints became unreachable while filing the bead, so the bug create was queued rather than returning a bead id.

## Diff summary

- Commits: local observation-only recorded-summary commit for this cycle; not reintegrated in this turn because this checkout still contains preserved local summary commits and is behind current `origin/main`.
- Files touched: `.cacophony/agent/ms-mac-cacophony-caco-web/summary/0047/*`.
- Tests: no Rust tests run; validation was the lightweight Playwright observation pass against the managed dashboard service.
- Behavioural delta: none in product code. Operationally, this cycle identified stale managed caco-web assets: the operator-facing dashboard service still appears to serve `v1.2.567` and old unscoped Summaries behavior after the rest of the fleet reported `v1.2.570`.

## Embedded artefacts

- `web/board-and-inbox-scan.log` — inbox output, `bd-95cda5` status, assigned-bead scans, label/text scans, duplicate search, daemon-status output after filing failure, and bead-create retry result.
- `web/observation.log` — full `caco-web-observe` route/keyboard/console/network transcript against `http://127.0.0.1:11180`.
- `web/filed-bead.log` — failed first bead-create attempt and queued outbox result.
- `web/notes.md` — concise metrics, stale-assets finding, no-reintegration caveat, and bead filing status.
- `web/01-page-2026-04-27T08-02-01-404Z.yml` — initial Playwright snapshot.
- `web/screenshots/*.png` — bounded screenshots copied from `.playwright-cli`, including Workspace narrow/wide route passes, handled Summaries timeout state, and final dashboard state.

## Operator-takeaway

The active web UI itself remained console-clean and visually stable, but this pass found an actionable caco-web operations/product gap: the managed dashboard on port 11180 still serves stale `v1.2.567` assets and old unscoped Summaries behavior after the system moved on. A focused bug create was attempted and queued for retry because the local daemon/beads service became unreachable during filing; no bead id or claim exists yet.
