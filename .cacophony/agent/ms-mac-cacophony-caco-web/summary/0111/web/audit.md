# caco-web comprehensive dashboard audit — 0111

## Scope inspected

- Standard current-assets pass: Workspace, Status, Agents, Beads, Feed, Chat, Summaries, keyboard `w` / `s` / `?`, console, and network.
- Expanded pass requested by Harry: narrow and wide routes for Status, Agents, Beads, Feed, Chat, Nodes, Services, Projects, Choices, Notifications, Actions, Logs, Timeline, Summaries, Merge Queue, and Workspace.
- Workspace pane sweep: Agents, Beads, Chat, Logs, Feed, Source, Status, Services, Nodes, Projects, Notifications, Actions, Timeline, Choices, Merge Queue, Speech, Bead Detail, Agent Detail, Hooks, and most Crons/Terminal coverage. The ad-hoc pane sweep had a quote-splitting issue for the first/last values, so a draft helper bead was filed.

## Defects filed and claimed

- `bd-f02d90` — caco-web logs stream abort pollutes browser console.
  - Evidence: `web/comprehensive-audit.log` lines 3518-3522 reported `Total messages: 1 (Errors: 1, Warnings: 0)` and `net::ERR_INCOMPLETE_CHUNKED_ENCODING` for `/api/v1/logs/stream?follow=true`.
  - Fix: convert upstream SSE read errors in `crates/caco-web/src/proxy.rs` into handled `event: error` SSE events instead of `Body` stream errors.
  - After-fix proof: `web/after-fix-logs-stream-console.log` shows the Logs route opened, route navigation away/back, and `Total messages: 0 (Errors: 0, Warnings: 0)`.

## Other observations

- `bd-1cf76a` remains in-progress under ms-dev and was not duplicated.
- No ready/open caco-web bead was available at cycle start.
- `bd-ead437` was filed as a draft workflow follow-up: add a first-party comprehensive caco-web route audit helper so future passes do not depend on brittle ad-hoc shell loops.
- Initial snapshot-timeout and later connected/degraded states used explicit operator copy across Status, Feed, Workspace, and Summaries.
- Several overflow probes reported intentional scroll containers (`overflow: auto`) for dense tables/logs; no new horizontal overflow defect was identified beyond the logs-stream console issue.
