# Session summary — bd-d38ec2: embeddable caco-web Pico AgentView + standalone /pico

## Goal

Continue caco-web pico parity by extracting the native browser Pico AgentView into a reusable mountable component and adding a standalone `/pico` page. This makes the browser surface usable by caco-embedded/direct consumers, not only the main dashboard and Workspace panes.

## Bead(s)

- `bd-d38ec2` — [pico] caco-web: extract embeddable Pico AgentView component + standalone /pico route.

## Before state

- The pico pane existed inside dashboard agent detail and Workspace via direct calls to `renderAgentPicoTab`.
- There was no reusable browser mount contract for caco-embedded or a minimal standalone page.
- Loading `app.js` outside the dashboard shell would start dashboard snapshot/merge-queue polling.

## After state

- `pico-agent-view.js` exposes `window.CacoPicoAgentView.mount(container, {agentId, wsUrl, label, chrome})` as a thin wrapper over the canonical caco-web pico renderer.
- Workspace `picoSession` panes consume the reusable wrapper rather than calling the renderer directly.
- `pico.html` provides a standalone shell with `?agent=<agent-id>` guidance.
- caco-web serves `/pico` directly as the standalone Pico page.
- `app.js` accepts an explicit pico session WebSocket URL override and skips dashboard bootstrap/polling on the standalone Pico page.
- caco-web validation passes: 639 tests.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched:
  - `crates/caco-web/static/pico-agent-view.js` — new embeddable component wrapper.
  - `crates/caco-web/static/pico.html` — new standalone page.
  - `crates/caco-web/static/app.js` — session URL override + standalone bootstrap guard.
  - `crates/caco-web/static/index.html` — load component after app.js.
  - `crates/caco-web/static/workspace-panes.js` — Workspace consumes the component.
  - `crates/caco-web/src/server.rs` — `/pico` route.
  - `crates/caco-web/src/tests.rs` — source/asset/route guard and ID-audit expansion.
- Tests: +1 caco-web source test for embeddable/standalone contract.
- Behavioural delta: direct consumers can mount the same native Pico AgentView and `/pico` can be opened without the full dashboard shell polling APIs.

## Embedded artefacts

- `web/validation.txt` — validation commands/results.
- `web/screenshots/pico-standalone-empty.png` — standalone `/pico` empty-state screenshot.

## Operator-takeaway

caco-web now has a reusable native Pico AgentView component and a standalone `/pico` entrypoint, so the browser surface is no longer tied exclusively to the dashboard/Workspace shell and can support caco-embedded style use.
