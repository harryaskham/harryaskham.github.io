# Session summary — web summaries loading fix

## Goal

Run the caco-web active duty cycle, pick up the newly discovered unowned web-summaries bug, and make the browser Summaries view load recorded summaries reliably under current state-branch pressure without noisy browser console failures.

## Bead(s)

- `bd-728663` — Fix web summaries loading in caco
- follow-up draft: `bd-50b0ae` — Optimize unscoped summaries listing under large state branches

## Before state

- Failing tests: none at session start.
- Relevant metrics: board scan found no assigned caco-web work but did find open unowned `bd-728663`; managed caco-web was healthy on port 11180 at version 1.2.567 while current checkout built caco-web 1.2.568.
- Context: the Summaries route often stayed on `Loading summaries…` or returned `Session summaries unavailable: daemon proxy timed out`. A focused repro showed `/api/v1/summaries?limit=20&offset=0` timing out at the web proxy's 8s summaries budget, while later daemon CLI calls could succeed after restart. Additional validation showed unscoped summary listing can hit the daemon's internal 30s timeout, whereas project-scoped queries return quickly.

## After state

- Failing tests: none observed.
- Relevant metrics: `cargo check -p caco-web --all-targets` passed; focused caco-web tests passed; full `cargo test -p caco-web --lib` passed with 298 tests. Current-assets Playwright validation rendered 10 summaries out of 933 for project `cacophony`, loaded parsed detail, and reported browser console `0` errors / `0` warnings with all observed requests returning `200 OK`.
- Context: the main dashboard shell now injects `window.CACO_WEB_DEFAULT_PROJECT`, using `CACO_WEB_DEFAULT_PROJECT`, `CACO_PROJECT`, or `CACOPHONY_PROJECT`. The Summaries frontend uses that project by default, requests a smaller initial page, explains cold list/detail loading, and the proxy keeps summaries 5xx responses console-clean with handled sentinel JSON.

## Diff summary

- Commits: `fix(caco-web): load web summaries by project (bd-728663)` on this agent branch; final squash hash is assigned during direct recorded reintegration.
- Files touched: `crates/caco-web/src/proxy.rs`, `crates/caco-web/src/server.rs`, `crates/caco-web/src/bin/caco-web-dev-server.rs`, `crates/caco-web/src/lib.rs`, `crates/caco-web/static/summaries.js`, `crates/caco-web/src/tests.rs`, `.cacophony/agent/ms-mac-cacophony-caco-web/summary/0023/*`.
- Tests: +1 targeted regression for dashboard default-project injection; existing summaries proxy/view regression expanded for 45s budget, project-scoped fetch, smaller initial page, and loading copy. Validation also included live current-assets Playwright against the Summaries route.
- Behavioural delta: Summaries no longer starts with an expensive unscoped cross-project query when the dashboard knows the current project; it loads a bounded 10-row page, uses a longer proxy budget for cold state-branch reads, and preserves console-clean retryable error handling for upstream summary 5xx responses.

## Embedded artefacts

- `web/board-and-inbox-scan.log` — inbox, assigned bead scan, label scans, text scan showing `bd-728663`, and caco web status.
- `web/bd-728663-show.log` — claimed bead details and acceptance criteria.
- `web/bd-728663-focused-repro.log` — before repro showing Summaries timeout sentinel after the original 8s proxy budget.
- `web/bd-728663-validation-timeout45-limit10.log` — intermediate validation showing unscoped list could still hit daemon internal 30s timeout.
- `web/bd-728663-validation-project-injected.log` — final Playwright validation proving project-scoped Summaries loaded 10 rows plus parsed detail with console clean.
- `web/validation-summary.log` — concise validation command summary.
- `web/reflection-dedup.log` and `web/reflection-created.log` — reflect-session dedup and draft follow-up creation.
- `web/screenshots/bd-728663-before-timeout.png` — before screenshot of retryable summaries timeout state.
- `web/screenshots/bd-728663-after-loaded.png` — after screenshot of loaded summaries list/detail for project `cacophony`.
- `web/bd-728663-after-snapshot.yml` — final Playwright snapshot metadata.

## Operator-takeaway

The visible caco-web Summaries failure was a web/default-query problem amplified by a backend hot path: project-scoped summary reads are fast enough, while unscoped state-branch scans can exceed daemon budgets. This slice fixes the browser experience and files a draft follow-up for the remaining daemon-side optimization.
