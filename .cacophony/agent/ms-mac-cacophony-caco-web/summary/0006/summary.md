# Session summary — caco-web backend-unavailable state for dashboard 503s

## Goal

Run the caco-web active duty cycle, convert the Playwright-observed restart-window defect into an authoritative bead, and make the dashboard stop presenting a green Connected state when daemon-backed dashboard APIs are returning HTTP 503.

## Bead(s)

- `bd-c3521a` — caco-web shows Connected while dashboard APIs return 503

## Before state

- Failing tests: none known.
- Relevant metrics: Playwright against the managed dashboard during a daemon restart saw `/health` return 200 while `/api/v1/ui/snapshot`, `/api/v1/merge-queue?since=24h`, `/api/v1/ui/stream`, and `/api/v1/logs/stream?follow=true` returned 503; the UI still exposed `Connected` and `Live SSE connected` with `Snapshot pending`.
- Context: the web shell could be alive while daemon-backed APIs were unavailable, and SSE open/error handling could overwrite the snapshot failure state with generic connected/disconnected wording.

## After state

- Failing tests: none known.
- Relevant metrics: patched Playwright repro with mocked 503 dashboard APIs shows header `Backend unavailable`, tooltip `Dashboard backend is temporarily unavailable (HTTP 5xx)...`, hero `Dashboard backend unavailable…`, and class `hero-pill backend_unavailable`.
- Context: snapshot 5xx/fetch failures now set a backend-unavailable flag and connection status; SSE open/error handling preserves that degraded state until a successful snapshot or real UI event proves recovery.

## Diff summary

- Commits: `d5168d9b4`
- Files touched: `crates/caco-web/static/app.js`, `crates/caco-web/static/style.css`, `crates/caco-web/src/tests.rs`
- Tests: +1 / -0 / flipped 0
- Behavioural delta: dashboard restart windows now show an explicit backend-unavailable state instead of a misleading green Connected status when the web shell is up but daemon-backed APIs are returning 5xx.

## Operator-takeaway

This turns a confusing restart-window trust gap into an explicit degraded dashboard state, so operators can distinguish “the web shell is reachable” from “the dashboard backend is actually usable.”
