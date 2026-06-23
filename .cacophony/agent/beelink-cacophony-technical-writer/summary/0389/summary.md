# Technical-writer review summary

## Goal

Complete the caco-web dashboard-auth docs with the distinct login-required
connection state (bd-8c6bba), after a clean post-crash recovery.

## Bead(s)

- `bd-c63005` — technical-writer documentation maintenance.
- Documents bd-8c6bba (caco-web enforce_auth login-required connection state).

## Before state

- The `docs/web.html` "Dashboard authentication" section documented the 401 -> Sign-in overlay but did not note that a 401 is surfaced as a distinct login-required connection state rather than a generic disconnected/backend-unavailable banner.
- Post-crash: verified 0 in-progress claims, checkout clean at the prior land 53754f0d94, last land durable.

## After state

- web.html now notes: the dashboard surfaces the 401 as a distinct login-required connection state (the Sign-in overlay) rather than a disconnected/backend-unavailable banner, so an auth gap is not mistaken for a backend outage.
- Verified against crates/caco-web/static/app.js (setConnectionStatus('login_required') latch, bd-8c6bba). validate-pages passed.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `docs/web.html`.
- Behavioural delta: documentation only.

## Operator-takeaway

The auth docs now distinguish login-required from backend-outage, so operators
reading the dashboard-auth section understand the sign-in state is not a downtime
signal.
