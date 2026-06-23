# Technical-writer review summary

## Goal

Complete the caco-web dashboard authentication docs with the same-day default-on
enforcement flip (bd-a482f2/bd-764efa), verified against source.

## Bead(s)

- `bd-c63005` — technical-writer documentation maintenance.
- Documents bd-a482f2 / bd-764efa (caco-web enforce_auth default-on flip).

## Before state

- The `docs/web.html` "Dashboard authentication" section (landed earlier today) described the login flow but not that auth enforcement is now default-on when configured.

## After state

- Added a paragraph: enforcement is default-on once the dashboard token + cookie-signing key are configured; `CACO_WEB_ENFORCE_AUTH` overrides (`1`/`true`=on, else off); inert when credentials are unset (dev/unconfigured unchanged). When active, `/api/*` (except `/api/web/session`) and `/metrics` require a session cookie or dashboard bearer (else 401); static pages, `/health`, and the login endpoint stay open.
- Resolved the apparent contradiction (commit "default-on flip" vs stale struct-level "defaults false" comments) by reading the authoritative `resolve_web_enforce_auth` (bd-764efa) — the flip is in config resolution, gated on the dashboard being configured.
- Validation: `./docs/validate-pages.sh` passed.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `docs/web.html`.
- Behavioural delta: documentation only.

## Operator-takeaway

The dashboard-auth docs now state when enforcement actually kicks in (configured =
default-on) and exactly what is gated, completing the sign-in documentation.
