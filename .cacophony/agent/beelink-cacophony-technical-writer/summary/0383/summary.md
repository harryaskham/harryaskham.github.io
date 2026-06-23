# Technical-writer review summary

## Goal

Document the caco-web dashboard authentication surface (session cookie + Sign-in
overlay + zero-config loopback ?token bootstrap), which shipped undocumented.

## Bead(s)

- `bd-c63005` — technical-writer documentation maintenance.
- Documents bd-e5f22f / bd-323724 (caco-web SPA login + ?token bootstrap).

## Before state

- `docs/web.html` had no dashboard authentication coverage (no login/session/token section), despite the shipped login overlay + `/api/web/session` exchange + loopback `?token` bootstrap.

## After state

- `docs/web.html` has a new "Dashboard authentication" section: httpOnly session cookie via `POST /api/web/session`; the Sign-in overlay (paste the caco-web access token from `$CACOPHONY_DIR/tokens/caco-web.token`) shown on 401; and the zero-config loopback `?token` bootstrap that is stripped from the URL before any network call (no Referer/history leak) so the durable token never travels in a URL.
- Verified against `crates/caco-web/static/app.js` (`maybeBootstrapLogin` / `showLoginForm`, bd-e5f22f).
- Validation: `./docs/validate-pages.sh` passed.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `docs/web.html`.
- Behavioural delta: documentation only.

## Operator-takeaway

Operators now have a doc for how to sign in to the dashboard. Separately noted:
bd-89088a/bd-0ec380 (forge-verify/mirror-lag, "part 1") landed; the
reintegration-policy update for that lands once the forge-verify work completes.
