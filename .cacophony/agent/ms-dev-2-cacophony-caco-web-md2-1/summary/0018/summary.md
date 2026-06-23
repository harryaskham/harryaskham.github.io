# Session summary — caco-web auth Slice 3 (SPA login form + ?token bootstrap)

## Goal

Build the dashboard login UX so request enforcement can be turned on without
locking out the dashboard: a zero-config `?token` bootstrap exchange with
strip-on-load, and an accessible login form shown on a dashboard-auth 401. Plus a
`CACO_WEB_ENFORCE_AUTH` opt-in so the full flow is testable before the default-on
flip. Built and chromium-validated; held for msm-3's review.

## Bead(s)

- `bd-323724` — caco-web auth Slice 3: SPA login form + ?token bootstrap + strip-on-load
- parent `bd-e5f22f` (reopened — it was wrongly auto-closed when a slice-2 commit
  mentioned the ID); epic `bd-764efa`. Remaining: the `enforce_auth` flip (slice 4).

## Before state

- Failing tests: none. Slices 1+2 (middleware + bootstrap) landed inert; there was
  no login UX, so enforcement could not be enabled without locking the dashboard.

## After state

- Failing tests: none. `cargo test -p caco-web --lib auth_middleware login_endpoint`
  green (5/5); `cargo check -p caco-cli` green; `node --check app.js` OK.
- `static/app.js`: `maybeBootstrapLogin()` (strip `?token` before any fetch, then
  exchange for the cookie; init made async), `showLoginForm()` (accessible overlay),
  and a marker-gated 401 hook (form only on the dashboard-auth 401).
- `server.rs`: the middleware 401 carries an `x-caco-web-login:1` marker.
- `caco-cli`: `CACO_WEB_ENFORCE_AUTH=1` opt-in (default OFF).

## Diff summary

- Code commit: pending final squash SHA from the reintegration receipt.
- Files: `crates/caco-web/static/app.js`, `crates/caco-web/src/server.rs`,
  `crates/caco-cli/src/lib.rs`.
- Tests: +0 Rust (5 existing middleware/login tests still green); chromium e2e
  evidence in `summary/pending/web/screenshots/`.
- Behavioural delta: none with enforce off (default) — the form only shows on a
  marked dashboard-auth 401, which only happens when enforce_auth is on.

## Embedded artefacts

- `web/screenshots/slice3-login-overlay.png` — the login overlay (dark-themed,
  role=dialog, focused input, legible).
- `web/screenshots/slice3-login-auto-on-401.png` — the form auto-shown on a 401.

## Operator-takeaway

The login UX is built and chromium-validated end to end: a loopback `caco web`
launch's `?token` link auto-logs-in and strips the token before any external fetch
(hardening 1); a missing/expired session shows an accessible login form; and the
form is gated to the dashboard-auth 401 (a proxied daemon 401 shows the forbidden
banner instead, not a useless login prompt). It is fully inert until enforcement is
enabled (`CACO_WEB_ENFORCE_AUTH=1`, or the later default-on flip). The only thing
left before turning enforcement on is the flip itself (slice 4), which msm-3 reviews.
