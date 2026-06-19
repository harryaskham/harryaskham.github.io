# Session summary — caco-web auth Slice 3b(1) (login endpoint + credential plumbing)

## Goal

Wire the resolved dashboard token + cookie key onto WebConfig/ProxyState and add
the dashboard LOGIN endpoint (`POST /api/web/session`) that exchanges the access
token for a signed httpOnly session cookie. Additive / inert-effect: no
request-time enforcement yet (the auth middleware + the zero-config loopback
auto-credential design is the remaining piece, gated on msm-3's review).

## Bead(s)

- `bd-f451b8` — caco-web auth Slice 3b(1): login endpoint + credential plumbing
- parent (kept OPEN): the caco-web real-auth child of the security epic; the
  enforcement middleware (gate `/api/*` + WS) + proxy.rs:812 guard + the loopback
  auto-credential + SPA login view remain.

## Before state

- Failing tests: none. dispatch_web resolved the dashboard token/key only to gate
  startup (S2) and discarded them; there was no login endpoint and no way for a
  browser to obtain a session cookie.

## After state

- Failing tests: none. `cargo test -p caco-web --lib login_endpoint + auth::`
  green (22/22); `cargo check --workspace --tests` green.
- `WebConfig` + `ProxyState`: `dashboard_token` + `cookie_key` fields (Default
  None; dev-server + embedded-daemon = None).
- `caco-cli dispatch_web`: resolve-and-STORE the credentials on WebConfig
  (preserving loopback auto-gen + non-loopback refuse-to-start from S2).
- `server.rs`: `POST /api/web/session` — validate the dashboard token
  (constant-time) → mint a signed session cookie (HttpOnly + SameSite=Strict +
  Secure + Path=/, 30d). Inert (404) until token + key are configured; 401 on
  wrong/missing. The browser never holds the token in JS.

## Diff summary

- Code commit: pending final squash SHA from the reintegration receipt.
- Files: `crates/caco-web/src/{server.rs,proxy.rs,tests.rs}`,
  `crates/caco-cli/src/lib.rs`, `crates/caco-daemon/src/lib.rs`,
  `crates/caco-web/src/bin/caco-web-dev-server.rs`.
- Tests: +2 axum integration (valid→200+secure cookie, wrong→401, unconfigured
  →404).
- Behavioural delta: a new login endpoint (token-gated cookie minting). No
  enforcement consumes the cookie yet → no change to existing surfaces.

## Embedded artefacts

None (Rust + integration tests).

## Operator-takeaway

The browser's auth entry point is in place and tested: present the dashboard
token to `POST /api/web/session`, receive a secure httpOnly session cookie. It
uses the already-reviewed S1 crypto and is inert-effect (nothing enforces the
cookie yet), so it lands risk-free. The remaining piece — the request-time
enforcement middleware that gates `/api/*` — is blocked on one design decision
for msm-3: the zero-config LOOPBACK auto-credential must work WITHOUT an IP/bind
bypass (Funnel forwards from 127.0.0.1). The leading option is a Jupyter-style
launch URL token (`caco web` prints the dashboard URL with `?token=<auto-gen>`;
the SPA exchanges it for the cookie). That decision unblocks enabling enforcement
+ the SPA login view.
