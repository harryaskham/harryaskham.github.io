# Session summary — bd-68a957: validate caco web serve startup flags

## Goal

Fix the fresh `caco web` surface so invalid startup flags fail fast with
operator-friendly validation errors instead of silently falling back to the
default port or deferring malformed daemon URLs until first use.

## Bead(s)

- `bd-68a957` — `caco web --port` silently coerces invalid values to default 11180; `--bind` and `--daemon-url` need better validation

## Before state

- `caco web --port abc`, `--port ''`, `--port -1`, and `--port 99999` all
  silently fell back to port `11180` because the CLI parsed to `u16` with
  `.parse().ok().unwrap_or(11180)`.
- `caco web --bind not-an-ip` failed only later via the web server with a bare
  `invalid socket address syntax` message and no concrete example.
- `caco web --daemon-url not-a-url` was accepted at startup and only failed on
  the first proxied request.

## After state

- `caco web --port` now validates strictly and rejects invalid values with:
  `bd-68a957: --port must be a positive integer 1..65535, got 'X'`.
- `caco web --bind` now rejects invalid or empty bind values up front with a
  message that includes valid examples and the full socket form.
- `caco web --daemon-url` now validates at startup using `reqwest::Url` and
  requires an `http://` or `https://` URL with a host.
- Added direct validator tests plus a source-contract check that
  `dispatch_web()` actually calls the validators.

## Diff summary

- Files touched:
  - `crates/caco-cli/src/lib.rs`
- Tests:
  - `cargo test -p caco-cli web_port_bind_and_daemon_url_validators_bd_68a957 -- --nocapture`
  - `cargo run -q -p caco -- web --port abc`
  - `cargo run -q -p caco -- web --bind not-an-ip`
  - `cargo run -q -p caco -- web --daemon-url not-a-url`
- Behavioural delta:
  - The `caco web` startup path is now fail-fast and operator-friendly instead
    of silently coercing or deferring bad inputs.

## Operator-takeaway

This was a classic fresh-surface validation hole: the feature worked for happy
paths, but invalid inputs quietly degraded into misleading defaults. The CLI
now rejects bad `web` startup flags early with explicit, example-rich errors so
operators do not burn time debugging the wrong port or a latent bad daemon URL.
