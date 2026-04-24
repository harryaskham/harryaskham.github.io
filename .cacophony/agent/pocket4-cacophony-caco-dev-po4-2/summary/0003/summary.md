# Session summary — quick-file bead timeout fix

## Goal

Fix the quick-file bead expand-with-AI path timing out silently when
the LLM takes longer than the browser's default fetch timeout to
expand verbose free-text into multiple beads. Add explicit timeouts
on both the browser side (120s AbortController) and the daemon side
(90s reqwest client timeout).

## Bead(s)

- `bd-eb581e` — Increase timeout for quick file bead operations
  (P1 bug)

## Before state

- Failing tests: none related
- Browser `fetch()` for `/beads/expand` had no timeout — relied on
  browser/proxy defaults (~60s, varies)
- Daemon `reqwest::Client::new()` in both Anthropic and OpenAI LLM
  call paths had no timeout — a wedged LLM endpoint would hang the
  handler forever
- Operator creating many beads via expand-with-AI would see either a
  silent hang or a cryptic network error

## After state

- Browser: 120s AbortController on the expand fetch — surfaces as a
  catchable error with a clear message
- Daemon: 90s reqwest timeout on both Anthropic and OpenAI LLM
  call paths — daemon returns a timeout error to the browser before
  the browser's own timer fires
- 180/180 test-small green (no test regressions)

## Diff summary

- Files touched:
  - `crates/caco-web/static/app.js` (+AbortController on expand
    fetch)
  - `crates/caco-daemon/src/llm.rs` (+90s timeout on both LLM
    provider paths)
- Tests: +0 / -0 / flipped 0
- Behavioural delta: expand-with-AI path now has layered timeouts
  (daemon 90s < browser 120s) so the daemon times out first with a
  structured error, and the browser times out as a backstop if the
  daemon response itself is lost.

## Operator-takeaway

The root cause was "no timeout anywhere" — a common pattern when LLM
calls are added via `reqwest::Client::new()` without a builder. The
90s daemon-side timeout is deliberately shorter than the 120s
browser-side AbortController so the operator sees a daemon error
message (which can include the LLM provider's error details) rather
than a generic browser abort. Future LLM call sites should use the
same timeout-builder pattern.
