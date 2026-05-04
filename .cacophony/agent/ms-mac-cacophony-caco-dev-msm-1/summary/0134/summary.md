# Session summary — TTS feed identity node-token regression

## Goal

Fix the regression where TTS feed handling could surface the synthetic `node-token` bearer identity as the speaking agent, instead of using the real source agent identity carried inside the feed payload.

## Bead(s)

- `bd-ef87ac` — Fix regression where agent speech identity is announced as literal node-token

## Before state

- Failing tests: none known at session start.
- Relevant metrics: operator reported live agent speech identity being announced as literal `node-token`, a regression after the earlier bd-da1220 caller-forwarding fix.
- Context: the TTS daemon already forwarded `x-caco-caller` to `/api/v1/audio/speech`, but feed-event extraction preferred the wrapper sender before the inner payload sender; when the wrapper sender was synthetic `ms-mac:_:node-token`, policy/traces/status could resolve against that placeholder.

## After state

- Failing tests: none observed.
- Relevant metrics: final `cargo fmt --all -- --check` passed; final `git diff --check` passed; queued `tj-ff93c2b6` passed `RUST_MIN_STACK=33554432 cargo test -p caco-cli bd_ef87ac --lib -- --test-threads=2` with two focused regressions.
- Context: feed-event extraction now rejects `node-token` caller parts and prefers a clean inner real sender when present; choice-presented events with a synthetic wrapper sender fall back to the presenting agent id.

## Diff summary

- Commits: `ccd6ca71a`.
- Files touched: `crates/caco-cli/src/lib.rs`, `README.md`, `SPEC.md`, `AGENTS.md`.
- Tests: +2 focused caco-cli unit regressions covering wrapped message speech and choice-presented speech when the outer feed sender is `ms-mac:_:node-token`.
- Behavioural delta: TTS speech identity, policy evaluation, traces, and status diagnostics no longer let a synthetic node-token wrapper sender override a real inner agent identity.

## Operator-takeaway

The TTS daemon should again announce and evaluate agent-authored speech as the real agent rather than the bearer-token placeholder, including after feed wrapping/replay paths that carry a synthetic outer sender.
