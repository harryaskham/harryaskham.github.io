# Session summary — MS Pi startup timeout tuning

## Goal

Reduce false-negative startup and resume failures for Pi-managed agents on MS nodes, where healthy sessions were being marked failed while the runtime and MCP bridge were still starting under load. The fix keeps direct runtime failures visible but gives Pi enough time to pass the same readiness gates on slower hosts.

## Bead(s)

- `bd-3886df` — MS node agent startup timeout is too short

## Before state

- Failing tests: none known for this bead.
- Relevant metrics: Pi runtime launch verification used the shared Node.js timeout of 10s; resume provider-ready handoff used 60s even though first-party MCP startup windows such as Tendril can legitimately take up to 180s.
- Context: bead evidence showed ms-mac/ms-dev workers recovering after being reported failed, and the unresolved example was a Pi worker failing because tmux still observed `bash` instead of the configured Pi runtime during startup.

## After state

- Failing tests: none from targeted validation.
- Relevant metrics: Pi runtime launch verification now uses a Pi-specific 30s window; resume provider-ready handoff now waits 240s, covering the 180s MCP startup budget plus margin.
- Context: Codex keeps the existing 10s Node.js runtime launch default, direct runtimes keep the 2s default, and env-var overrides still take precedence.

## Diff summary

- Commits: `8c921fa56` (code change; this summary is committed as a sibling session-recording commit)
- Files touched: `crates/caco-daemon/src/agent/mod.rs`, `crates/caco-daemon/src/agent/tests.rs`, `SPEC.md`
- Tests: +0 / -0 / flipped 0; ran `cargo fmt --all -- --check` and `cargo test -p caco-daemon runtime_launch_timeout --lib`.
- Behavioural delta: slow-but-healthy Pi managed sessions on MS nodes get longer startup and resume readiness windows before being classified as runtime launch or provider-ready failures.

## Operator-takeaway

The short-term fix is deliberately narrow: it lengthens only the Pi-specific timing path implicated by the MS-node false failures, while preserving fast failure for direct runtimes and the established Codex timeout.
