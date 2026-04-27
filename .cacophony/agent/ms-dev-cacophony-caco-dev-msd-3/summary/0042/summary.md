# Session summary — safe recorded-summary next-index helper

## Goal

Add a first-party way for agents to allocate the next recorded-summary directory without relying on fragile shell arithmetic or local-only directory scans. This supports the broader commit-path safety work by reducing recorded reintegration friction after summaries have been stripped from the code branch and published to `cacophony-state`.

## Bead(s)

- `bd-3a9c14` — Provide safe summary-index allocation helper

## Before state

- Failing tests: none in the focused bd-3a9c14 validation path.
- Relevant metrics: `docs/cli.html` had to remain under the 51200 byte Pages budget while documenting the new command.
- Context: agents were using ad-hoc shell snippets such as quoted `10#$INDEX` arithmetic while comparing local summaries with durable state-branch summaries, which caused an `integer expression expected` failure and accidental low-index allocation.

## After state

- Failing tests: none observed in focused validation.
- Relevant metrics: `docs/cli.html` is 50965 bytes, under the 51200 byte budget; the helper smoke test reported `next_index_padded: "0042"` for this agent after fetching `cacophony-state`.
- Context: `caco summaries next-index` now inspects both local `.cacophony/agent/<id>/summary/` directories and the durable `cacophony-state` branch, then prints the next zero-padded index. It also supports JSON output and defaults `--agent` from `CACO_AGENT_ID` / `CACOPHONY_AGENT`.

## Diff summary

- Commits: `6fd889254` (`bd-3a9c14: add safe summary next-index helper`)
- Files touched: `crates/caco-cli/src/summary_cmd.rs`, `crates/caco-cli/src/lib.rs`, `README.md`, `SPEC.md`, `AGENTS.md`, `docs/cli.html`, `docs/reintegration-policy.md`, `docs/reintegration-policy.html`
- Tests: +3 / -0 / flipped 0
- Behavioural delta: agents can run `caco summaries next-index --agent "$CACO_AGENT_ID"` before authoring recorded summaries, avoiding brittle manual parsing and arithmetic around zero-padded summary IDs.

## Operator-takeaway

The recorded-summary workflow now has a small first-party allocation command, so future agents should not need to hand-roll state-branch index comparisons when preparing safe recorded reintegration.
