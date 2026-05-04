# Session summary — state-only rebase cleanup

## Goal

Fix `caco agent rebase` so persistent agents do not remain locally ahead with already-published `.cacophony/agent/**` summary artefact commits after a successful direct reintegration and later rebase.

## Bead(s)

- `bd-e65f72` — Direct reintegration leaves persistent checkout ahead with state-only summary commits

## Before state

- A persistent agent could direct-reintegrate work, publish summaries to `cacophony-state`, then run `caco agent rebase` and replay a summary-only commit onto the current target.
- The checkout would appear ahead even though the only ahead commits were already-recorded state artefacts, creating confusing future reintegration and attribution state.

## After state

- `caco agent rebase` now detects when every commit ahead of the fetched authoritative target touches only `.cacophony/agent/**` paths.
- In that state-only shape, it resets the checkout back to the target and reports the cleanup in human and JSON output.
- If any ahead commit touches ordinary code or other repository paths, cleanup is skipped and the code commit is preserved.
- SPEC, README, AGENTS, and Pages reintegration policy docs describe the new first-party cleanup behavior.

## Diff summary

- Files touched: `crates/caco-cli/src/lib.rs`, `SPEC.md`, `README.md`, `AGENTS.md`, `docs/reintegration-policy.md`, `docs/reintegration-policy.html`
- Tests: added focused `caco-cli` unit coverage for state-only cleanup and for preserving code commits.
- Validation: `cargo test -p caco-cli agent_rebase_state_only_cleanup -- --test-threads=1`; `cargo check -p caco-cli --lib`; `rustfmt --edition 2021 --check crates/caco-cli/src/lib.rs`; `git diff --check`; `docs/validate-pages.sh`.
- Behavioural delta: first-party rebase now converges published summary-only checkout drift without asking persistent agents to do manual reset or cherry-pick salvage.

## Operator-takeaway

If a persistent checkout is ahead solely because recorded summaries were already published to `cacophony-state`, the normal `caco agent rebase` recovery path now cleans it up safely and visibly, while leaving real code commits untouched.
