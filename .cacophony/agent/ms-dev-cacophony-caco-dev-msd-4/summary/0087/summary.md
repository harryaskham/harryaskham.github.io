# Session summary — TUI summaries timeout

## Goal

Fix the TUI Summaries pane failing to load recorded-summary history with a generic HTTP transport error when the daemon needs longer than the ordinary interactive timeout to return the list.

## Bead(s)

- `bd-29d373` — TUI Summaries always fails to load

## Before state

- Failing tests: no focused timeout regression covered TUI summary history requests.
- Relevant metrics: direct daemon/CLI probes for `/api/v1/summaries?limit=200&offset=0` returned successfully but took roughly 9 seconds on the warm path and had previously exceeded the TUI client's default 10-second request budget on cold scans.
- Context: the TUI client used the shared default 10s timeout for summary list/detail fetches, which was too tight for recorded-summary state-branch scans.

## After state

- Failing tests: none in validation.
- Relevant metrics: TUI summary list and detail fetches now use a dedicated 60-second bounded timeout, with a regression test proving it exceeds the ordinary 10-second interactive timeout.
- Context: docs/spec now call out the extended bounded timeout for cold summary scans; the generated CLI docs paragraph from the prior exact-`@` work was removed to restore the published page size budget while README/SPEC retain that guidance.

## Diff summary

- Commits: `9b6c2e05e`
- Files touched: `crates/caco-tui/src/client.rs`, `SPEC.md`, `docs/tui.html`, `docs/cli.html`
- Tests: `cargo fmt --all -- --check`; `cargo test -p caco-tui summaries_timeout_exceeds_default --lib`; `cargo check -p caco-tui`; `docs/validate-pages.sh`; `cargo clippy -p caco-tui`; `cargo test-small`.
- Behavioural delta: the TUI Summaries pane should no longer fail during normal cold/warm recorded-summary list loads just because they exceed the generic control-plane timeout.

## Operator-takeaway

The Summaries pane now has a timeout budget aligned with the size of the recorded-summary history, so it should load rather than failing immediately with a transport error.
