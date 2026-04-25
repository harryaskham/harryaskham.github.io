# Session summary — TUI diagnostic command naming

## Goal

Make the TUI command surface harder to misuse during visual audits by clearly separating the live dashboard, the real-dashboard benchmark harness, and the isolated graphics testbed. The immediate operator pain was an agent launching the isolated graphics testbed when the expected target was the real dashboard/audit surface.

## Bead(s)

- `bd-8dfaa5` — Rename TUI diagnostic/testbed commands so agents pick the intended audit target

## Before state

- Failing tests: none known at start.
- Relevant metrics: TUI help exposed `caco tui benchmark`, `caco tui fps-benchmark`, and `caco tui graphics-testbed`, but the old names/summaries did not strongly distinguish real-dashboard vs isolated chrome-only targets.
- Context: docs and operator guidance still recommended `graphics-testbed` directly, making it easy for agents to pick the wrong surface.

## After state

- Failing tests: none observed in targeted validation.
- Relevant metrics: `caco tui dashboard-benchmark` now names the real dashboard benchmark path; `caco tui isolated-graphics-testbed` names the chrome-only harness; `graphics-testbed` and `benchmark` remain compatibility aliases with explicit disambiguating help.
- Context: README, SPEC, AGENTS, docs, just recipes, and CLI metadata now all use the clearer names.

## Diff summary

- Commits: `a664254fc`
- Files touched: `crates/caco-cli/src/lib.rs`, `README.md`, `SPEC.md`, `AGENTS.md`, `docs/cli.html`, `docs/tui.html`, `justfile`
- Tests: `cargo fmt --all -- --check`; `git diff --check`; `cargo test -p caco-cli tui_ --lib`; `cargo check -p caco-cli --tests`; `cargo run -q -p caco -- help tui`; `cargo run -q -p caco -- help tui isolated-graphics-testbed`; `cargo run -q -p caco -- help tui dashboard-benchmark`; `docs/validate-pages.sh`
- Behavioural delta: no existing command is removed; clearer aliases and help text steer agents toward the real dashboard when that is the audit target and toward the isolated harness only for border/effect experiments.

## Operator-takeaway

The old commands still work, but future agents should see unambiguous help: use `caco tui` for the live dashboard, `caco tui dashboard-benchmark` for the real-dashboard benchmark, and `caco tui isolated-graphics-testbed` only for chrome-only graphics experiments.
