# Session summary — caco tui theme launch flag

## Goal

Implement the controller-filed `bd-c3a39a` feature so operators can launch the dashboard with a named theme using `caco tui --theme <name>` instead of verbose globals overrides or persistent config edits.

## Bead(s)

- `bd-c3a39a` — Add caco tui --theme shorthand for selecting named themes

## Before state

- Failing tests: none; this was an operator-requested ergonomics feature.
- Relevant metrics: `caco tui` already supported `--global tui.theme_name <name>` and `--extra-config-yaml`, but there was no first-class `--theme` flag and no direct unknown-theme guidance at launch.
- Context: named themes are registered under `tui.themes`, while node-level `nodes[].tui.theme_name` can override the global theme selection.

## After state

- Failing tests: none in focused validation.
- Relevant metrics: queued `cargo test -p caco-cli bd_c3a39a` passed after the latest first-party rebase as `tj-56cf9fef`; queued `cargo build -p caco` passed as `bj-c429ca56`; `git diff --check` passed.
- Context: `caco tui --theme <name>` now validates `<name>` against the effective registered theme set, applies it after global/node theme resolution for the launch, forwards it through `caco tui --node <node>`, and advertises it in CLI help/docs.

## Diff summary

- Commits: one bead commit (`bd-c3a39a: add tui theme launch flag`; final SHA recorded by git after commit creation).
- Files touched: `crates/caco-cli/src/lib.rs`, `README.md`, `SPEC.md`, `AGENTS.md`
- Tests: added focused caco-cli unit coverage for `--theme` parsing/help registration and theme registry validation/unknown-name guidance.
- Behavioural delta: operators can run `caco tui --theme enterprise-blueprint` for a session-local theme selection. Unknown names fail before entering the TUI with a message listing registered themes.

## Operator-takeaway

The dashboard theme switcher now has the expected direct CLI affordance: `caco tui --theme <registered-theme>` is a safe one-launch override, not a persistent config mutation, and it coexists with the existing lower-level `--global` and `--extra-config-yaml` experimentation paths.
