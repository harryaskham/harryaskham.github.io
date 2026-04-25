# Session summary — bd-45c709 filtered unknown-subcommand Allowed lists

## Goal

Stop agent-context CLI typo errors from leaking operator-only subcommand names through the inline `Allowed:` list while preserving the existing discoverability wording for visible subcommands.

## Bead(s)

- `bd-45c709` — Unknown-subcommand `Allowed:` error list leaks hidden-in-agent-context operator-only subcommands

## Before state

- Text help already hid `agent_safe: false` subcommands in agent context and appended the explicit hidden-count footer.
- The unknown-subcommand dispatcher path ignored that same visibility filter and listed every static subcommand in `Allowed:`.
- Repro families included `tts daemon`, all `codespace` lifecycle verbs, `checkout regenerate`, and `choices tui` / `serve-ttyd`.

## After state

- Added a shared `visible_subcommand_names_for_errors` helper that reuses the same agent-context visibility path as text help.
- Unknown-subcommand errors now list only visible subcommands, or `Allowed: none in this context` when a branch has no agent-visible children.
- Added a regression test covering `tts`, `codespace`, `checkout`, and `choices` under an explicit agent-context environment.

## Diff summary

- Commits: pending reintegration commit for `bd-45c709`.
- Files touched: `crates/caco-cli/src/lib.rs`.
- Tests: `cargo test -p caco-cli --lib unknown_subcommand_allowed_list_hides_agent_unsafe_subcommands`; existing unknown-subcommand tests for choices, mcp, and update; `cargo check -p caco-cli --tests`; `cargo fmt --all -- --check`; `git diff --check`.
- Behavioural delta: typo guidance is now consistent with help visibility and no longer exposes hidden agent-unsafe command names.

## Operator-takeaway

Agent-mode `Allowed:` errors now agree with `--help`: hidden operator-only verbs stay hidden, while visible subcommands still get inline typo guidance.
