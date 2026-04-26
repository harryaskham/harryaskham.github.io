# Session summary — explicit JSON boolean parsing

## Goal

Fix the CLI parser footgun where `--json true` left `true` behind as a positional argument, causing commands such as `caco bd show --json true` to treat `true` as a bead id and emit a misleading not-found error instead of recognizing the operator's intended JSON flag value.

## Bead(s)

- `bd-d351f8` — Reject or normalize boolean global flag values like --json true

## Before state

- Failing tests: no standing test; the issue was filed from a real operator/agent mistake during `bd-9bb0b2` work.
- Relevant metrics: `parse_command_path` treated bare `--json` as a boolean flag but did not consume immediately following `true` / `false`, so those tokens could enter command positionals.
- Context: the common user expectation from some generated/help surfaces is that `--json true` behaves like an explicit boolean spelling.

## After state

- Failing tests: none in the validation run.
- Relevant metrics: new parser regression proves `--json true` sets `json_requested = true` with no positional leakage and `--json false` consumes the value while leaving `json_requested = false`.
- Context: bare `--json` remains the canonical form; only immediate `true` / `false` values are consumed specially.

## Diff summary

- Commits: `58c35e652`
- Files touched: `crates/caco-cli/src/lib.rs`, `SPEC.md`
- Tests: +1 focused parser test; `cargo test -p caco-cli parse_command_path_accepts_explicit_json_bool_bd_d351f8 --lib`; `cargo check -p caco-cli`; `cargo clippy -p caco-cli` (completed with existing warning in `audio_cmd.rs`); `cargo test-small`.
- Behavioural delta: explicit boolean values after `--json` are now parsed as the JSON flag value rather than as command arguments.

## Operator-takeaway

The parser now normalizes a common accidental spelling, so a stray `true` after `--json` will no longer masquerade as a bead id or other positional and obscure the real command intent.
