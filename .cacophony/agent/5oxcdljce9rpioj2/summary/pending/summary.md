# Session summary — persistent caco-macos env quoting fix

## Goal

Restore the persistent `caco-macos` startup path by finding why its managed tmux session exited immediately during `init.sh`, then landing a durable daemon-side fix so future generated agent environments can safely carry apostrophes in profile/runtime text.

## Bead(s)

- `bd-5ffab0` — Persistent caco-macos exits immediately during ms-mac startup

## Before state

- Failing tests: none known before this slice.
- Relevant metrics: `caco agent status --id ms-mac-cacophony-caco-macos` showed state `failed`; `caco agent logs --raw` only reached `init.sh starting` with no readiness sentinel; sourcing the generated env file reproduced `syntax error near unexpected token '('` at the `CACO_STOP_NUDGE_TEXT` line.
- Context: the generated `env.sh` wrote every value as `export KEY='{value}'` without escaping internal apostrophes. The endless-profile stop-nudge text contains phrases such as `operator's`, so Bash terminated the quote early and the persistent agent died before checkout bootstrap.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: new regression test `env_script_shell_quotes_apostrophes_in_values` writes a generated env script containing `operator's`, sources it through Bash, and verifies `parse_env_sh_vars` decodes the escaped value.
- Context: `write_env_script` now shell-quotes all generated environment values, including runtime defaults, PATH prepends, runtime dir, and extra env. The env parser now handles the standard shell-escaped apostrophe sequence.

## Diff summary

- Commits: pending at summary authoring time.
- Files touched: `crates/caco-daemon/src/agent/spawn.rs`, `crates/caco-daemon/src/agent/tests.rs`.
- Tests: added one daemon unit regression test.
- Behavioural delta: future managed agent recreates can source env.sh successfully even when profile/runtime strings contain apostrophes, preventing caco-macos and other persistent Pi agents from failing before readiness.

## Operator-takeaway

The caco-macos failure was not macOS/Tendril-specific; it was a generic env.sh quoting bug exposed by the current endless stop-nudge prompt. Once this daemon fix is running, recreating the persistent should generate a syntactically valid env.sh and allow normal startup.
