# Session summary — exec managed remote SSH children

## Goal

Reduce the persistent macOS `(sshd-session)` zombie class tracked in `bd-6b85d3` by tightening Cacophony-managed remote SSH command construction: first-party remote `caco` dispatch and remote tmux attach paths should replace the login shell with the managed child process instead of leaving an extra shell layer for `sshd-session` to retain after detach or exit.

## Bead(s)

- `bd-6b85d3` — ms-mac sshd-session parents retain persistent defunct children

## Before state

- Failing tests: none known for this exact code path at session start.
- Relevant metrics: log-monitor evidence in the bead showed repeated persistent `(sshd-session)` parents with defunct children on ms-mac, sometimes high-count recurrences and later lower residual counts.
- Context: `@node` remote dispatch, `foreach node` remote fan-out, CLI raw remote attach, and TUI remote attach/preview launched managed remote commands through a remote shell command string without an explicit `exec` handoff.

## After state

- Failing tests: none in targeted queued validation.
- Relevant metrics: managed remote SSH command strings now use `exec ...` for first-party remote `caco` dispatch/fan-out and remote tmux attach/preview, reducing the process tree under macOS `(sshd-session)` parents by one login-shell child.
- Context: arbitrary `caco ssh <node> ...` passthrough remains unchanged; the guardrail is scoped to Cacophony-managed remote command construction and remote attach metadata/rendering.

## Diff summary

- Code/content commits: `a377507df`
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched: `crates/caco-cli/src/lib.rs`, `crates/caco-tui/src/app.rs`
- Tests: +2 / -0 / flipped 1 existing locale test expectation to include the new `exec` prefix
- Behavioural delta: remote `@node`/foreach-managed `caco` commands and CLI/TUI remote tmux attach commands now begin the remote shell command with `exec`; after rebasing over `bd-693bca`, the attach helper also preserves the new `LANG=en_US.UTF-8` locale export.
- Validation: post-rebase queued `cargo test -p caco-cli remote_agent_attach_json_uses_exec_tmux_command -- --test-threads=2` passed as `tj-e6cec84f`; queued `cargo test -p caco-cli shell_exec_command_prefixes_managed_remote_commands -- --test-threads=2` passed as `tj-68d4f5b8`; queued `cargo test -p caco-tui remote_agent_attach_command_exports_lang_bd_693bca -- --test-threads=2` passed as `tj-19d70b32`. Pre-rebase spot checks also passed for the two new caco-cli tests and `foreach_node_remote_uses_ssh`; an earlier version of the first new test failed in `tj-419fb477` because the expected shell quoting omitted the existing quoted `CACO_NODE=...` argument, then was corrected before the code commit.

## Operator-takeaway

This is a narrow guardrail rather than a destructive cleanup: it does not kill any active ms-mac sessions, but future Cacophony-managed remote attaches and remote command dispatches should leave fewer shell intermediates for macOS `sshd-session` to hold as persistent zombies.
