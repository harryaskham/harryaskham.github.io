# Session summary — remote raw attach env portability

## Goal

Fix the operator-reported remote raw agent attach failure where `caco agent attach --raw` reached a remote node over SSH but the remote command failed with `env: '-u': No such file or directory`, preventing direct tmux attachment to remote agents.

## Bead(s)

- `bd-54a3c6` — Remote raw agent attach fails when SSH command uses env -u on target

## Before state

- Failing command: `caco agent attach --id ms-dev-2-agent-utils-agnt-dev-ms2-0 --raw` from ms-mac reached ms-dev-2 but exited with `env: '-u': No such file or directory`.
- Root cause context: the CLI remote raw attach command rendered `exec env LANG=en_US.UTF-8 -u TMUX_TMPDIR -u TMUX ...`, which is not portable because some `env` implementations stop parsing options after the first `NAME=VALUE` assignment.
- Existing contract: `SPEC.md` and `README.md` require `--raw` to attach directly to the underlying tmux pane locally or over SSH for remote agents.

## After state

- The remote raw attach command now renders `exec env -u TMUX_TMPDIR -u TMUX LANG=en_US.UTF-8 tmux ...`, keeping `env` options before assignments so remote implementations do not treat `-u` as the command.
- Existing JSON attach metadata and command-construction tests were updated to enforce the portable ordering.
- The focused queued caco-cli test passed.

## Diff summary

- Code/content commits: `51bed53a9` (`bd-54a3c6: make remote raw attach env command portable`)
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: focused queued test `tj-71fef391` passed: `RUST_MIN_STACK=33554432 cargo test -p caco-cli --lib remote_agent_attach -- --test-threads=1`
- Behavioural delta: remote raw attach preserves the locale export and `exec` guardrail while making the `env -u` usage portable across target nodes.

## Operator-takeaway

The failure was a command-ordering portability bug, not an SSH reachability issue. Moving `env -u` options before `LANG=...` should unblock raw remote agent attachment on nodes whose `env` rejects options after assignments.
