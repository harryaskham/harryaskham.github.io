# Session summary — remote TUI PTY attach stops shelling through caco ssh

## Goal

Fix the remote TUI PTY preview/attach path that could fail in service-managed environments because the TUI spawned `caco ssh ...` and depended on the child process finding `caco` in PATH. The bead specifically asked to avoid using `caco ssh` for hot PTY SSH connections and instead use the node/attach metadata already available in the TUI.

## Bead(s)

- `bd-7cf3e8` — Remote TUI PTY preview cannot find caco in service PATH

## Before state

- Failing tests: none known for the touched TUI path before the change.
- Relevant metrics: one ready bead remained after the previous bd-2994e4 landing; this bead was priority P2 and operator-filed.
- Context: remote TUI attach/preview built a PTY child as the current `caco` binary with arguments equivalent to `caco ssh <node> -- -t <remote tmux command>`. That re-ran CLI node resolution and could fail when a service PATH could not find the intended `caco`, even though the TUI attach metadata already carried node, SSH destination, port, and key.

## After state

- Failing tests: none in the focused queued validation. An initial validation attempt used an incomplete exact Cargo filter and correctly failed as zero-tests; the fully-qualified retries passed.
- Relevant metrics: remote PTY attach/preview now builds direct `ssh` arguments with daemon-resolved destination, port, and key plus OpenSSH ControlMaster options using the same per-process control-path shape as the existing TUI SSH pool.
- Context: the CLI raw remote attach contract still uses the managed/current `caco` binary where appropriate, but the TUI hot preview/attach path no longer shells through `caco ssh` or depends on a remote `caco` PATH lookup.

## Diff summary

- Code/content commits: `c94196ce0` (`bd-7cf3e8: use direct ssh for remote tui pty`)
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched: `crates/caco-tui/src/app.rs`, `crates/caco-tui/src/state/mod.rs`, `SPEC.md`, `.cacophony/agent/winmini-cacophony-caco-dev-wmi-2/summary/pending/summary.md`
- Tests: +1 focused TUI unit test; no tests removed or flipped.
- Validation:
  - `git diff --check`
  - queued `cargo test -p caco-tui --lib remote_ssh_attach_uses_direct_ssh_with_controlmaster_bd_7cf3e8 -- --exact` — job `tj-3cd42cff` failed validation because the exact filter ran zero tests
  - queued `cargo test -p caco-tui --lib app::tests::remote_ssh_attach_uses_direct_ssh_with_controlmaster_bd_7cf3e8 -- --exact` — job `tj-db706b9b` passed
  - queued `cargo test -p caco-tui --lib app::tests::attach_remote_agent_attempts_ssh_attach -- --exact` — job `tj-a6d656cc` passed
- Behavioural delta: TUI remote PTY preview/attach now invokes `ssh` directly with reusable ControlMaster settings and attach metadata, avoiding duplicate `caco ssh` resolution and service-PATH failures.

## Operator-takeaway

The reported PATH failure was in the TUI's transport choice, not in the remote tmux command itself. Remote preview/attach now uses the same authoritative attach metadata and SSH connection shape the TUI already maintains, so a working SSH/tmux target should no longer be derailed by a missing `caco` binary in the service environment.
