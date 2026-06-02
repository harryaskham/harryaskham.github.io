# Session summary — Fix caco cron remote fan-out 'command not found: caco' (bd-fecc1f)

## Goal

An operator checkpoint surfaced a live failure: the hourly `codespace-keepalive`
cron dispatched fine on the local node (ms-mac) but failed on the remote nodes
ms-dev and helsinki with `zsh:1: command not found: caco`, with the operator
note "crons need path when run remotely, or this cron needs to locate binary
properly". The goal was to make `caco cron run` fan-out succeed on remote nodes
whose non-interactive SSH login shell does not have `caco` on PATH.

## Bead(s)

- `bd-fecc1f` — caco cron run remote fan-out fails with 'command not found: caco' (bare caco not on remote SSH PATH)

## Before state

- Failing tests: none (runtime/dispatch bug, not a test failure).
- `caco cron run` remote-dispatch branch in `dispatch_cron_run`
  (crates/caco-cli/src/lib.rs) built the remote command as a bare argv vec
  `vec!["caco", "cron", "run", "--name", name, "--local"]`. On remote
  non-interactive SSH login shells (zsh on ms-dev/helsinki) `caco` is not on
  PATH, so the cron fan-out failed with `command not found: caco` on every
  remote node while local dispatch worked. This affected ALL multi-node crons,
  not just codespace-keepalive.
- The sibling `restart_node_remote` path had already solved exactly this under
  bd-e9b445 via `resolve_remote_caco_binary` (config binary_path -> `which
  caco` -> well-known fallbacks like $HOME/.cargo/bin, $HOME/.nix-profile/bin,
  /run/current-system/sw/bin) invoked as a shell script. The cron fan-out was
  never given the same treatment.

## After state

- Failing tests: none.
- `dispatch_cron_run` now resolves the remote binary with
  `resolve_remote_caco_binary` and dispatches a shell-quoted script via a new
  `build_remote_cron_run_script(caco_bin, name)` helper, so remote cron
  dispatch survives login-shell PATH gaps. The cron name is shell-quoted.
- Validation (all green, via the daemon test queue):
  - `cargo test -p caco-cli --lib build_remote_cron_run_script` — 1 passed (new test)
  - `cargo test -p caco-cli --lib resolve_remote_caco_binary` — 2 passed
  - `cargo clippy -p caco-cli --lib` — clean

## Diff summary

- Code/content commit: pending final squash SHA from the reintegration receipt.
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: +1 (`build_remote_cron_run_script_uses_resolved_binary_and_quotes_name_bd_fecc1f`)
- Behavioural delta: remote cron fan-out now invokes the resolved remote `caco`
  binary path as a shell-quoted script instead of a bare `caco` argv, fixing
  `command not found: caco` on remote nodes for codespace-keepalive and all
  other multi-node crons.

## Operator-takeaway

The cron remote fan-out was the last `caco`-over-SSH dispatch path still using a
bare `caco` token instead of the bd-e9b445 `resolve_remote_caco_binary` helper
that `restart_node_remote` already used. This is why codespace-keepalive
silently failed on ms-dev/helsinki (the keepalive itself uses `gh`, not `caco`
— the failure was purely the remote cron *dispatch* shell not finding `caco`).
Any future `caco`-invoking-`caco`-over-SSH path should reuse
`resolve_remote_caco_binary`; a grep for hardcoded `"caco".to_owned()` in
SSH-bound argv vectors is a good audit for other latent instances.
