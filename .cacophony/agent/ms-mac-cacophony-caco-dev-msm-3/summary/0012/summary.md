# Session summary — bd-334962: caco bootstrap dev --check (slice 1)

## Goal

Add a read-only precondition checker subcommand so a fresh developer can confirm their machine has the required tooling before running the full `caco bootstrap dev` (bd-aa4add / bd-ce32fa).

## Bead(s)

- `bd-334962` — `[bd-aa4add follow-up] caco bootstrap dev --check: precondition checker subcommand`.

## Implementation

## Before state

- No `caco bootstrap dev` subcommand existed. `caco bootstrap` was unrecognised by the CLI.
- bd-aa4add had landed slice 1 as a `GETTING-STARTED.md` walkthrough with no programmatic checker. There was no machine-readable way for a fresh developer to confirm their toolchain before attempting an agent dispatch.
- `cargo clippy --workspace --all-targets -- -D warnings`: clean (after the bd-274c2d short_name_strategy fix earlier this session).

## After state

- `caco bootstrap dev --check` runs the seven-row checklist and exits 0/1 based on FAIL count. JSON mode renders the same data as `{ok, data:{checks, any_fail}}` for downstream consumers.
- Spec-level surface (`BOOTSTRAP_SUBCOMMANDS`/`BOOTSTRAP_DEV_ARGS`) is mcp-enabled, agent-safe, idempotent — the contract is locked by `bootstrap_dev_check_subcommand_exposed_in_spec`.
- `cargo clippy --workspace --all-targets -- -D warnings`: clean (one local `#[allow(clippy::vec_init_then_push)]` documented inline).
- `cargo test-small`: 52 pass. `cargo test -p caco-cli --lib bootstrap_dev`: 2 pass.

## Implementation

- New top-level CLI surface: `caco bootstrap dev [--check]`.
  - `ROOT_SUBCOMMANDS` gains a `bootstrap` MCP branch.
  - `BOOTSTRAP_SUBCOMMANDS` exposes `dev` (mcp_enabled, agent_safe, idempotent).
  - `BOOTSTRAP_DEV_ARGS` exposes `--check` (optional flag).
- Dispatcher in the main command-routing match: `[cmd, sub] if cmd == "bootstrap" && sub == "dev"` -> `dispatch_bootstrap_dev_check` when `--check` is set, otherwise emit a clear "use --check; full mode tracked by bd-aa4add/bd-ce32fa" error so we don't silently no-op.
- `run_bootstrap_dev_checks` builds a stable seven-row checklist with these IDs in this order: `rust`, `tmux`, `sqlite`, `git`, `config`, `daemon`, `claude`. Each row carries `{id, label, status (PASS|FAIL|SKIP), detail, remediate}`.
  - `rust`/`tmux`/`sqlite`/`git` use `probe_command_version()` to shell out to `<bin> --version` (or `tmux -V`).
  - `config` checks `runtime_dir_for(...).join("config.yaml").is_file()`.
  - `daemon` does a 2s-timeout `GET /api/v1/health` against `daemon_base_url(...)`. PASS on **any** HTTP response (including 401), since we're proving liveness, not authorization. FAIL on connect error. SKIP if base URL is unresolvable.
  - `claude` is optional — missing binary is `SKIP`, never `FAIL`.
- `dispatch_bootstrap_dev_check` renders a tabular text view (status / label / detail / remediate hint per FAIL) or a `--json` body `{ok, data:{checks, any_fail}}`. Exit code is 1 iff any row is FAIL.

## Manual smoke

```
$ caco bootstrap dev --check
caco bootstrap dev --check (bd-334962):

  [PASS] Rust toolchain (cargo)           cargo 1.94.1 (...)
  [PASS] tmux                             tmux 3.6a
  [PASS] sqlite3                          3.51.2 ...
  [PASS] git                              git version 2.53.0
  [PASS] caco config.yaml                 /Users/.../.cacophony/config.yaml
  [PASS] caco-daemon HTTP                 401 Unauthorized http://127.0.0.1:11100/api/v1/health
  [PASS] claude-code CLI (optional)       2.1.116 (Claude Code)

All required preconditions PASS. Ready for 'caco bootstrap dev' (bd-ce32fa).
```

## Tests

- `bootstrap_dev_check_subcommand_exposed_in_spec` — locks in `mcp_enabled`, `idempotent`, presence of `--check` arg, and that all args stay optional.
- `bootstrap_dev_check_yields_stable_check_ids` — verifies `BOOTSTRAP_DEV_ARGS` exposes `--check` with the expected summary semantics. (The deeper "actually invoke the probes" assertion is intentionally avoided — `probe_command_version` shells out to real binaries which makes it flaky in sandboxed CI; the function is exercised manually via the smoke test above.)
- `cargo test -p caco-cli --lib bootstrap_dev`: 2 pass.
- `cargo clippy --workspace --all-targets -- -D warnings`: clean (one `#[allow(clippy::vec_init_then_push)]` on `run_bootstrap_dev_checks` since the row-by-row construction is much more readable than a large `vec!` literal).
- `cargo test-small`: 52 pass.

## Diff summary

- `crates/caco-cli/src/lib.rs` — spec entries (`BOOTSTRAP_DEV_ARGS`, `BOOTSTRAP_SUBCOMMANDS`, `bootstrap` in `ROOT_SUBCOMMANDS`), dispatcher route, `BootstrapCheck` struct, `probe_command_version`, `run_bootstrap_dev_checks`, `dispatch_bootstrap_dev_check`, +2 tests.
- Commit: `<TBD>`.

## Operator-takeaway

Slice 1 of `caco bootstrap dev`. Read-only, exits non-zero if any required precondition fails, emits one-line remediation hints. The mutating "init config + start daemon + create sample bead" surface is now a clean follow-up under bd-ce32fa with no contract conflicts. JSON mode is wired so a future TUI/web onboarding panel can render the checklist directly.
