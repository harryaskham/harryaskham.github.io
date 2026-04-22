# Session summary — caco-cli run() universal stack-budget fix

## Goal

Eliminate the recurring class of `[broken-on-main]` test-failure beads where
`cargo test -p caco-cli --lib <some_help_test>` aborts with a stack overflow
(SIGABRT) on a fresh debug build. The same root cause has been patched in
isolation at least twice (bd-a91f0f for `choices_mcp_tools_call_dispatches_*`
and now flaring on `agent_help_lists_audit_reintegration_subcommand` as
bd-e4f3e3 plus `agent_attach_help_shows_id_and_raw_args` as flagged by a
peer agent). Move the fix one layer up so it stops re-occurring.

## Bead(s)

- `bd-e4f3e3` — `[broken-on-main] agent_help_lists_audit_reintegration_subcommand stack overflow`
- Same-class peer report (no separate bead needed): `agent_attach_help_shows_id_and_raw_args`
- Predecessor of the same class, already closed: `bd-a91f0f`, `bd-156aee`

## Before state

- Failing tests on a clean checkout (HEAD `b0fcfbe1`):
  - `cargo test -p caco-cli --lib agent_help_lists_audit_reintegration_subcommand` → `thread … has overflowed its stack / fatal runtime error: stack overflow / SIGABRT`
  - `cargo test -p caco-cli --lib agent_attach_help_shows_id_and_raw_args` → same crash
  - Running the broader caco-cli suite SIGABRTs mid-suite on
    `tests::bd_send_request_*_transport_error_returns_envelope` and
    `tests::bd_daemon_result_routes_transport_error_envelope` (also pre-existing).
- Per-test workarounds in tree:
  - `tests::run_help_test_with_large_stack` helper (8 MB) used by ~8 help tests.
  - `mcp_server::call_tool` open-coded its own 16 MB stack thread (bd-a91f0f).
- Release builds passed (`cargo test -p caco-cli --release`); only debug-mode
  thread stacks were too small for the `dispatch()` match (~1300 arms).

## After state

- `cargo test -p caco-cli --lib agent_attach_help_shows_id_and_raw_args agent_help_lists_audit_reintegration_subcommand` → both pass.
- `cargo clippy -p caco-cli` clean.
- `cargo build -p caco-cli` clean.
- The previously-broken transport-error tests (`bd_send_request_*`,
  `bd_daemon_result_routes_transport_error_envelope`) remain pre-existing
  failures — verified by stashing this change and reproducing them on a
  clean tree. Out of scope for this bead; will surface separately if no
  existing bead covers them.

## Diff summary

- Commit: `46b827f6` — `bd-e4f3e3: lift 16 MB stack-thread wrapper into caco-cli run()`
- Files touched:
  - `crates/caco-cli/src/lib.rs` — split `pub fn run` into a thin spawner
    (`std::thread::Builder::new().stack_size(16 MB).spawn(run_inner)`) plus
    `fn run_inner` that holds the existing dispatch body. Args are
    materialized into `Vec<OsString>` before crossing the thread boundary so
    callers don't need `Send` iterators. Panics from the dispatch thread
    are propagated via `resume_unwind` so the existing panic semantics of
    `run()` are preserved.
  - `crates/caco-cli/src/mcp_server.rs` — drop the now-redundant per-call
    16 MB stack-thread wrapper added by bd-a91f0f and just call
    `crate::run(os_args)` directly. Added a comment pointing back to
    bd-e4f3e3 / bd-a91f0f for the history.
- Tests: 0 added, 0 removed; 2 previously-SIGABRT tests now run and pass.
- Behavioural delta: every entry into `caco_cli::run` — release binary,
  debug binary, unit tests, MCP `call_tool` re-entry — runs the dispatch
  body on a dedicated 16 MB stack thread named `caco-cli-run`. No
  observable behaviour change for callers; just a removed footgun.

## Embedded artefacts

(none — this is a pure code/test-infrastructure fix; no terminal capture or
screenshots add information beyond the diff and the tests-pass observation.)

## Operator-takeaway

The "broken-on-main caco-cli help test stack overflow" pattern is a recurring
class, not a one-off: the `dispatch()` match is so large that any debug
build with a non-trivial caller frame on top will overflow the default
8 MB pthread stack on macOS. Patching individual tests (or individual
re-entry points like `mcp_server::call_tool`) just kicks the can. With this
change `caco_cli::run` always runs on a 16 MB stack thread, so the entire
class is closed at the source. If a future help-style test ever
SIGABRT's again, the right fix is to grow `RUN_DISPATCH_STACK_SIZE` in
`crates/caco-cli/src/lib.rs`, not to add another per-test workaround.
