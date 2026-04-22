# Session summary — caco-cli transport-error tests stack-budget fix

## Goal

Close the second wave of `[broken-on-main]` caco-cli stack-overflow tests
that became visible once bd-e4f3e3's `pub fn run()` 16 MB wrapper let the
suite run far enough to surface them. Same root-cause family as bd-e4f3e3 /
bd-a91f0f, but the failing tests do not go through `run()` — they call
`transport_error_envelope` / `bd_send_request` / `bd_daemon_result`
directly, so the previous fix did not reach them.

## Bead(s)

- `bd-a7441a` — `[broken-on-main] caco-cli transport-error tests SIGABRT in transport_error_envelope sidecar-fallback path`
- Predecessors in the same class: `bd-e4f3e3`, `bd-a91f0f`, `bd-156aee`

## Before state

- Failing tests on a clean checkout (HEAD `f1ef26ab`):
  - `tests::transport_error_envelope_shape` → SIGABRT, stack overflow
  - `tests::bd_send_request_json_transport_error_returns_envelope` → SIGABRT
  - `tests::bd_send_request_text_transport_error_returns_cli_error` → SIGABRT
  - `tests::bd_daemon_result_routes_transport_error_envelope` → SIGABRT
- `cargo clippy -p caco-cli`: clean.
- Why: each test invokes `transport_error_envelope`, which falls back via
  `daemon_restarting_envelope_from_sidecar` → `probe_sidecar_lifecycle_blocking`
  → `sidecar_base_url_for` → `load_config_novalidate_for`, loading the
  full caco config on the calling thread. macOS's default ~2 MB pthread
  test stack overflows that path. Production callers always go through
  `pub fn run()` (bd-e4f3e3), which already runs on a 16 MB thread, so
  this is a pure test-isolation issue.

## After state

- All four tests pass under `cargo test -p caco-cli --lib --
  <test_name> --test-threads=1`.
- Existing bd-e4f3e3 fixes (`agent_attach_help_shows_id_and_raw_args`,
  `agent_help_lists_audit_reintegration_subcommand`) still pass.
- `cargo clippy -p caco-cli`: clean.
- `cargo build -p caco-cli`: clean.

## Diff summary

- Commit: `e0b4c7b4` — `bd-a7441a: wrap transport-error tests in 16 MB stack helper`
- File touched:
  - `crates/caco-cli/src/lib.rs` — three changes:
    1. Bump `tests::run_help_test_with_large_stack` from 8 MB to
       `super::RUN_DISPATCH_STACK_SIZE` (16 MB), aligning the test-budget
       knob with the production-budget knob from bd-e4f3e3.
    2. Wrap the four affected tests' bodies in
       `run_help_test_with_large_stack(|| { ... })`.
    3. Broaden the envelope-code assertions to accept any of the three
       valid transport-failure envelope codes
       (`transport_error`, `daemon_restarting`, `daemon_down`). This is
       behaviour-preserving: the bd-88ddd7 sidecar fallback is allowed to
       upgrade the envelope when a live sidecar is reachable on the test
       host, and the test contract is the structural shape (failure
       envelope, retryable flag is a boolean, exit code 1 in JSON mode),
       not a specific string code.
- Tests: 0 added, 0 removed; 4 previously-SIGABRT tests now run and pass.
- Behavioural delta: none in production — only test infrastructure and
  test assertions are touched.

## Embedded artefacts

(none — pure test-infrastructure follow-up to bd-e4f3e3.)

## Operator-takeaway

bd-e4f3e3 fixed the dispatch-tree stack overflow at the *production*
boundary (`pub fn run`). bd-a7441a closes the symmetric *test*-side
problem for code paths that the tests poke directly without going through
`run()`. Both fixes pivot on a single 16 MB constant
(`RUN_DISPATCH_STACK_SIZE`), so future stack-overflow regressions in
caco-cli have one knob to adjust. If a future test in caco-cli ever
SIGABRT's in this same family, wrap it with `run_help_test_with_large_stack`
or — better — bump `RUN_DISPATCH_STACK_SIZE` once and let both surfaces
absorb it.
