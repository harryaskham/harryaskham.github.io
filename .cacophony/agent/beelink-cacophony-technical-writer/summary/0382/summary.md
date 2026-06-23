# Technical-writer review summary

## Goal

Document a verified gate-coverage gap that caused repeated fleet confusion: the
reintegration gate's `cargo test-small` excludes four crates, so runtime test
failures in them are non-gating.

## Bead(s)

- `bd-c63005` — technical-writer documentation maintenance.
- Context: bd-38bab0 (non-gating caco-cli test failures), bd-ff92cd (cross-platform gate dimension).

## Before state

- AGENTS.md described the reintegration gate's `cargo test-small` but did not note it excludes `caco-cli`/`caco-daemon`/`caco`/`caco-sidecar` from the test run. Multiple controllers flip-flopped (green→hold→green) on a gate question because of this, until they read `.cargo/config.toml:52`.

## After state

- AGENTS.md now records the gate-coverage boundary: `test-small` is `test --workspace --lib --bins --exclude caco-cli --exclude caco-daemon --exclude caco --exclude caco-sidecar`, so it does not RUN those four crates' unit tests; since `cargo check --workspace --tests` only compiles (not runs) tests, a runtime test FAILURE in those crates is non-gating (lands on main, surfaces only under full `cargo test` / `just test-large`), while compile breaks are still caught except the linux-gate-misses-darwin platform-cfg dimension.
- Verified against `.cargo/config.toml:52` and corroborated by ms-dev-2-ctrl/pico-ctrl/tui-md2-1 code-reads.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `AGENTS.md`.
- Behavioural delta: documentation only.

## Operator-takeaway

The gate's test-small exclusion list is now documented so future agents/operators
know caco-cli/caco-daemon/caco/caco-sidecar runtime test failures are real
broken-on-main but non-gating — preventing the green/hold confusion that recurred.
