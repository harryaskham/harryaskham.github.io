# Technical-writer review summary

## Goal

Fix doc-drift: the queued cargo `CARGO_BUILD_JOBS` default changed from a flat 2
to CPU-proportional (bd-233590), but AGENTS.md, README.md, and docs/cli.html
still said "default 2".

## Bead(s)

- `bd-c63005` — technical-writer documentation maintenance.
- Documents bd-233590 (proportional queued cargo parallelism).

## Before state

- AGENTS.md, README.md, and `docs/cli.html` described the queued cargo `CARGO_BUILD_JOBS` cap as a fixed "default 2" with only `CACO_QUEUED_CARGO_BUILD_JOBS` to tune it.

## After state

- All three now describe the verified bd-233590 contract: a core-count-proportional default of `floor(nproc/2)`, clamped to a floor of 2 and a default max of 4, env-tunable via the new `CACO_QUEUED_CARGO_BUILD_JOBS_MAX`, with macOS/ms-mac pinned to the floor; an explicit `CACO_QUEUED_CARGO_BUILD_JOBS` still wins (and `0` disables injection). `RUST_TEST_THREADS` shares the same proportional default.
- Verified against `crates/caco-daemon/src/queued_job_env.rs` (`proportional_queued_cargo_build_jobs`, `effective_queued_cargo_build_jobs_max`, bd-233590).
- Validation: `./docs/validate-pages.sh` passed.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `AGENTS.md`, `README.md`, `docs/cli.html`.
- Behavioural delta: documentation only.

## Operator-takeaway

The queued-cargo parallelism docs now match the shipped CPU-proportional behavior,
including the new `CACO_QUEUED_CARGO_BUILD_JOBS_MAX` clamp and the macOS floor pin.
