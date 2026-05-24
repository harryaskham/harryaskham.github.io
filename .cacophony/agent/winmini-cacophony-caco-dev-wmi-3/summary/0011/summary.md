# Session summary — bd-f3929a direct-integration disk preflight

## Goal

Make direct reintegration fail early with actionable first-party cleanup guidance when the filesystem backing the temp checkout root is too full to safely clone the isolated integration checkout.

## Bead(s)

- `bd-f3929a` — winmini root filesystem fills and blocks reintegration checkouts

## Before state

- winmini root filled to 100% / about 2 GiB free, causing another worker's direct reintegration to fail mid-clone while writing many files under a `nix-shell` temp root.
- Existing direct reintegration already pruned stale `caco-direct-integration-*` tempdirs, but it did not check free space before creating and cloning the isolated integration checkout.
- Operational relief was performed separately via first-party current-agent Cargo-target pruning; this code slice focuses on preventing the next cryptic clone failure.

## After state

- `prepare_isolated_integration_checkout` now prunes stale direct-integration tempdirs, then checks free bytes on `std::env::temp_dir()` before creating the isolated checkout.
- If the temp root has less than a 5 GiB safety floor, reintegration refuses before clone with a `bd-f3929a` error that names the temp root, current free bytes, the safety floor, and first-party cleanup options: current-agent Cargo-target prune, coordinated active-agent Cargo-target prune, and direct-integration tempdir preview/delete.
- Focused queued validation passed in job `tj-2416cb3b`: `RUST_MIN_STACK=33554432 CARGO_BUILD_JOBS=2 cargo test -p caco-daemon --lib direct_integration_low_temp_space_error_mentions_first_party_cleanup_bd_f3929a -- --test-threads=1`.

## Diff summary

- Code/content commits: `70e5f1b74` (`bd-f3929a: preflight direct integration temp free space`)
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched: `crates/caco-daemon/src/reintegration.rs`
- Tests: +1 focused source-level regression test / -0 / flipped 0
- Behavioural delta: low disk now produces an early, actionable direct-reintegration refusal rather than a noisy partial git clone failure after many file-write errors.

## Operator-takeaway

This does not automatically prune other running agents. It makes direct reintegration safer under disk pressure by refusing before clone and pointing operators/agents at the approved first-party cleanup paths that were used to recover winmini.
