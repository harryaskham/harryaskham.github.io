# Session summary — bd-149a3b empty bead show id guard

## Goal

Burn down the next contained bead-surface validator leak by making `caco bd show --bead-id ''` fail cleanly at the CLI boundary instead of round-tripping an empty ID to the daemon and surfacing a 404-plus-EOF parser error.

## Bead(s)

- `bd-149a3b` — `caco bd show --bead-id ''` leaks HTTP 404 + serde EOF instead of rejecting empty input up front

## Before state

- Failing tests: none in scope before the fix; this was a bad error-path behaviour on a mature CLI surface.
- Relevant metrics: `dispatch_bd_show(...)` accepted `--bead-id` as-is, so an empty string produced `/beads/` on the daemon URL and bubbled back `invalid response (HTTP 404 Not Found): EOF while parsing a value at line 1 column 0`.
- Context: sibling surfaces already used the shared `validate_non_empty_id(...)` helper to stop this exact trailing-slash + empty-body leak pattern before the request was sent.

## After state

- Failing tests: none observed in the focused `caco-cli` coverage.
- Relevant metrics: `dispatch_bd_show(...)` now applies `validate_non_empty_id("--bead-id", ..., "caco bd show", Some("caco bd list"))` before building the daemon URL.
- Context: the rebuilt CLI now reports `error: --bead-id must not be empty for caco bd show (list available: caco bd list)` instead of leaking transport and parser internals.

## Diff summary

- Commits: `f11974339`
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: `cargo test -p caco-cli dispatch_bd_show_uses_non_empty_bead_id_validator_bd_149a3b -- --nocapture`; `cargo test -p caco-cli validate_non_empty_id_shape -- --nocapture`; `cargo build -p caco`; `./target/debug/caco bd show --bead-id ''`
- Behavioural delta: empty bead IDs for `caco bd show` are now rejected client-side with the shared non-empty-ID wording and a discovery hint to `caco bd list`.

## Operator-takeaway

This was another clean CLI-boundary hardening slice: one shared validator call closed the error leak without touching daemon behaviour, and the manual rebuilt-binary repro now matches the intended operator-facing contract.
