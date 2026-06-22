# Session summary — bd-45114c: auto-skip the reintegration gate for zero-Rust-surface lands

## Goal

Stop gate-redundant lands (docs + version-string-only Cargo bumps, e.g. the update-helper version-bump cadence) from queuing a reintegration gate test job and congesting the gate queue — the same congestion class that drives the bd-0ffc21 staleness livelock my keystone fixed. Auto-detect the zero-Rust-compile-surface class and skip the gate, conservatively (fail-safe to gating).

## Bead(s)

- `bd-45114c` — Auto-skip reintegration gate for zero-Rust-surface lands (docs/version-bump-only) — complements bd-0ffc21. (Filed by update-helper; claimed + landed by me on ctrl's go to take ready reint-adjacent work.)

## Before state

- `classify_gate_skip_tier` (reintegration_gate.rs) had SkipPureDocsState / ConfigValidateOnly / SkipNativeSurface / FullGate. `classify_path` lumps any Cargo.toml/Cargo.lock into CodeOrUnknown -> FullGate, so a pure docs+version-bump land (CHANGELOG/docs + Cargo version strings) still ran the full cargo gate (test-small/check/clippy) and enqueued a gate test job under load.
- v1.2.1329 had to be manually --skip-hooks'd to escape exactly this.
- Failing tests: none.

## After state

- A docs + version-string-only Cargo bump auto-skips the gate (no test job enqueued); ANY .rs / non-version Cargo / build.rs / flake / config / native / ambiguous change still runs the gate normally.
- cargo check --workspace --tests: green (post-rebase, tj-6e11a446). bd-45114c tests: 5/5 green (tj-e1148f09).
- Failing tests: none.

## Diff summary

- Code commit: landed squash SHA from the reintegration receipt (pre-squash agent tip 4518c9ec8).
- Files touched: `crates/caco-daemon/src/reintegration_gate.rs` only (deliberately SEPARATE from the serialized reintegration.rs batch bd-ade262/bd-6e9810).
- Added (conservative, fail-safe): `line_is_version_assignment`, `cargo_manifest_diff_is_version_bump_only` (full-context -U100000 diff, section-tracked: skips ONLY package `version="..."` in [package]/[workspace.package]/[[package]]; dep version, inline dep, checksum, source, feature, profile, changed header, or dependency-table version -> gate), `zero_rust_surface_cargo_candidates` (all paths docs OR Cargo manifest, else None), `cargo_changes_are_version_bump_only` (git-backed, fail-safe on any git error/non-version change). New `GateSkipTier::SkipZeroRustSurface`; `run_reintegration_gate` downgrades FullGate->skip only when docs+version-bump-only, with an EPIPE-safe audit line. `gate_commands_need_worktree` handles the new tier.
- Tests: +5 (4 unit: version-field detection, version-bump skips, dep/edition/checksum/header/empty gate, candidate detection; 1 end-to-end run_reintegration_gate with a FAILING gate so a wrong skip Passes where a correct gate-run Rejects).
- GateSkipTier is used only in reintegration_gate.rs (grep-confirmed) -> the new variant cannot cause a bd-72198a-class non-exhaustive break elsewhere.

## Embedded artefacts

None.

## Operator-takeaway

bd-45114c removes the gate-redundant land class (docs + workspace version bumps) from the reint gate queue entirely, complementing bd-0ffc21's staleness fix at the source: instead of escaping the livelock with manual --skip-hooks (as v1.2.1329 had to), version-bump cadence lands now skip the irrelevant cargo gate automatically. Detection is deliberately conservative — it fails safe to running the gate on any ambiguity, so a broken-code land can never sneak through via a false "no-Rust" classification.
