# Session summary — bd-f49a71 sccache devshell wiring regression-detector

## Goal

Close the prevention loop on bd-f49a71 (compile-fanout outage that
saturated helsinki) by adding a `caco doctor` regression-detector that
flips to `degraded` if any future commit drops the sccache wiring from
`flake.nix`'s default-devshell `shellHook`. The compile cache itself
is already wired (landed in fcaa0fe5); this session protects it from
silent regression.

## Bead(s)

- `bd-f49a71` — Compile fanout saturates helsinki: concurrent workers each run independent cargo builds with no shared cache
- (filed alongside: `bd-c19193` — [broken-on-main] agent_summary_exclude_routine_node_health stack overflow, observed pre-existing on main during validation)

## Before state

- Failing tests: `tests::agent_summary_exclude_routine_node_health_hides_mismatch_and_advisory` SIGABRTs with stack overflow on a clean main (filed as bd-c19193, not in scope here).
- `caco doctor` had no sensor for the sccache wiring. Coverage doc `docs/audits/bd-4fcf9c-doctor-outage-coverage.md` did not list bd-f49a71.
- `flake.nix` shellHook contains the three required exports (`RUSTC_WRAPPER`, `CARGO_INCREMENTAL=0`, `SCCACHE_DIR`) plus the `sccache` devshell package, but a future cleanup commit could silently drop them with no observability — exactly the regression shape from the original outage (`.envrc` resolved `command -v sccache` before `use flake`).
- Live sccache stats on the agent host show 66.75% overall hit rate (73.79% Rust), confirming the cache is active and the bead's root-cause mitigation is working.

## After state

- Failing tests: bd-c19193 (pre-existing) still fails on main; not addressed by this bead. All 57 `cargo test-small` tests pass. All 5 new sccache tests pass, including the live-flake regression-detector.
- `caco doctor` now includes section 10.11: `build` / `sccache devshell wiring`, with a static read of the repo `flake.nix` to detect missing tokens. Status flips to `warning` (degraded) if any of the four required tokens are absent.
- Coverage doc updated with a new bd-f49a71 row in the symptom→sensor table.
- CHANGELOG.md `[Unreleased]` entry added.

## Diff summary

- Commits: `e34cd495 bd-f49a71: caco doctor sccache devshell wiring regression-detector`
- Files touched:
  - `crates/caco-cli/src/lib.rs` (+~150 lines: `check_sccache_devshell_wiring`, `classify_sccache_wiring`, `locate_repo_flake`, section 10.11 in the doctor pipeline, 5 unit tests)
  - `docs/audits/bd-4fcf9c-doctor-outage-coverage.md` (+1 row)
  - `CHANGELOG.md` (+1 Unreleased entry)
- Tests: +5 / -0 / flipped 0
- Behavioural delta: `caco doctor` gains a new degraded-eligible signal when run from inside a cacophony checkout. Outside a checkout, or with `CACO_DOCTOR_SKIP_SCCACHE=1` set, the sensor is silent — no behaviour change for operator binaries that don't have the source tree.

## Operator-takeaway

The bd-f49a71 compile cache is already live and saving real work
(73.79% Rust cache hits in production), but its wiring is fragile —
it lives in three independent string-export lines inside `flake.nix`'s
shellHook, with no compile-time guarantee that a future refactor
keeps them. This sensor closes that loop: any commit that drops
`RUSTC_WRAPPER`, `CARGO_INCREMENTAL=0`, or `SCCACHE_DIR` from the
shellHook will turn `caco doctor` yellow with a hint pointing back to
bd-f49a71, surfacing the regression in seconds rather than waiting
for the next 24-worker compile storm to take helsinki down again.
The classifier is also unit-tested against the live `flake.nix`, so
the regression is caught even before the change reaches main.
