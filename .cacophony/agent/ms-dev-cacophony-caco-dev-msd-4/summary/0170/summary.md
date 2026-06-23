# bd-14838a — caco doctor asserts SCCACHE_CACHE_SIZE in the sccache-wiring sensor

## Goal
Follow-up to bd-154692 (sccache 30G ceiling): catch a future flake cleanup that silently drops
SCCACHE_CACHE_SIZE (→ sccache's 10G default → the cold-gate eviction bd-154692 fixed). ms-dev-2-ctrl-endorsed.

## Change
caco-cli classify_sccache_wiring (bd-f49a71 doctor sensor): added a SCCACHE_CACHE_SIZE check as a 3-way,
NOT bundled with critical wiring (it's TUNING — without it sccache still caches at 10G, not broken):
- critical wiring (RUSTC_WRAPPER/CARGO_INCREMENTAL=0/SCCACHE_DIR/sccache) missing → existing cold-compile warning;
- else SCCACHE_CACHE_SIZE missing → distinct accurate warning (10G default evicted by concurrent worker+gate compiles, see bd-154692);
- else ok (message now lists SCCACHE_CACHE_SIZE).
+unit test classify_sccache_wiring_warns_when_sccache_cache_size_missing; the live-flake regression test
now also asserts the size export is present (passes against the bd-154692 flake.nix).

## Validation
caco-cli is gate-EXCLUDED from test-small → self-validated:
- cargo test -p caco-cli classify_sccache_wiring: 6/6 pass (tj-cf6e0c1c), incl. the new test + live-flake.
- cargo clippy -p caco-cli --all-targets: my change is clean (the one clippy error is a SEPARATE pre-existing
  dead-code warning `push_agent_branch_for_pr is never used`, PR-mode code, NOT my classify_sccache_wiring change — flagged to ctrl).

## Diff
- Code commit: 701bdb0280 (defer landed squash SHA to the reint receipt).
- crates/caco-cli/src/lib.rs: classify_sccache_wiring 3-way + new unit test + live-flake assertion.
