# bd-891da4 — TUI FPS benchmark stale-binary provenance

Implemented benchmark provenance and an opt-in hard guard for `scripts/tui-fps-bench.sh` so `--no-build` runs cannot silently look like current-checkout evidence when they are actually using an older installed `caco` binary.

## Changes

- `scripts/tui-fps-bench.sh`
  - Adds `--require-current-binary` and `CACO_FPS_REQUIRE_CURRENT_BINARY=1`.
  - Reads the benchmarked binary's `caco version --json` commit/version.
  - Reads checkout `git rev-parse HEAD`, short HEAD, and dirty state.
  - Warns when the binary commit is missing, mismatched with source HEAD, or the checkout is dirty.
  - Fails before running the benchmark when the hard guard is enabled and binary/source provenance is stale or unverifiable.
  - Annotates output JSON `harness` metadata with binary version/commit, source HEAD, dirty state, binary/source drift flag/reason, and the hard-guard setting.
  - Prints a concise `Binary:` line in the human summary.
- Documentation and contract updates:
  - `SPEC.md` 20.7.5.2 records the provenance/guard contract for the first-party wrapper.
  - `README.md`, `AGENTS.md`, and `docs/benchmarks/bd-9daede-real-terminal-fps.md` document the drift warning and hard-guard option.

## Validation

- `bash -n scripts/tui-fps-bench.sh`
- `git diff --check -- scripts/tui-fps-bench.sh AGENTS.md README.md SPEC.md docs/benchmarks/bd-9daede-real-terminal-fps.md`
- Fake harness test with shim `caco` and `tmux`:
  - stale fake binary (`commit=deadbeef`) produced a warning;
  - JSON contained `harness.caco_version == "9.9.9"`, `harness.caco_commit == "deadbeef"`, source HEAD, dirty state, drift flag, and drift reason;
  - `--require-current-binary` failed with the expected error before running the benchmark.

## Notes

The current working checkout is dirty by definition while this patch is staged, so validation intentionally proves that dirty source state is recorded as drift during fake `--no-build` evidence runs.
