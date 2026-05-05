# bd-bf9fc5: warn when paced TUI benchmark FPS can hide graphics work cost

## What changed

- `scripts/tui-fps-bench.sh` now prints an explicit `Truth check:` line when a benchmark result is paced, warning that Overall FPS is target-capped and must not be used as graphics/text parity evidence.
- The same output also warns when work/headroom FPS is materially below achieved FPS in non-paced output.
- Updated `SPEC.md`, `README.md`, `AGENTS.md`, and `docs/tui.html` to make the benchmark-output interpretation contract explicit.

## Why

Operators were seeing Kitty mode far slower than text mode while single-run benchmark output could still show target-FPS parity. The benchmark JSON already carried work/headroom metrics, but the human output emphasized capped Overall FPS first. This makes the CLI output itself harder to misread and points operators to Work FPS, `--uncapped`, and `scripts/tui-fps-compare.sh` for parity claims.

## Validation

- `bash -n scripts/tui-fps-bench.sh`
- `scripts/tui-fps-bench.sh --help`
- synthetic `jq` parse/render check for the new truth-check branch
- `docs/validate-pages.sh`
- `git diff --check`
