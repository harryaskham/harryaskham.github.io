# bd-cd87fc: keep TUI benchmark paced truth-check visible without jq

## What changed

- Moved the paced/headroom benchmark truth-check into `print_benchmark_truth_check()`, a small Python-backed path that runs before the optional `jq` pretty-printer.
- Removed the duplicate `jq`-only truth-check branch so output has one warning line whether or not `jq` is installed.
- Updated SPEC/README/AGENTS/docs TUI guidance to state that the paced Overall FPS warning is jq-independent.

## Why

`jq` is optional in `scripts/tui-fps-bench.sh`. The previous human warning existed only in the `jq` summary output; minimal hosts without `jq` fell back to raw JSON and could again make target-capped Overall FPS look like graphics/text parity. The truth-check now remains visible on those hosts.

## Validation

- `bash -n scripts/tui-fps-bench.sh`
- `scripts/tui-fps-bench.sh --help`
- extracted `print_benchmark_truth_check()` and ran it against synthetic paced JSON, verifying it printed `Truth check: paced Overall FPS...`
- `docs/validate-pages.sh` (output saved to `/tmp/docs-validate-pages-bd-cd87fc.log`)
- `git diff --check`
