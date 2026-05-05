# bd-46ac44: do not count retained Kitty redisplays as full TUI uploads

## What changed

- Live upload telemetry now separates retained display-only fast paths from full bitmap uploads:
  - retained redisplays increment `upload_dedupe_hits`,
  - full `upload_success_count` / `upload_bytes` only count actual PNG/native bitmap uploads.
- Real-dashboard benchmark upload telemetry now follows the same split.
- Retained redisplays still count as graphics placement work / graphics frames, because they send Kitty `a=p` display commands.
- Updated SPEC/README/docs wording so benchmark consumers know retained redisplays are not full-upload payload bytes.
- Added regression coverage for a retained-only benchmark upload pass.

## Why

A retained Kitty redisplay is the optimized path: it sends a small display command for a terminal-retained image and avoids retransmitting PNG payload bytes. Counting those as full uploads made optimized runs look upload-heavy and polluted cache-miss/upload-byte metrics, making graphics benchmarks harder to interpret. This keeps upload counts truthful while preserving retained redisplay evidence.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs crates/caco-tui/src/app/benchmark_support.rs`
- `docs/validate-pages.sh` (output saved to `/tmp/docs-validate-pages-bd-46ac44.log`)
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_46ac44"` — `tj-69130f77`, passed
