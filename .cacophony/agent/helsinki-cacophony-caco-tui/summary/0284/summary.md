# Session summary — Label graphics zero-hit upload bursts

## Goal

Respond to Harry’s overnight safe-improvement direction by taking the draft perf-log finding for repeated zero-hit/full-upload graphics bursts and making a low-risk observability improvement, not a speculative rendering change.

## Bead(s)

- `bd-58fd37` — Investigate zero-hit kitty graphics upload bursts.
- Related discarded experiment: `bd-54a8d6` — Build feed panel title without format intermediates, demoted to draft after actual Kitty regression.

## Before state

- Failing tests: none known for this slice.
- Relevant perf-log context: `bd-58fd37` was filed from idle perf-log review after `caco log perf-list --project cacophony --since 12h` showed repeated `tui.graphics`/kitty windows with zero render-cache hits and large upload batches. Follow-up metric queries showed examples such as `gfx_cache_hit_rate=0%` with 107 misses, `gfx_upload_count=107`, `gfx_upload_wire_bytes≈9.9MB`, and `gfx_upload_pass_avg≈8.3s` in the same time window. The draft’s broader 12h review reported several larger bursts, up to 1096 uploads, ~81.8MB wire traffic, and ~15.1s upload-pass average.
- Context: a feed-title allocation experiment (`bd-54a8d6`) was tried first because no ready caco-tui/performance bead existed, but actual Kitty after evidence regressed badly, so it was reverted and demoted to draft before this safe perf-log slice.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: no FPS improvement is claimed. `PerfFlush::labels()` now adds diagnostic labels when the current graphics flush window itself is suspicious: `cache:zero_hit` when misses are nonzero and hits are zero, `upload:burst` when uploads are at least 100, `upload_wire:large` when estimated wire bytes are at least 10 MiB, and `upload_pass:slow` when average upload-pass time is at least 1s. These labels make future perf-log filtering/correlation of the observed bursts practical without changing rendering behavior.
- Context: the change is intentionally observability-only and leaves graphics cache/upload semantics untouched.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/perf.rs`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: added two perf unit tests covering diagnostic label addition and omission below thresholds.
- Behavioural delta: perf events for graphics flushes can now carry burst-diagnostic labels; no UI/rendering behavior changes.
- Validation: `./scripts/rustfmt-changed.sh`; `cargo check -p caco-tui`; `cargo test -p caco-tui flush_labels_zero_hit_upload_bursts_bd_58fd37`; `cargo test -p caco-tui perf::tests`; `cargo clippy -p caco-tui --lib -- -D warnings`; `cargo test -p caco-tui`; `git diff --check`.

## Operator-takeaway

Instead of guessing at a risky graphics-cache fix, this slice makes the recurring zero-hit/full-upload bursts searchable in the existing perf-log stream. The next optimiser can filter on `cache:zero_hit`, `upload:burst`, `upload_wire:large`, or `upload_pass:slow` to identify which transition/view caused a burst before changing rendering logic.
