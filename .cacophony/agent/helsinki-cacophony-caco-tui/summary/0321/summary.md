# Session summary — classify short graphics warmup misses separately

## Goal

Handle the ready TUI graphics perf follow-up `bd-cdb4dd`, which reported `cache:zero_hit` upload bursts persisting after `bd-58dcaa`. The goal was to determine whether the cited post-fix samples were sustained cache invalidation churn or expected short warmup/catch-up telemetry, and to tighten labels so future investigations are pointed at true steady churn.

## Bead(s)

- `bd-cdb4dd` — TUI graphics upload bursts persist after bd-58dcaa closure

## Before state

- Failing tests: none known at the start.
- Relevant metrics: bead evidence cited an event with 44 rendered frames, only 4 graphics upload passes, 350 uploads, ~19.7 MiB estimated Kitty wire traffic, and 0 cache hits. Under the `bd-58dcaa` rule, only single-pass cold misses were split from `cache:zero_hit`; short multi-pass warmup windows still looked like sustained idle churn.
- Context: recent local perf-list samples on Helsinki did not include current `tui.graphics` records, so this slice is a telemetry classification fix based on the recorded bead evidence rather than a fresh FPS/upload benchmark claim.

## After state

- Failing tests: none observed after the fixture fix.
- Relevant metrics: `PerfFlush::labels()` now classifies zero-hit windows with up to four graphics upload passes as `cache:cold_warmup`; `cache:zero_hit` is reserved for larger samples. Single-pass windows still use `cache:cold_single_pass`, and burst/wire labels are preserved.
- Context: this determines the cited post-`bd-58dcaa` four-pass sample as short warmup/catch-up telemetry rather than proof of sustained invalidation. It does not reduce upload bytes by itself; it makes future perf-list evidence separate expected warmup bursts from real idle churn.

## Diff summary

- Commits: final branch/reintegration commit to be assigned by `caco agent reintegrate`.
- Files touched: `crates/caco-tui/src/perf.rs`, `SPEC.md`, `docs/tui.html`.
- Tests: added `flush_labels_cold_warmup_instead_of_zero_hit_bd_cdb4dd`; adjusted the sustained zero-hit fixture to remain above the new threshold.
- Behavioural delta: `tui.graphics` perf labels now distinguish `cache:cold_single_pass`, `cache:cold_warmup`, and sustained `cache:zero_hit`; no TUI rendering or upload scheduling semantics changed.
- Validation: `./scripts/rustfmt-changed.sh`; `docs/validate-pages.sh`; `git diff --check`; queued focused cold-warmup test (`tj-8de41a26`, `tj-751eed9d`); queued sustained zero-hit test after fixture fix (`tj-e146f1cf`); queued `cargo check -p caco-tui` (`tj-2c2ef839`); queued `cargo clippy -p caco-tui --lib -- -D warnings` (`tj-48d75543`); queued full `cargo test -p caco-tui` (`tj-5182ae92`). A prior full test run (`tj-a81e2203`) failed because the existing sustained zero-hit fixture was moved to five graphics passes but kept an upload-pass time below the slow-label threshold; the fixture was corrected and rerun successfully.

## Operator-takeaway

The reported “persistent” upload bursts were short zero-hit warmup windows by the available evidence. Future perf-list sweeps will label those as `cache:cold_warmup` instead of `cache:zero_hit`, leaving `cache:zero_hit` for sustained multi-pass churn that actually needs cache invalidation investigation.
