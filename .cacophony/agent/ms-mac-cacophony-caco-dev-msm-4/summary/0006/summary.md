# Session summary — TUI Tendril audit metric slice

## Goal

Continue the Ghostty/Tendril TUI audit under the persistent TUI driver loop, using the real `caco tui benchmark` target and saving captures into this recorded summary so Harry can watch them via `/tmp/watch-captures.sh`.

## Bead(s)

- `bd-cace41` — TUI Ghostty border drawing artefacts visible in manual preview
- parent: `bd-c1c272` — [PERMANENT] TUI ghostty/tendril improvement and computer-control audit
- follow-up filed: `bd-8dfaa5` — Rename TUI diagnostic/testbed commands so agents pick the intended audit target

## Before state

- Real target clarification: operator corrected that `caco tui benchmark` is the real dashboard benchmark target; `graphics-testbed` is the isolated border testbed and was the wrong target for this audit slice.
- Baseline real benchmark from the dedicated Ghostty window produced about 47 FPS at a 60 FPS target, 1,787 successful graphics uploads, 0 upload failures, about 1.91 uploads/frame, and about 30.6 MB of kitty wire bytes.
- Benchmark JSON exposed upload and renderer-cache counters, but did not expose retained kitty image redisplays, making it hard to distinguish full PNG uploads from cheap retained image placement redisplays during Ghostty flicker audits.
- Tendril captures were already being stored under `summary/0006/screenshots/`, and the temporary watcher was updated to show each image path once across sibling ms-mac agent checkouts.

## After state

- `caco_tui::RealTuiBenchmarkResult` now includes `retained_redisplays` and `retained_redisplays_per_frame`, sourced from the existing `GraphicsCounters::upload_dedupe_hit_count` that the benchmark upload path already records when it reuses retained kitty images.
- Added focused unit coverage proving the real benchmark result reports retained redisplays and per-frame rate.
- Latest Tendril checkout was used through `nix run /Users/harryaskham/.cacophony/daemon/checkouts/tendril` for capture/control probes; captures are saved in this summary's screenshots directory.
- The latest Tendril run path typed into the correct Ghostty terminal but did not submit the command with the `return` key tap during a focus-contention moment; this was reported to `tndl-ctrl` with screenshot evidence.

## Diff summary

- Files touched: `crates/caco-tui/src/app/benchmark_support.rs`, `.cacophony/agent/ms-mac-cacophony-caco-dev-msm-4/summary/0006/*`.
- Tests added: 1 unit test, `real_benchmark_result_reports_retained_redisplays_bd_cace41`.
- Validation: `cargo test -p caco-tui real_benchmark_result_reports_retained_redisplays_bd_cace41 --lib -- --test-threads=1`; `cargo test -p caco-tui benchmark_support --lib -- --test-threads=1`; `cargo fmt --all -- --check`; `git diff --check`.
- Behavioural delta: the real TUI benchmark JSON now contains retained-image reuse metrics needed to audit kitty graphics efficiency and Ghostty flicker without guessing from upload counts alone.

## Embedded artefacts

- `screenshots/audit-current-after-resume.png` — capture of the dedicated Ghostty audit window after resuming the session.
- `screenshots/before-retained-metric-benchmark.png` and `screenshots/after-retained-metric-benchmark.png` — capture pair around the attempted real benchmark rerun through Tendril.
- `screenshots/latest-tendril-capture.png` — capture made through latest Tendril via `nix run`.
- `screenshots/latest-tendril-return-probe.png` — evidence for the latest Tendril return-key probe.
- `data.json` — compact benchmark/audit metrics and Tendril notes for this slice.

- `screenshots/capture-only-58582-current.png` — capture-only Tendril screenshot of dedicated Ghostty window 58582, avoiding agent-side command spawning.
- `screenshots/capture-only-58582-top-zoom.png`, `screenshots/capture-only-58582-left-join-zoom.png`, and `screenshots/capture-only-58582-center-join-zoom.png` — bounded zoom crops for border/join inspection from the live 58582 window.

- `screenshots/load-aware-capture-58582.png` and `screenshots/load-aware-capture-58582-bottom-zoom.png` — capture-only check under ms-mac load, confirming the audit window state without spawning new commands.

## Operator-takeaway

This slice did not tune the borders yet; it made the real TUI benchmark more useful for the ongoing audit by exposing retained kitty image reuse, while preserving screenshot evidence in the summary stream. The next visual fix should use these metrics to separate true bitmap reuploads from retained redisplays when chasing Ghostty flicker and noisy border artefacts.
