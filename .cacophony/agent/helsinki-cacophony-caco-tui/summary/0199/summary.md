# Session summary — graphics-gated optimiser evidence guard

## Goal

Continue the active caco-tui optimiser loop after Harry corrected the benchmark evidence path, and prevent future optimiser cycles from claiming graphics FPS wins from text-mode tmux data.

## Bead(s)

- `bd-56c069` — Require graphics-gated evidence in TUI animation optimiser profile.
- Discarded experiment: `bd-a1717a` — Avoid z-clause string allocation in kitty placement commands.

## Before state

- Failing tests: none for this profile-only slice. The previous broken-on-main kitty failure `bd-fb4bea` was already landed and closed.
- Relevant metrics: text-mode fixture runs had been used earlier in the session and reported high work FPS with `graphics_capability=None`; Harry pointed out those are not actual graphics evidence. Actual Xvfb/kitty baselines on main around this slice reported `graphics_capability=Kitty`, uploads/deletes observed, upload wire bytes around 26.6MB, roughly 249–315 app-side work FPS, and roughly 137–156 terminal-inclusive FPS.
- Context: a tiny `z_clause` allocation-removal experiment (`bd-a1717a`) passed byte-equivalence tests but did not produce a reliable actual-graphics win, so it was reverted and moved back to draft with evidence.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: no performance win is claimed for this slice. The optimiser profile now explicitly requires graphics-gated, terminal-inclusive evidence for graphics/FPS claims and treats text-mode tmux metrics only as CPU/layout headroom.
- Context: future profile users should run `scripts/tui-fps-bench.sh --release --graphics --uncapped --terminal-sync --debug ...` or an equivalent real-TUI benchmark that reports `Kitty`/`Ghostty` capability, observed graphics work, terminal-sync metrics, and upload/delete counts before claiming graphics FPS improvement.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: no code tests added or removed.
- Behavioural delta: no runtime behaviour change. This is a self-improvement/profile guard so the persistent optimiser uses the right benchmark lane.
- Validation: `git diff --check`; manual inspection of the profile diff; actual graphics benchmark evidence captured in `/tmp/caco-fps-cycle10-graphics-baseline.json`, `/tmp/caco-fps-after-bd-a1717a-graphics.json`, and `/tmp/caco-fps-after-bd-a1717a-graphics-rerun.json` before discarding the experiment.

## Operator-takeaway

The key correction is now baked into the optimiser profile: text-mode tmux numbers are no longer acceptable evidence for graphics FPS wins. Future cycles must use actual Kitty/Ghostty graphics with terminal-sync metrics before landing graphics-performance claims.
