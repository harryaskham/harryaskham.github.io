# bd-c1c272 TUI Ghostty/Tendril audit slice

## Goal

Exercise the real Cacophony TUI through a dedicated Ghostty window and the Tendril computer-control path, then land the smallest durable improvement from the audit.

## Bead(s)

- `bd-c1c272` — permanent TUI Ghostty/Tendril improvement tracker.
- Filed `bd-d522bf` — Wayland portal capture timeout after successful Ghostty window discovery.

## Before state

`caco tendril` correctly avoided pretending to be a full computer-control wrapper, but its help/docs did not warn operators that target discovery and advertised capture capability can still precede a platform capture timeout. The TUI audit profile also lacked an explicit instruction to preserve the failing Tendril envelope and file a focused bead when capture fails before screenshots exist.

## After state

This slice records the live audit evidence and updates the operator guidance so future agents use the capture-act-verify loop, treat capture failures as actionable evidence, and avoid assuming `capture=true` guarantees a screenshot.

## Evidence

- `tui-benchmark.json` — real-dashboard benchmark completed under a TTY harness: ~30.30 FPS in text-mode fixture, `graphics_capability: None`, GPU fell back to CPU because no wgpu adapter was visible.
- `tendril-list-after-ghostty.json` — sanitized Tendril list result containing only the dedicated Ghostty TUI window (`caco-tui-audit-*`).
- `tendril-capture-window.log` — Tendril capture failed with `platform_adapter_timeout` / `xdg-desktop-portal screenshot did not respond within 10000 ms`.

No screenshot artefact is included because the audited failure occurs before Tendril can write the requested screenshot file.

## Operator takeaway

The Ghostty TUI launch/discovery path works on this host, but Wayland portal screenshot capture currently blocks the full visual loop. Follow-up implementation is tracked by `bd-d522bf`; this commit narrows the operator guidance and docs so the next loop does not mistake discovery success for capture success.
