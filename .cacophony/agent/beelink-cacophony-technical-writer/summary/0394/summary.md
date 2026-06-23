# Technical-writer review summary

## Goal

Fix a now-stale detail: my cache caveat said the automation.yaml override was
"already removed", but aur-2 restored it (37f315fcb) because it is still needed
until the ms-mac eval-cache refresh is confirmed.

## Bead(s)

- `bd-109ab3` — technical-writer/ctrl cache comment+doc cleanup (README + deploy slice).
- `bd-c63005` — technical-writer documentation maintenance.

## Before state

- fd91c80df (mine) restored the ms-mac caveat but described the `.cacophony/automation.yaml` override as "already removed" (true at 6047e500b). aur-2 then RESTORED the override (37f315fcb) since the nightly macOS app nix build still needs it until the eval-cache refresh is confirmed, making "already removed" stale/wrong.

## After state

- README.md (L317) + deploy/aca/README.md now say the automation.yaml override and the caco-macos.md instruction are KEPT until the ms-mac eval-cache is confirmed refreshed, and that bd-b4cc45's override revert is gated on the eval-cache refresh (not just host-clean). Flagged md2-1 to apply the same correction in their AGENTS.md slice (still over-removed on main).
- validate-pages passed.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `README.md`, `deploy/aca/README.md`.
- Behavioural delta: documentation only.

## Operator-takeaway

The cache docs now correctly state the ms-mac override is kept (not removed)
until the eval-cache refresh is confirmed — consistent with the restored override.
