# Slice 9 — bd-4ee05d: optimize oversized Pages image assets

## Goal

Reduce docs/images PNG weight from ~8MB to ~1.7MB by resizing oversized screenshots to web-appropriate dimensions.

## Bead(s)

- **bd-4ee05d** (task, P3) — [docs] Optimize oversized Pages image assets.

## Before state

- `docs/images/tui-hero.png`: 6016×3384 RGBA, 6.5MB (README hero only, not in any HTML page).
- `docs/images/tui.png`: 2992×1680 RGBA, 1.3MB.
- Total: ~7.8MB of static image weight in the Pages artifact.

## After state

- `docs/images/tui-hero.png`: 1504×846 RGB, 995KB (85% reduction).
- `docs/images/tui.png`: 1496×840 RGB, 679KB (47% reduction).
- Total: ~1.7MB. Net savings: ~6.1MB.
- Resized with Pillow LANCZOS, converted RGBA→RGB (no transparency needed for screenshots), PNG optimize=True.
- Visual quality preserved at web display resolution.

## Diff summary

```
 docs/images/tui-hero.png | Bin 6726101 -> 1018877 bytes
 docs/images/tui.png      | Bin 1289807 ->  695166 bytes
 2 files changed, 0 insertions(+), 0 deletions(-)
```

## Operator-takeaway

Pages artifact is ~6MB lighter. README hero image still renders correctly at typical display widths.
