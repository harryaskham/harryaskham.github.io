# Session summary — gitignore generated images dirtying the canonical checkout (bd-18eced)

## Goal

Stop the background image generation from leaving untracked files in
`.cacophony/images/` that dirty the daemon canonical checkout and block ALL
cacophony reintegrations (a P1 that forced caco-ctrl to manually re-clear the
canonical every few minutes). Do it without breaking the documented
`caco image generate --commit` workflow or ignoring intentionally-committed
images.

## Bead(s)

- `bd-18eced` — Generated images in .cacophony/images/ dirty the daemon
  canonical checkout, blocking all cacophony reintegrations (P1 bug)

## Before state

- Failing tests: none (repo-config/behavior bug).
- `.cacophony/images` is `generation.yaml`'s `output_dir` for many avatar/mood
  presets; the daemon's background generation regenerates PNGs there on a
  ~3-6min cadence. `.cacophony/images/` was NOT gitignored (`git check-ignore`
  empty) and had ZERO tracked files (`git ls-files` = 0), so each generation
  left untracked files that dirtied the canonical checkout and blocked direct
  reintegration fleet-wide. Curated/committed avatars actually live under
  `static/` (e.g. `static/claude1.png`), not `.cacophony/images/`.

## After state

- Failing tests: none. `git check-ignore` now matches
  `.cacophony/images/<file>.png` (ignored), while `static/claude1.png` remains
  NOT ignored (committed avatars preserved).
- The background-generation dirt no longer appears in the canonical checkout;
  reintegrations are no longer blocked by it.

## Diff summary

- Code/content commit: pending final squash SHA from the reintegration receipt.
- Files touched: `.gitignore` (+9), `crates/caco-cli/src/lib.rs` (+5/-1).
- Changes:
  - `.gitignore`: ignore `.cacophony/images/` (generation output_dir) with a
    comment explaining it is regenerable generated output, not source, and that
    curated committed avatars live under `static/`.
  - `caco image generate --commit`: changed the auto-commit `git add` to
    `git add -f` so an explicit `--commit` of a generated image still works past
    the new ignore (the documented images-mixin workflow is preserved).
- Tests: +0 / -0 (validated via `git check-ignore` + the reint cargo gate; the
  `-f` change is a trivial compile-checked arg addition).
- Behavioural delta: background-generated images no longer dirty the checkout;
  `caco image generate --commit` still commits via force-add; committed
  `static/` images unaffected.

## Operator-takeaway

`.cacophony/images/` is generation scratch (regenerated avatars), not source —
the curated committed avatars are under `static/`. It was never gitignored, so
the background generation cadence quietly dirtied the canonical checkout and
blocked every reintegration until manually cleared. The careful fix (per
project-health's caution) is a dir-scoped ignore of the generation output_dir
plus a `git add -f` in the `--commit` path so the documented explicit-commit
workflow is preserved — not a blanket image ignore. If a future generated image
must be tracked, `caco image generate --commit` (force-add) or a manual
`git add -f` still works; routine background generation stays ignored.
