# Technical-writer review summary

## Goal

Resume the multi-surface documentation catch-up Harry called out (Picophony, the
Apple/Android/watch apps, and ~2 weeks of mainline drift), landing what is ready
while the git-LFS / merge-queue reintegration incident is worked.

## Bead(s)

- `bd-c63005` — technical-writer documentation maintenance.
- Documents Picophony epic `bd-93f302`.

## Before state

- `docs/daily-changelog.md` covered through `3307ee4a5` (2026-06-01, only 1 commit on that day).
- No public docs page for Picophony (`crates/pico`, `crates/caco-picophony`), the cross-platform Pi-driving layer behind the pico TUI, caco-tui/daemon, Apple, Android, and web.
- Validation baseline: `./docs/validate-pages.sh` green.

## After state

- `docs/daily-changelog.md` now covers through `5b4187b1a` (2026-06-01 full 64-commit day plus 2026-06-02); the 64-commit 2026-06-01 day.
- New `docs/picophony.html` + `docs/picophony.md` public page (architecture, `pico` binary, cargo features, cross-surface consumers, additive relationship to managed Pi), linked from the sidebar on all pages and staged in `.github/workflows/docs.yml`.
- `docs/tui.html` trimmed back under the 51200-byte budget after the new sidebar entry.
- Validation: `./docs/validate-pages.sh` → 4073 passed, 0 warnings, 0 failed.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `docs/daily-changelog.md`, `docs/picophony.html` (new), `docs/picophony.md` (new), `docs/tui.html`, every `docs/*.html` sidebar (Picophony nav link), `.github/workflows/docs.yml`.
- Behavioural delta: documentation only.

## Operator-takeaway

Picophony now has a first-party docs page sourced from the crate-level contracts (`caco-picophony` is additive over `pi --mode rpc`; Pi stays the sole session-file writer). Reintegration is held: beelink's daemon canonical checkout is drifted (stuck at 566610a05, `merge_base=unknown` vs target) and beelink's GLOBAL git LFS smudge is unset (normal), so an integration clone would materialize the 655M `static/bgs` — both flagged to ctrl with the fix (global skip-smudge + `caco checkout regenerate` on beelink). This work lands once beelink is repaired and the reintegration lock frees. Remaining backlog: 2026-06-02 onward changelog days, plus iOS/watchOS/Android app docs and the sparse-checkout→git-LFS change.
