# Session summary — bd-215e3f: doctor distinguishes modified vs untracked checkouts

## Goal

Stop `caco doctor` shouting "uncommitted changes" at any
non-empty `git status --porcelain`. Distinguish modified
tracked files (real concern) from untracked-only
(usually a leftover artifact, surface as info).

## Bead(s)

- `bd-215e3f` — own bead filed after observing the
  noisy hint on collective checkout. Closed.

## Before state

- `caco doctor` rendered:
  ```
  ! warning  checkout 'collective'  branch=main head=... dirty=true
  recovery hints:
    → Project 'collective' checkout has uncommitted changes.
  ```
- Reality: just one untracked directory
  (`collective-public/flakes/`); zero modified tracked
  files. The "uncommitted changes" phrasing implied
  tracked-file mutation.

## After state

- New `git_dirty_kind(dir) -> (has_modified, has_untracked)`
  helper replaces `is_git_dirty`.
- Status mapping:
  - `has_modified` → **warning** (genuinely concerning)
  - untracked-only → **info** (leftover artifact, no
    integrity issue)
  - clean → **ok**
- Hint text matches reality:
  - "Project X checkout has modified tracked files."
  - "Project X checkout has untracked files."
  - "Project X checkout has modified tracked files and
    untracked files."
- Verified live on collective: now reads
  `info  checkout 'collective'  ... dirty=true` with hint
  `→ Project 'collective' checkout has untracked files.`

## Diff summary

- 1 file touched, +35 / −15:
  - `crates/caco-cli/src/lib.rs`: replace `is_git_dirty`
    with `git_dirty_kind`; update section 9 to use the
    new helper and emit specific hint text.

## Verification

- `cargo build --bin caco`: clean (one prior unused-fn
  warning fixed by removing the old helper).
- Live `./target/debug/caco doctor` shows the new
  info-tier render.

## Operator-takeaway

Family with bd-126b99/bd-a403a1/bd-30fbfb/bd-2886bb/bd-dfc91a
(CLI honesty pass continues) — diagnostic output should
report the actual condition observed, not a worst-case
synonym. Untracked-only no longer raises a warning that
operators have to mentally downgrade.
