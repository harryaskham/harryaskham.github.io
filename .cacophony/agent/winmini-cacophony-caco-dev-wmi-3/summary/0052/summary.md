# bd-bbe764 — Stale-lock reaper active_git_check no longer false-positives on non-git processes

## Bead
bd-bbe764 (daemon-resilience/git-lock/reaper/false-positive, P2; filer caco-ctrl, discovered-via caco-ios-0). The stale-lock reaper's "no-active-git" predicate (bd-cd9125/bd-fecda2) reported `active git process cwd in checkout: pid 4550 (log)` and PRESERVED an orphaned index.lock — but pid 4550 was a 9h-orphaned `/usr/bin/log stream` altool monitor, NOT git. A non-git process whose cwd is in the checkout wrongly pinned a genuinely-stale lock as "active," defeating the reaper and wedging the checkout (blocking rebase/reaping).

## Root cause
On macOS, `lsof -c git -d cwd` does NOT restrict to git processes — lsof selection options are OR'd, so `-c git -d cwd` returns EVERY process's cwd (the `-d cwd`) in addition to git's files. So non-git cwd records (e.g. the `log` monitor with cwd in the checkout) reached `classify_stale_git_lock_holder`. That classifier matched a hardcoded needle list and returned `Unknown` for any unrecognized command — and both call sites treated `ActiveGitOwner | Unknown => active`, so an unrecognized non-git process (`log`) was classified active → lock preserved. The needle list was also incomplete for git plumbing (git-pack-objects, git-commit-tree, …).

## Fix (crates/caco-daemon/src/git_lock_cleanup.rs)
- Rewrote `classify_stale_git_lock_holder` to match the GIT EXECUTABLE specifically — `basename == "git" || basename.starts_with("git-")` (mirroring the Linux /proc `is_git_command` check). Confirmed git → `ActiveGitOwner`; ANY non-git command (log, cargo, editors, unknown monitors) → `BenignIdleHolder`. The `git-` prefix covers ALL git plumbing (more complete + safer than the old explicit needle list).
- Removed the now-redundant `STALE_GIT_LOCK_ACTIVE_GIT_NEEDLES` + `STALE_GIT_LOCK_BENIGN_HOLDER_NEEDLES` consts (no external refs).
- Both call sites (lsof-cwd `classify_lsof_cwd_records_for_checkout` + ps `classify_ps_active_git_lines`) now route `BenignIdleHolder | Unknown => ignored`, `ActiveGitOwner => active` — only a confirmed git executable pins the lock; non-git/unknown holders are ignored (the reaper still additionally gates reapability on the lock-holder lsof check + age, so this only removes the false-positive, not the real guards).

So a non-git process with cwd in the checkout no longer pins a stale lock; a genuinely-active git process (any `git`/`git-*`, incl. plumbing) still does.

## Validation (daemon test queue)
- `cargo test -p caco-daemon --lib classify_stale_git_lock_holder` (tj-c71a72cc): PASSED — updated `classify_stale_git_lock_holder_separates_git_from_idle_bd_6cf648`: git/git-rebase/`/usr/bin/git` → Active; tmux/node/cargo/rustc/lsof/grep/coreutils → Benign; the bd-bbe764 regressions `log` + `/usr/bin/log` → Benign (was the false-positive); git plumbing not in the old needle list (`git-pack-objects`, `/usr/lib/git-core/git-commit-tree`) → Active via the prefix.
- `cargo clippy -p caco-daemon --lib` (tj-e8e96263): git_lock_cleanup.rs clippy-clean; the 1 remaining warning is the pre-existing unrelated agent/lifecycle.rs:10401, not gate-blocking.
- rustfmt-clean on changed regions; `git diff --check` clean.

## Scope
Sibling to bd-a3ea00 (msm-3's active lock-hardening lane: fetch timeout+kill) but explicitly NOT part of it — this is the reaper active-git predicate only.

## Diff
See the reintegration receipt for the landed squash SHA.
