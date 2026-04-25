# Session summary — bd-faa43b remove personal GitHub namespace from release profiles

## Goal

Strip the personal GitHub namespace
(`ssh://git@github.com/harryaskham/cacophony.git`) from the shipped
release-profile frontmatter so reusable profiles don't leak one
operator's account or couple to a single fork's repo topology.

## Bead(s)

- `bd-faa43b` — [docs] Remove personal GitHub remotes from release profiles (P3 task, technical-writer privacy review)

## Before state

- `.cacophony/profiles/changelog-manager.md`,
  `release-manager.md`, and `update-helper.md` each had a
  frontmatter block:
  ```yaml
  git_remotes:
    github: ssh://git@github.com/harryaskham/cacophony.git
  ```
  which the daemon's lifecycle handler (`agent/lifecycle.rs`)
  auto-applied as `git remote add github <url>` on agent spawn —
  silently pinning everyone's checkout at one operator's GitHub
  namespace.
- Prose under "One-Time Setup" / "One-time setup" claimed the
  profile auto-configures the github remote.

## After state

- `git_remotes` block removed from all three frontmatters,
  replaced with a `bd-faa43b`-tagged YAML comment that points
  operators at the project / checkout layer for remote
  configuration (e.g. project integration config, or a startup
  `git remote add github <your-fork-or-upstream-url>`).
- Prose updated to match: operator now configures the remote
  manually; canonical remote name `github` is preserved so
  reintegration / fetch / push commands in the rest of the profile
  continue to work without further edits once the remote is wired.

## Diff summary

- Commit: 8545e245b
- Files touched:
  - `.cacophony/profiles/changelog-manager.md` (-2 frontmatter
    lines, +8 comment lines)
  - `.cacophony/profiles/release-manager.md` (-2 frontmatter
    lines, +8 comment lines, prose rewritten)
  - `.cacophony/profiles/update-helper.md` (-2 frontmatter lines,
    +8 comment lines, prose rewritten)
  - `crates/caco-profile/src/lib.rs` (+55) — new
    `shipped_profiles_have_no_personal_github_namespace_bd_faa43b`
    walks every `.cacophony/profiles/*.md`, isolates the
    frontmatter block, and asserts neither `harryaskham` nor
    `git_remotes:` appear there.
- Tests: cargo test-small 264/264 pass.

## Operator-takeaway

Reusable profiles no longer ship with one fork's GitHub namespace
hard-coded. Operators wire the `github` remote at the project /
checkout layer. The new test pins the contract so a drive-by edit
can't quietly re-introduce a personal namespace into the shipped
frontmatter.
