# Session summary — android-cli install guidance

## Goal

Capture Harry's android-cli install command in the Android QA documentation so remote Android hosts can be prepared consistently before agents use `qa-screenshot.sh --remote-android`.

## Bead(s)

- `bd-local-android-cli-docs` — operator-requested Android QA documentation follow-up while the local daemon was unreachable

## Before state

- Remote Android QA mode existed and preferred `android-cli`, but the docs did not show how to install the `android` binary on a Darwin arm64 remote host.
- Harry provided a command with a path mismatch between `$HOME/local/bin/android` and `$HOME/.local/bin/android`.
- The local Cacophony daemon and beads primary were temporarily unreachable, so a formal bead could not be created before applying this small docs-only fix.

## After state

- `companion/android/QA.md` now includes an `Install android-cli` section.
- The documented command creates `$HOME/.local/bin`, downloads the Darwin arm64 binary to `$HOME/.local/bin/android`, and chmods the same file.
- The docs note that non-interactive SSH sessions need `$HOME/.local/bin` on `PATH`, or agents can pass `--remote-android-bin "$HOME/.local/bin/android"`.

## Diff summary

- Commits: current docs and summary commits
- Files touched:
  - `companion/android/QA.md`
- Tests:
  - `git diff --check` — passed
- Behavioural delta: documentation only; no app or helper runtime behavior changed.

## Embedded artefacts

- `screenshots/tendril-display-android-cli-docs.png` — retained low-resolution Tendril capture from the remote Android helper workflow context.

## Operator-takeaway

Remote Android hosts now have a copy-pasteable android-cli install path that avoids the `local/bin` versus `.local/bin` mismatch and works with the existing remote helper options.
