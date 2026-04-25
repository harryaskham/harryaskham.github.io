# Session summary — checkout_bootstrap git-hook install is now shell-robust

## Goal

Fix the intermittent ms-mac persistent-worker bootstrap failure where
`checkout_bootstrap` repeatedly died at `./scripts/install-git-hooks.sh` with
exit 127. The goal was to make git-hook installation succeed in minimal
bootstrap shells and to turn PATH/interpreter failures into explicit
operator-readable diagnostics instead of opaque retry-loop noise.

## Bead(s)

- `bd-0d9e23` — ms-mac persistent worker checkout_bootstrap fails installing git hooks

## Before state

- The repo advertised `./scripts/install-git-hooks.sh` as the canonical
  checkout-bootstrap step for managed checkouts.
- That installer used `#!/usr/bin/env bash` and immediately invoked `git`.
- The tracked `.githooks/pre-commit` hook also used `#!/usr/bin/env bash` and
  bash-specific arrays/process substitution.
- On ms-mac persistent-worker bootstrap, repeated failures were observed at:
  - `checkout_bootstrap[0] ./scripts/install-git-hooks.sh`
  - exit 127
- That strongly suggested a brittle shell/interpreter/PATH assumption during
  early bootstrap.

## After state

- `scripts/install-git-hooks.sh` now uses POSIX `sh` and performs explicit
  `git` availability checks before trying to resolve the repo root.
- Missing-`git` failures now print a clear diagnostic including the current
  `PATH` and exit 127 intentionally.
- The installer now self-heals a missing execute bit on `.githooks/pre-commit`
  via `chmod +x`, and only fails if that repair itself fails.
- `.githooks/pre-commit` was rewritten from bash-specific logic to POSIX `sh`
  so the installed hook no longer depends on bash arrays/process substitution.
- The hook still preserves the intended behavior:
  - find staged Rust files
  - require `rustfmt`
  - format them
  - re-add them to the index

## Diff summary

- Files touched:
  - `scripts/install-git-hooks.sh`
  - `.githooks/pre-commit`
- Behavioural delta:
  - checkout bootstrap no longer depends on `env bash` resolution just to
    install hooks
  - missing PATH/tooling failures are now explicit and actionable
  - tracked hook installation is more tolerant of exec-bit drift
- Validation:
  - `sh ./scripts/install-git-hooks.sh`
  - `PATH=/nonexistent /bin/sh ./scripts/install-git-hooks.sh` → clear
    `git not found on PATH` diagnostic
  - temp repo smoke test:
    - install hook file
    - stage a Rust file
    - run `sh ./.githooks/pre-commit`
    - confirm hook succeeds and `rustfmt` path works

## Operator-takeaway

This failure was caused by shell brittleness in the earliest bootstrap path,
not by the actual git-hook logic. The hook installer and hook itself now avoid
bash-only assumptions and emit clearer PATH/tooling diagnostics, which should
stop ms-mac persistent workers from retry-looping on hook installation.
