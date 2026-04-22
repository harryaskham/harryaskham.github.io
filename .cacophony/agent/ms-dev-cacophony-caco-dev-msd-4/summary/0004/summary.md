# Session summary — bd-89615a release.yml python3 fallback

## Goal

Unbreak the release workflow on the helsinki self-hosted Linux
runners, which were failing at the upload step with
`python3: command not found` since v1.2.488.

## Bead(s)

- `bd-89615a` — `[broken-on-main]` release.yml v1.2.488/489 fails
  on helsinki self-hosted Linux runner: python3 not on PATH at
  Upload step.

## Before state

- The upload step in `.github/workflows/release.yml` invoked bare
  `python3 - "$REPO" "$TAG" ... <<'PYEOF' ...`.
- The bd-227f86 comment block asserted "Python3 is always present
  on the nix runner" — false on the new helsinki Linux runners.
- Both `caco-linux-x86_64` and `caco-linux-aarch64` jobs failed
  with exit 127 at the upload heredoc; macOS unaffected.

## After state

- Upload step now resolves the python3 interpreter dynamically:
  1. If `python3` is on PATH (macOS, any host with python3
     installed), use it directly — zero perf change.
  2. Else if `nix` is on PATH, fall back to
     `nix shell --quiet nixpkgs#python3 --command python3`. The
     build step above already proves `nix develop` is available
     on every release runner, so this is a guaranteed fallback
     that does not depend on operator action to install python3
     on the runner host.
  3. Else fail loud with `FATAL: neither python3 nor nix on PATH`.
- Comment block updated to record the bd-89615a discovery and the
  new resolution strategy.
- Same urllib uploader body retained verbatim — no API change.

## Diff summary

- Commits: `a2ac5a3c`
- Files touched: `.github/workflows/release.yml` (+25 / -4)
- Tests: workflow-only change, no Rust unit tests.
- Behavioural delta: helsinki Linux release jobs now use
  `nix shell nixpkgs#python3` when python3 is missing; macOS jobs
  unchanged.

## Validation

- `python3 -c 'import yaml; yaml.safe_load(open(".github/workflows/release.yml"))'`
  parses the workflow cleanly.
- Local bash sanity-check of the array-dispatch + heredoc
  forwarding pattern under `set -euo pipefail` succeeds.
- Will be observably validated on the next `v*` tag push.

## Operator-takeaway

If a future release runner ships without `python3` on PATH, the
workflow will silently fall through to `nix shell nixpkgs#python3`
and continue. If both python3 and nix are missing, the upload
step now fails fast with `FATAL: neither python3 nor nix on PATH;
cannot upload release asset` — clear signal to fix the runner.
