# Session summary — AKS current-main rollout and beads sync fix

## Goal

Complete another AKS rollout pass after main moved: roll the production AKS deployment to the current image, prove private `@cluster` access still works, reconcile the isolated AKS board, and fix the remaining in-cluster beads sync failure.

## Bead(s)

- `bd-2d1ffe` — Restore AKS cluster health and ergonomic local access

## Before state

- Failing tests: live `caco @cluster:caco-aks bd sync` failed with `database error: row: disk I/O error` after the current-main image rollout.
- Relevant metrics: AKS was Running on prior image `6cc6561f29f5`; current `origin/main` image tag was `ce7f413b707a`; SQLite integrity checks passed, but an ordered scan of `beads.db` failed after 1469 rows because `/tmp` was read-only.
- Context: the chart intentionally uses `readOnlyRootFilesystem`, but the container did not mount a writable `/tmp`; Git also could not persist `safe.directory` to `/home/caco/.gitconfig` because the image home is read-only.

## After state

- Failing tests: none in targeted AKS validation.
- Relevant metrics: Helm release `cacophony-aks` reached revision 30 on image `harryaskhamcacoacr.azurecr.io/cacophony:ce7f413b707a`; `caco @cluster:caco-aks version --json` reports `1.2.561`; all role pods are Running; `caco @cluster:caco-aks bd sync` succeeds with `exported: 3845`, `pulled: false`, `pushed: true`.
- Context: the Helm chart now mounts an `emptyDir` at `/tmp` and carries Git `safe.directory` configuration via `extraEnv`; the isolated AKS board has `bd-2d1ffe` and `bd-9a699d` closed and the wrong-RG RBAC action back in draft.

## Diff summary

- Commits: `062f699e4`
- Files touched: `deploy/helm/cacophony/templates/statefulset.yaml`, `deploy/helm/cacophony/values.yaml`, `deploy/helm/README.md`, `deploy/helm/validate.sh`, `deploy/aks/PRODUCTION-ROLLOUT.md`
- Tests: `./deploy/helm/validate.sh`, `./deploy/aks/validate.sh`, live `caco @cluster:caco-aks bd sync`, live `just aks-self-dry-run-lite`, live `caco @cluster:caco-aks status --json true`, and real-TTY `caco @cluster:caco-aks tui` smoke through `tmux-cli`.
- Behavioural delta: read-only-root AKS pods now have writable temp space for SQLite/Git operations and do not need to mutate `~/.gitconfig` to trust the state/checkouts.

## Operator-takeaway

AKS is now on current main, private `@cluster` TUI access works from this machine, and the in-cluster isolated bead store can sync cleanly instead of failing on read-only `/tmp`.
