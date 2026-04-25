# Session summary — release runner stale target cleanup

## Goal

Fix `bd-a47c5d`, where self-hosted Linux release builds failed across DynamicUser runs because checkout-local `target/` persisted with ownership from a previous ephemeral uid.

## Bead(s)

- `bd-a47c5d` — release.yml: x86_64 build perm-denied on stale target/ across DynamicUser runs

## Before state

- Failing tests: release workflow x86_64 build failed in GitHub Actions with `Permission denied (os error 13)` writing first compile artifacts under a stale `target/` directory.
- Relevant metrics: affected recent release tags included v1.2.541, v1.2.542, v1.2.551, and v1.2.552; manual recovery required `sudo rm -rf target` and rerun.
- Context: Linux self-hosted runners use `DynamicUser=yes`, so persisted checkout workdirs can retain old uid ownership between runs.

## After state

- Failing tests: not run end-to-end because this is a GitHub Actions production-runner condition.
- Relevant metrics: `.github/workflows/release.yml` parses as valid YAML after the change.
- Context: Linux release jobs now remove checkout-local `target/` immediately after disk cleanup and before any Cargo build, trying normal `rm -rf` first and falling back to `sudo rm -rf` when stale uid ownership blocks removal.

## Diff summary

- Commits: `338e4a15d`
- Files touched: `.github/workflows/release.yml`
- Tests: YAML parse check with Python.
- Behavioural delta: stale target directories from previous DynamicUser runs should no longer block the next Linux release build.

## Operator-takeaway

The x86_64 release lane now performs the same cleanup operators were doing manually, making release retries less dependent on SSHing into the runner to delete `target/`.
