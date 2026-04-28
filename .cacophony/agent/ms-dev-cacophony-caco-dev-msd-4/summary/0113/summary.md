# Session summary — Picasso checkout artefact blocker cleanup

## Goal

Fix `bd-0f29b3`, where the `picasso` canonical daemon checkout on ms-mac could not refresh because old generated `.cacophony` session artefacts were left as untracked files and collided with paths tracked by the target branch.

## Bead(s)

- `bd-0f29b3` — `[log-monitor] picasso canonical checkout refresh blocked by untracked .cacophony artefacts`

## Before state

- Failing tests: no local test failure; live evidence was from ms-mac daemon logs.
- Relevant metrics: ms-mac `picasso` checkout had hundreds of `.cacophony/agent/...` and legacy `.cacophony/agents/*/reflect/...` artefact paths in a dirty state, and `git checkout harryaskham/health/main` had previously failed with “untracked working tree files would be overwritten by checkout”.
- Context: Canonical checkouts are supposed to be clean default-branch object sources, but older runtimes could leave generated state in the checkout before the normal post-checkout clean step ran.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: ms-mac `picasso` canonical checkout was archived to `/Users/harryaskham/.cacophony/build-artifacts/bd-0f29b3-picasso-cacophony-before-clean-20260428T065722Z.tgz`, reset to `harryaskham/health/main` at `8ef12df1ec`, and verified `cacophony_status=clean`; a self-checkout of the same branch succeeded.
- Context: Daemon checkout refresh now pre-cleans untracked `.cacophony/agent` generated artefacts and, if checkout still fails with Git’s untracked-overwrite diagnostic, retries only after cleaning blocker paths classified as known generated Cacophony artefacts. Mixed blocker lists containing arbitrary paths still fail.

## Diff summary

- Commits: implementation commit `bd-0f29b3: clean generated checkout artefact blockers` plus this summary commit.
- Files touched: `crates/caco-daemon/src/checkout.rs`, `SPEC.md`.
- Tests: added `refresh_cleans_generated_cacophony_artifact_checkout_blockers_bd_0f29b3` and `checkout_artifact_blocker_classifier_rejects_arbitrary_paths_bd_0f29b3`.
- Behavioural delta: canonical checkout refresh can converge past stale generated session artefacts without silently deleting arbitrary untracked project files.
- Validation: `cargo fmt --all -- --check`; `cargo test -p caco-daemon bd_0f29b3 -- --nocapture`; live ms-mac picasso checkout clean/self-checkout verification.

## Operator-takeaway

The live picasso checkout is unwedged now, and future daemon refreshes have a narrow generated-artefact cleanup path instead of failing before the existing reset/clean convergence can run.
