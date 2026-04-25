# PR reintegration config switch

## Goal
Switch the Cacophony development profile and Cacophony project configuration to exercise PR-backed reintegration for newly recreated dev workers.

## Bead(s)
- Operator-requested follow-up to the PR-based reintegration workflow rollout.

## Before state
The `dev` profile defaulted to direct reintegration only, and the `cacophony` project did not declare explicit PR-backed integration policy/topology in repository config.

## After state
The `dev` profile defaults to `pr_review` while still allowing direct and recorded reintegration overrides. The `cacophony` project declares an explicit writable fork remote, PR target remote, `integration.pr_base`, `integration.reintegrate_target`, and `default_intent: review` / `backend: pull_request` policy.

## Diff summary
- Updated `.cacophony/profiles/dev.md` reintegration frontmatter from `direct` to `pr_review` and expanded allowed modes.
- Updated `.cacophony/projects.yaml` for the `cacophony` project with PR remotes and pull-request integration policy.
- Validated the materialized config with the project overlay.

## Validation
- `caco config validate --strict --project-config-dir .cacophony`

## Operator-takeaway
After this lands and the daemon reloads the config, recreating a dev worker should use PR review reintegration by default so the worker can test the updated PR workflow directly.
