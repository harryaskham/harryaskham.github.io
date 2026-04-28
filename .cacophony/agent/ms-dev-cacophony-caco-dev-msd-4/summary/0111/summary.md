# Session summary — PR-backend no-URL stale-runtime guidance

## Goal

Clarify the operator guidance for a recurrent direct reintegration anomaly where a PR-backend path reports `published fork/main but no PR URL` after a worker expected direct/local-merge behavior. The goal was to make the docs reflect the current source guard and tell operators how to classify this symptom without falsely closing implementation beads.

## Bead(s)

- `bd-d45b0e` — `[docs] direct reintegrate still reports PR-backend no-URL after publishing fork/main`

## Before state

- Failing tests: none known for this doc-only bead.
- Relevant metrics: technical-writer evidence reported `caco agent reintegrate --mode direct` returning `bd-1d514b: direct intent over pull_request backend published fork/main but no PR URL was available`, while the pushed branch appeared to contain the documentation commits.
- Context: Current source already has a pre-publish guard for direct PR-backend flows that would push to a branch with the same name as the PR base. The docs still treated all no-URL-after-publish cases as generic PR-backed ambiguity and did not call out stale runtime/config skew for projects expected to be `default_intent: direct` + `backend: local_merge`.

## After state

- Failing tests: none in documentation validation.
- Relevant metrics: `docs/reintegration-policy.md` and its styled HTML sibling now state that current builds must refuse `fork/main` against `upstream/main` before publishing; if an operator still sees `published fork/main but no PR URL`, they should record the running `caco version` with the branch SHAs and treat it as stale-runtime evidence.
- Context: Added an explicit direct/local-merge fallback section: when project policy says direct/local_merge, `--mode direct` should land on `integration.reintegrate_target` directly and should not attempt PR creation.

## Diff summary

- Commits: implementation commit `bd-d45b0e: document stale PR-backend no-url signal` plus the summary-only commit for this record.
- Files touched: `docs/reintegration-policy.md`, `docs/reintegration-policy.html`.
- Tests: documentation validation only.
- Behavioural delta: no runtime code changed; operator guidance now distinguishes stale-runtime PR-backend no-URL symptoms from current-source local direct reintegration behavior.
- Validation: `docs/validate-pages.sh`; `cargo fmt --all -- --check`.

## Operator-takeaway

If a worker on the current direct/local-merge topology sees `published fork/main but no PR URL`, do not assume current source intentionally published main through the PR backend. Preserve evidence, check the node's running binary/config, and treat that wording as likely stale runtime or misconfigured backend until proven otherwise.
