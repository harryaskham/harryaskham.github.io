# Technical-writer review summary

## Goal

Fix a current docs-correctness defect: the AGENTS.md / reintegration-policy
wording on PR-backed reintegration implied an async "keep the runtime alive until
the PR lands" model, which actively misled a fleet design thread. Correct it to
the code-accurate synchronous behavior.

## Bead(s)

- `bd-c63005` — technical-writer documentation maintenance.
- Related: PR auto-merge rollout discussion; `bd-571549` (AutoMerge routes to open+squash), `bd-89088a` (mirror-lag disposition false-failure).

## Before state

- `AGENTS.md` line 63 said PR-mode reintegration "keeps the runtime alive until the PR actually lands" — read by several agents (aur-5, md2-0, sonance, msd-0) as an async wait. Code reality (ms-dev-2-ctrl/md2-1, reintegration.rs:11152 / lib.rs:52546): the reintegrate call runs `gh pr merge --squash` synchronously and returns `success:false` on unmergeable; PR #6 merged in 3s with `auto_merge=NULL`.

## After state

- `AGENTS.md` and `docs/reintegration-policy.md`/`.html` now state PR-backed reintegration runs the merge synchronously in-process, errors with synchronous bounded rebase+retry on drift (dead-letter on persistent drift), and that the only "kept-alive" guarantee is the agent-lifecycle one (PR modes do not auto-recreate the agent; recreate is a separate operator step). SPEC left unchanged (two-layer-correct, per no-SPEC-rewrite rule).
- Validation: `./docs/validate-pages.sh` passed; sibling marker refreshed.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `AGENTS.md`, `docs/reintegration-policy.md`, `docs/reintegration-policy.html`.
- Behavioural delta: documentation only.

## Operator-takeaway

The PR-mode reintegration behavior docs are now code-accurate single-truth:
synchronous merge-or-explicit-fail, no async wait; "non-destructive" means
no auto-recreate, not async-keep-alive. Pending follow-up: the flip-gated
safety-posture note + `bd-89088a` mirror-lag verify workaround when the
cacophony PR flip lands, the 2026-06-22 changelog day, and the sparse->git-LFS change.
