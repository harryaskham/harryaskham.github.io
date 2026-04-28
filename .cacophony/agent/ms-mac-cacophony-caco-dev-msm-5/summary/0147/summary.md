# Session summary — align SPEC agent-log contract

## Goal

Bring the normative SPEC back in line with the current post-bd-aa882f logging architecture: structured runtime logs are the canonical historical session source, while legacy `logs/session.log` pipe-pane captures are compatibility/reclaimable artifacts only.

## Bead(s)

- `bd-46c6dd` — [docs] SPEC still treats logs/session.log as canonical after pipe-pane removal

## Before state

- Failing tests: none known for this docs-only scope.
- Relevant metrics: `grep` still found SPEC language saying `logs/session.log` was the canonical persisted raw execution log and that `caco agent logs` should prefer it when present.
- Context: `docs/logs.md` and `docs/logs.html` already described the bd-aa882f removal of unbounded tmux pipe-pane capture, but SPEC sections 16.6.2 / 16.7.2 and related TUI/persistent-agent text still used stale raw-log terminology.

## After state

- Failing tests: none observed.
- Relevant metrics: stale phrase check passes for `Lifecycle Raw Execution Logs`, `logs/session.log is the canonical`, `persisted raw lifecycle log`, and `raw-log capture`.
- Context: SPEC now says modern managed agents use runtime structured logs, live pane viewing is separate, `logs/session.log` and `logs/capture.json` are legacy-only, and TUI/agent-detail companion surfaces refer to structured historical logs.

## Diff summary

- Commits: `ead9464ec`
- Files touched: `SPEC.md`
- Tests: source-only validation with `git diff --check` and targeted grep stale-phrase checks.
- Behavioural delta: documentation/spec-only change; no runtime behavior changed.

## Operator-takeaway

The product contract no longer tells implementers to recreate the removed unbounded `logs/session.log` pipe-pane path. Future logging work should build on structured runtime logs and treat legacy session logs as cleanup/reclaimable compatibility artifacts.
