# Session summary — agent pause forensic blind spots fixed

## Goal

Investigate the report that ms-mac silently paused several `caco-dev-*`
workers during a daemon version-mismatch window with no audit trail and a
"pause hook stamp missing" divergence marker. The key question was whether
there was a daemon-internal safety path that paused workers on mismatch, or
whether the forensic surfaces were lying.

## Bead(s)

- `bd-c8fc66` — `[daemon/lifecycle] ms-mac daemon silently paused 4/5 caco-dev agents during version-mismatch window (no CLI caller, no audit trail, no pause-hook stamp)`

## Before state

- Reported symptoms:
  - 4/5 ms-mac `caco-dev-*` agents observed in `paused`
  - no `agent pause` command audit event visible
  - pause-hook divergence marker reported as missing
  - timing happened during a version-mismatch window, so daemon-internal
    pause logic was suspected
- Code reality before this patch:
  - worker-agent `Paused` transitions only existed on the explicit pause path
  - single-agent pause emitted feed + command-audit side effects
  - bulk pause did **not** emit those side effects
  - `caco agent status` for remote agents discarded daemon-reported
    pause-stamp metadata and recomputed it locally in the CLI process

## After state

- Root-cause conclusion from code inspection:
  - there is **no** daemon-internal worker pause-on-version-mismatch path in
    the current tree
  - the stronger explanation is an explicit pause path whose observability was
    incomplete, plus a remote-status false-positive around pause-hook stamps
- Fixes landed:
  1. single and bulk pause now share one canonical pause side-effects helper
  2. bulk pause now emits the same per-agent feed / audit trail as single pause
  3. bulk pause now also preserves the same persistent-sentinel sync behavior
  4. `caco agent status` now preserves daemon-reported remote pause-stamp
     metadata instead of recomputing it locally and fabricating false
     divergence for remote agents
- Practical outcome:
  - future bulk pauses are no longer audit-silent
  - remote-agent pause diagnostics no longer falsely report
    `paused + missing stamp` just because the CLI is running on a different node

## Diff summary

- Files touched:
  - `crates/caco-daemon/src/lib.rs`
  - `crates/caco-cli/src/lib.rs`
- Validation:
  - `cargo build -p caco-cli`
  - `cargo test-small`
- Regression coverage:
  - added CLI regression test ensuring remote daemon-provided pause-stamp
    metadata wins over local recomputation for remote agents
- Behavioural delta:
  - `POST /api/v1/agents/pause` now has observability parity with
    `POST /api/v1/agents/{id}/pause`
  - remote status output is less misleading during pause forensics

## Operator-takeaway

The important result here is that the scary hypothesis — “the daemon silently
pauses worker agents during version mismatch” — is not supported by the code.
What *was* real was a forensic trap: bulk pause lacked audit/feed emission, and
remote status could misdiagnose pause-hook divergence. Those blind spots are
now fixed, so the next time an agent pause happens there should be a much more
credible trail to follow.
