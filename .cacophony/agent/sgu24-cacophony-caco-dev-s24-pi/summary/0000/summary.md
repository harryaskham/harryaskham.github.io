# Session summary — lightweight phone dev profile

## Goal

Create a phone-friendly persistent Pi development profile for `caco-dev-s24-pi` so future launches keep the Pi runtime conveniences but avoid the full generic dev worker behaviours that encourage auto-claiming, collab-mode, local build/test loops, or other heavyweight work on Harry's phone.

## Bead(s)

- Operator request — add `light-dev.yaml` and point `caco-dev-s24-pi` at it for the next launch.

## Before state

- Failing tests: none run; this was profile/config documentation work on a low-power phone node.
- Relevant metrics: `caco-dev-s24-pi` imported `cacophony_dev.yaml` and appended `caco-mobile`, so it still inherited the generic dev, auto-claim, collab-mode, merge-queue, and worker validation posture.
- Context: The existing `caco-mobile` mixin narrowed local work but did not remove the full dev worker lifecycle from the S24 Pi persistent declaration.

## After state

- Failing tests: none run locally; only lightweight source/structural checks were performed.
- Relevant metrics: `caco-dev-s24-pi` now imports `.cacophony/agents/light-dev.yaml`, which imports `pi-common.yaml` and composes `self-improvement`, `session-recording`, `reflect-session`, `endless`, `light-dev`, and `caco-mobile` without `dev`, `auto-claim`, `collab-mode`, or `merge-queue`.
- Context: Future materializations of the S24 Pi worker should retain Pi inbox/loop/self-compact/self-nudge/self-ops/sudo/caco-command/speak/image/Tendril/tmux/git-check support while treating work selection and validation as lightweight and operator-directed.

## Diff summary

- Code/content commits: `1cac8333d`.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched: `.cacophony/profiles/light-dev.md`, `.cacophony/agents/light-dev.yaml`, `.cacophony/agents/cacophony_persistent.yaml`, `AGENTS.md`, `README.md`.
- Tests: +0 / -0 / flipped 0. Ran `git diff --check` and lightweight Python structural checks for the new snippet and S24 declaration.
- Behavioural delta: The phone Pi persistent worker no longer inherits the generic full dev queue-draining profile; it keeps explicit lightweight development ability and uses queued/remote validation for compiler-heavy proof.

## Operator-takeaway

`caco-dev-s24-pi` is now configured for the next launch as a low-power Pi dev worker: useful for explicit small edits and coordination, but not a phone-host build/test/auto-claim worker.
