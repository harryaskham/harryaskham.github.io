# Session summary — SSH pty-request model (bd-a1a358 S5-prep)

## Goal

Router-sanctioned compile-validatable SSH model slice: the pty allocation /
terminal-geometry model the SSH terminal session (S5) sends on connect + resize.
Pure, sshj-agnostic. Final load-light foundation slice before the emulator-gated
SSH connect (S3b) — the remaining SSH work (connect/pool/session) waits for a
working android QA env.

## Bead(s)

- bd-a1a358 S5-prep (child bead filed but proxy-read-unresolvable this window;
  work landed under the parent epic reference, child reconciled when reads recover).

## Before state

- Failing tests: none.
- No pty/terminal-geometry model for an SSH shell channel.

## After state

- Failing tests: none. New `SshPtyRequestTest` 4/4 green; `:app:testDebugUnitTest`
  SUCCESSFUL.
- `SshPtyRequest(termType, cols, rows)` + `sshPtyRequest(cols,rows,termType)`
  (blank term -> xterm-256color default; reject non-positive geometry) +
  `sshPtyResizeNeeded(current,cols,rows)` (positive + changed) for SSH
  window-change requests.

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `connection/SshPtyRequest.kt` (new) — model + builder + resize predicate.
  - test `SshPtyRequestTest.kt` (new) — 4 tests (valid build, blank-term default,
    reject non-positive, resize-only-on-real-change).
- Tests: +4, -0, flipped 0.
- Behavioural delta: none yet (pure model; not wired).

## Embedded artefacts

- None. Pure model, fully unit-tested.

## Operator-takeaway

Completes the load-light SSH foundation (S1 target, S2 key format/source, S3a
host-key/format policy, S5-prep pty geometry) — all pure + unit-tested + landed.
The remaining SSH work (S3b sshj connect + BouncyCastle Android runtime, S4 pool,
S5 session I/O) is emulator/runtime-gated and waits for a working android QA env
(WSL2 ms-dev-2 emulator is a confirmed host blocker; infra bead filed). Also filed
the WSL2-emulator-QA infra escalation bead per the router's request.
