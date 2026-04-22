# Session summary — broken-on-main root-cause investigation (bd-ee1696)

## Goal

Document why 12+ broken-on-main waves persisted this session
despite the `cacophony-fast-tests` gate. Determine whether
bd-29bf2b's gate upgrade is sufficient.

## Bead(s)

- `bd-ee1696` — Investigation: broken-on-main waves vs gate (P3 task)

## Before state

- Pattern observed but undiagnosed; bd-29bf2b assumed gate-command
  upgrade was the root fix.
- No reference doc for future broken-on-main beads to link.

## After state

- `docs/investigations/bd-ee1696-broken-on-main-root-cause.md`
  (155 lines) documents stale-base reintegration as true root
  cause, with table of 5 analyzed waves and 3 concrete
  countermeasures (immediate rebase-check, mixin-composition audit,
  bd-2c399b queue daemon).

## Findings

- bd-29bf2b's `cargo test --lib --workspace` upgrade is
  necessary but not sufficient.
- Root cause is **stale-base reintegration**: agent gate runs
  on tree-built-on-`main@T0`; peer's struct-add lands at `T1`;
  agent submits at `T2` and the merge produces a tree where
  the stale fixture meets the new struct.
- Most waves analyzed (bd-a1ec44, bd-ab1c38, bd-f72c32,
  bd-f4f4cd, bd-bce6ea) WOULD have been caught by the agent's
  own gate had the agent rebased *after* peer's reintegration.
- bd-b9c9eb (clippy lint) requires `clippy: true` in mixin —
  already set; suggests some agents are not composing the
  mixin at all.

## Proposed follow-ups

1. Rebase-check IMMEDIATELY before submit (cheap, closes most
   of stale-base hole).
2. Default `clippy: true` already exists; audit whether agents
   are actually composing `cacophony-fast-tests`.
3. bd-2c399b (queue daemon) — long-term canonical fix.

## Diff summary

- Files touched (+155 / 0):
  - `docs/investigations/bd-ee1696-broken-on-main-root-cause.md` — new

## Verification

- Doc-only; no code touched.

## Operator-takeaway

bd-29bf2b is a 5% defense-in-depth improvement. The real fix is
bd-2c399b (queue daemon) or a tighter rebase-check window.
Until then, expect 1-3 broken-on-main waves per high-throughput
merge day. Future broken-on-main beads should link to this doc.
