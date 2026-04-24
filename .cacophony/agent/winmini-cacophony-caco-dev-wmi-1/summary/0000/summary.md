# Session summary — bd-b335a5: auto-claim skip EPIC beads

## Goal

Workers cannot meaningfully implement an EPIC umbrella bead. The
observed pattern across po4-5 sessions (bd-1401f4 + bd-ab3050 +
bd-56ca56) was `caco bd claim --project <P>` (no --bead-id)
consistently returning bd-9496d1 (the [EPIC] STT hardening
umbrella) because it was the highest-priority open unassigned
bead, forcing every worker to unclaim and pick deliberately —
defeating the auto-claim path. Fix: in the daemon's claim
resolver, exclude beads where bead_type=Epic OR title starts with
'[EPIC]' from the no-bead-id auto-claim queue. EPICs remain
claimable explicitly via --bead-id.

## Bead(s)

- `bd-b335a5` — auto-claim should skip type=epic and bd-titles
  starting with [EPIC] (P3 task). Filed via reflect-session
  pattern from bd-1401f4 session.

## Before state

```
$ caco bd claim --project cacophony       # no --bead-id
claimed: bd-9496d1 — [EPIC] stt-xplat hardening umbrella ...
                                          # umbrella, can't implement

$ caco bd unclaim --bead-id bd-9496d1
unclaimed.
```

Pattern repeats across every fresh worker spawn that uses the
no-id auto-claim; the highest-priority open EPIC dominates.

## After state

```
$ caco bd claim --project cacophony       # no --bead-id
claimed: bd-X — <next ready non-EPIC bead>

$ caco bd claim --bead-id bd-9496d1       # explicit still works
claimed: bd-9496d1 — [EPIC] ...           # operator opt-in
```

The auto-claim path now skips:
- beads where `bead_type == BeadType::Epic` (canonical type
  marker)
- beads whose title starts with `[EPIC]` (legacy convention for
  beads filed without setting --type epic)

Both gates apply only to `claim_next_ready` (the no-bead-id auto-
claim path). Explicit `claim_bead(bead_id, ...)` is unchanged so
operators can still hand-pick an EPIC.

## Diff summary

- 1 file changed, +56 / -1 (`crates/caco-beads/src/store.rs`):
  - `BeadsStore::claim_next_ready`: added the type/title skip
    inside the candidate loop, before `claim_bead`.
  - New unit test
    `claim_next_ready_skips_epic_type_and_epic_titled_beads`:
    inserts a P0 EPIC by type, a P0 EPIC by title prefix, and a
    P1 ordinary task; asserts the auto-claim returns the P1 task.

## Validation

- `cargo test -p caco-beads --lib claim_next_ready`: all 8
  pre-existing tests + the new test pass.
- `cargo check --workspace`: clean.

## Operator-takeaway

The 'every fresh worker auto-claims the umbrella' footgun is
closed. Workers spawning into `caco bd claim --project X` (no
id) now skip past the EPIC backlog and land on something
implementable. EPICs remain explicit-claim-only — owners /
controllers can still target them deliberately.

Push-discipline (post-clarification): own-branch push allowed;
default-branch force-push banned; only reintegrate / complete
land work on main. This session continues to use only local refs
+ daemon-mediated reintegrate.
