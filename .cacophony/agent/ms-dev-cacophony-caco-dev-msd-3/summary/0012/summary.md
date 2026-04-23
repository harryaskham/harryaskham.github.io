# Session summary — bd-fe65b4: discarded-SHA audit logging

## Goal

Surface the local-only commits that `force_reset_checkout_to_remote`
silently discards during divergence-recovery, so the bd-cb44d9 shape
(premature bead-close → push fails → reset --hard rolls back the
commit but the close persists) can no longer happen invisibly.

## Bead(s)

- `bd-fe65b4` — Atomic close gate: beads-db must NOT mark bead
  closed before git push to origin succeeds (root cause of bd-cb44d9)

## Before state

`force_reset_checkout_to_remote` ran:

```
git merge --abort
git rebase --abort
git reset --hard HEAD
git fetch <remote> <target>
git checkout --force <target>
git reset --hard <remote>/<target>
```

— with no logging of which local commits were about to be dropped.
Daemon reflog evidence (per postmortem): bd-a2ace0 had two distinct
landed commits (572feaaa + 467f3eb7) reset away in ~1 minute; the
beads-db retained `closed` for the bead, but origin/main had no
sign of the work.

## After state

Before the destructive `reset --hard <remote>/<target>`, the
function now `rev-list <remote>/<target>..HEAD --format='%H %s'`s
the local-only commits and writes them to stderr with a `DISCARDED`
prefix and a recovery hint that explicitly names the bd-cb44d9
shape ("if any of those SHAs reference a closed bead, the close
was premature; cherry-pick onto a fresh agent branch and re-
reintegrate, then reopen the closed beads").

The reset itself is preserved — changing it to a rebase here is
too risky because this code path is called from contexts where the
canonical checkout is expected to mirror remote bit-for-bit (e.g.
canonical-checkout reconcile after a conflicted merge attempt).
The minimum defensible improvement is "no longer silent".

## Diff summary

- `crates/caco-daemon/src/reintegration.rs` (+135):
  - `force_reset_checkout_to_remote` adds rev-list capture + stderr
    audit block before the reset.
  - New test `force_reset_checkout_to_remote_logs_discarded_commits`
    seeds an upstream, makes a local-only commit, calls the
    function, asserts HEAD ends at origin/main and the local-only
    file is gone (semantic outcome — logging itself is best-effort
    eprintln so isn't scraped by the test).

## Embedded artefacts

(none)

## Operator-takeaway

Acceptance criterion #4 of bd-fe65b4 is now satisfied. The other
three (reorder close-after-push, leave bead in_progress on push
failure, replace reset with rebase where safe) require a larger
refactor of `close_beads_for_completed_agent_notify` and the
`flush_and_mark_push → async sync loop` sequencing. The bead is
left unclosed by this commit so a follow-on cycle can pick up the
ordering work; this commit alone is the smallest change that
guarantees the next bd-cb44d9 incident leaves a forensic trail
instead of disappearing without a trace.

Sibling bead `bd-53f5a7` (shrink-cap defense, still unclaimed at
time of writing — caco-ctrl alert mentioned 5 destructive
reconciles in the last hour) is the bd-store-level equivalent
defence and is complementary; both should land together for the
strongest protection.
