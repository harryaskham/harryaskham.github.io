# bd-ebdf72: enrich reintegration non-FF push errors with verified recovery routine

## Goal

Replace the bare git stderr (`error: failed to push some refs to '...' / hint: Updates were rejected because the tip...`) that surfaces from `caco agent reintegrate` and `caco agent complete` on a non-fast-forward squash-push race, with an enriched error that includes the **verified** recovery routine I've personally walked through 5+ times this session.

## Bead(s)

- bd-ebdf72 (P2 footgun bug; closes via reintegrate)

## Before state

When the squash-push to main lost a race with another reintegrate, the operator saw only:
```
error: failed to push some refs to '/home/.../checkouts/cacophony'
hint: Updates were rejected because the tip of your current branch is behind
hint: its remote counterpart.
hint: ...
```

Critically, the underlying `caco-daemon::reintegration::reintegrate` flow may have left the checkout on the `main` branch mid-rebase, which means a subsequent `git rev-parse HEAD` captures `main`'s tip, not the work-in-flight commit. The standard "rebase routine" then applies main onto main, the cherry-pick is empty, and the work disappears (still recoverable via `git reflog` but operator/agent lost time + composure).

I lost a commit this way 3 separate times this evening before figuring out the `git checkout agent/<branch>` step had to come first.

## After state

New helper `enrich_reintegration_error<E: std::fmt::Display>(e: E) -> CliError` in `crates/caco-cli/src/lib.rs` (just above `dispatch_agent_complete`), called via `.map_err(enrich_reintegration_error)` from both `dispatch_agent_complete` and `dispatch_agent_reintegrate`. When the message contains `failed to push some refs`, `non-fast-forward`, or `updates were rejected` (case-insensitive), the original error is preserved verbatim and followed by:

```
bd-ebdf72 recovery routine (verified): the squash-push to main lost a race
with another reintegrate. Your work is safe in `git reflog`. To recover:

1. cd <agent checkout>
2. git checkout agent/<branch>     # CRITICAL — reintegrate may have left HEAD on `main`
3. WORK_SHA=$(git reflog | grep -m1 '<your bead id>' | awk '{print $1}')
4. git fetch origin agent/<branch>
5. git reset --hard origin/agent/<branch>
6. git fetch origin main && git merge --no-edit -X theirs origin/main
7. git cherry-pick $WORK_SHA
8. cargo test-small && cargo clippy --workspace --all-targets -- -D warnings
9. caco agent reintegrate --id <id> --mode direct,recorded
```

3 unit tests pin behaviour:
- `enrich_reintegration_error_passes_unrelated_errors_through` (no false positives)
- `enrich_reintegration_error_annotates_failed_to_push_some_refs` (positive case + original preserved)
- `enrich_reintegration_error_annotates_non_fast_forward` (alternate trigger)

Verification:
- `cargo test -p caco-cli --lib enrich_reintegration_error`: 3/3 PASS
- `cargo test-small`: 57/57 PASS (one flaky tui test on first run, passed on retry; not related)
- `cargo clippy --workspace --all-targets -- -D warnings`: clean

## Diff summary

1 file changed, +73 / −2:

- `crates/caco-cli/src/lib.rs`:
  - +35 / −0 helper `enrich_reintegration_error`
  - +0 / −2 (`.map_err(|e| CliError::new(e.to_string()))` → `.map_err(enrich_reintegration_error)` at 2 sites)
  - +30 / −0 unit tests

## Operator-takeaway

**This is fix B from the bead** (annotate the error) — fix A (leave checkout on agent branch, not main) is a deeper structural change in the daemon's reintegration flow and should be a follow-up bead. Fix B is a strict improvement: the operator/agent now sees the recovery routine **inline with the error**, can copy-paste it, and avoids losing work to the wrong recovery path.

The recovery routine itself is "verified" because I've now walked through it under many race conditions tonight, including conditions where `caco agent reintegrate` itself left the checkout in surprising states. Step 2 (`git checkout agent/<branch>`) is the one I missed first that cost me commits.

Future work:
- Fix A: daemon should leave checkout on agent branch on push failure (structural, follow-up bead)
- Auto-recovery wrapper: `caco agent reintegrate --auto-recover` could automate the 9-step routine when it detects the non-FF case (but introduces the risk of merging the wrong commit if reflog parsing goes wrong; defer until manual recovery is reliable for ≥1 week)

This cycle: another quiet broken-on-main-free cycle (fifth in a row).
