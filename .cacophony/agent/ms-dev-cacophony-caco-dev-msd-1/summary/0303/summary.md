# Session summary — bd-d4b93b watchdog-c (mode-2), slice-1a

## Goal
Implement mode-2 (watchdog-c) of bd-d4b93b — the reliable detection of a canonical-checkout
reintegration *wedge*: a reint stuck/retrying in the 'prepare daemon checkout' phase under
degraded egress, holding the reintegration lock and freezing main, that never times out or
releases. ctrl-routed to the ms-dev heavy-daemon-Rust lane; ctrl-endorsed as ESCALATE-ONLY
first (no auto-abort of a live reint in the first land — that is the higher-risk follow-on).

## Bead(s)
- bd-d4b93b (in_progress, mine) — slice-1a landed here; remains in_progress for the
  follow-on slices (1b proactive caco ops finding; 1c case-(b) daemon-parented hung-op
  escalate-detect; later auto-abort+respawn + true-remote main-frozen calibration).
- Mode-1 (reaper-a, orphan-saturation) already landed separately (54775654d) — not this slice.

## Before state
- The reintegration lock re-acquires with FRESH metadata on every merge-queue retry, so the
  lock `acquired_at` age resets to ~1s each retry — lock-age LIES about how long a checkout has
  truly been stuck. In-flight-record-age is also unreliable (phantom rows, bd-444041). A
  genuinely wedged prepare (held + main frozen + retrying for many minutes) was therefore
  INVISIBLE: `cancel-stale-lock` correctly refuses a live holder, and nothing surfaced the true
  stuck-duration, so a wedge stayed silent until an operator noticed frozen main.

## After state (slice-1a — detection foundation + contention-point surfacing)
- New module-static persisted per-reint phase tracker in `reintegration.rs`, keyed on the
  canonical checkout path: `record_reint_phase` updates `phase_entered_at` ONLY on a phase
  CHANGE, so a lock re-acquire that re-enters the same phase keeps the original clock. This is
  the fix for the lock-age reset — the across-retry phase-duration is the reliable stuck signal.
- Pure wedge predicate `reint_prepare_wedge_detected(phase_duration, holder_alive, main_frozen,
  threshold)` = ALL of live-holder AND frozen-main AND duration ≥ threshold (10min). Lock-age
  and in-flight-row-count are deliberately NOT inputs (the false-signal traps).
- `observe_canonical_main_frozen` tracks the canonical main ref across observations (window, not
  a single snapshot) and resets the phase clock when main advances (wedge resolved).
  `detect_canonical_prepare_wedge` combines the three signals; returns `Some(secs)` only when
  wedged. ESCALATE-ONLY — never aborts.
- Consumer (immediate operator value): the held-lock diagnostic in
  `CanonicalCheckoutLock::acquire_with_metadata` now surfaces the TRUE across-retry
  phase-duration instead of the lying lock-age, and flags a `SUSPECTED WEDGE … Escalate to
  ctrl/operator (caco ops)` when the three reliable signals line up — so a blocked agent/operator
  at the contention point sees the real stuck signal, not a ~1s lock age.
- `record_reint_phase` wired at the 'prepare' acquire site; the phase string is now a shared
  `CANONICAL_PREPARE_PHASE` const so the lock metadata and the tracker can never drift apart.
- 10 new unit tests (the phase-change-only-reset invariant, persistence across re-record,
  saturating duration, the all-three-signals predicate, main-frozen window semantics, and the
  detect-helper's wedge/no-wedge cases). `cargo test -p caco-daemon --lib bd_d4b93b` green.
- Purely OBSERVATIONAL: records to a static map + enriches an error-path diagnostic; no change
  to any reintegration flow decision, so it cannot break the reint path.

## Diff summary
Single file: `crates/caco-daemon/src/reintegration.rs` (+392/-7). Adds the phase tracker,
main-ref observer, pure predicate, detect helper, the held-lock diagnostic enrichment, the
prepare-site `record_reint_phase` wiring, the `CANONICAL_PREPARE_PHASE` const, and the 10 unit
tests. (Final landed squash SHA: see the reintegration receipt.)

## Operator takeaway
A wedged canonical-checkout prepare is no longer silent at the point of contention: anyone
blocked on the held reint lock now sees the TRUE across-retry phase-duration and an explicit
SUSPECTED WEDGE / escalate hint instead of a misleading ~1s lock age. This is escalate-only
detection — it never aborts a live reint. Calibration from the ms-mac incident responders
(doctor/caco-vm/msm-2/transcript-bead-filer) is captured on the bead: a proactive `caco ops`
finding (1b), the daemon-parented hung-op detector (1c), and true-remote main-frozen + auto-abort
are the reviewed follow-ons.
