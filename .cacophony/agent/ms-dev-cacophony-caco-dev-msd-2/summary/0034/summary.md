# Session summary — bd-fe9a46 (idle-worker auto-resume)

## Bead
bd-fe9a46 (P1) — the bd-ab9442 root fix #2: idle endless/dev workers go quiescent
after a land and don't auto-claim the next even when ready supply > 0, requiring a
manual controller "spike" re-nudge. Detection (bd-ab9442) shipped; auto-resume did
not. Taken per Harry's "don't gate on session-freshness/depth, all senior devs"
directive + ctrl's Option-(a) design endorsement (the bead was earlier deferred to
a fresh session, which Harry explicitly lifted).

## Fix (Option a — LLM-delegated, .cacophony/pi/self-nudge/extensions/caco-self-nudge.mjs)
The self-nudge idle-branch nudge text previously told an idle worker to "return to
idle without inventing work." Now, when idle + ready board supply > 0, it
encourages resuming a claim — with the keystone delegated to the worker (LLM):
- collectReadySupply(): raw ready count via `caco bd list --ready --count-only`
  (gathered only when no active claim). Coarse trigger; null on error (conservative).
- shouldNudgeIdleClaim({appearsActive, readySupply}): pure, exported predicate —
  !appearsActive && readySupply finite && > 0. Conservative on unknown supply.
- Idle-branch claim-instruct wording: claim via the atomic no-ID claim UNLESS under
  a deliberate controller hold/stagger/livelock-hold (worker self-suppresses if
  held = the deliberate-hold-suppression KEYSTONE); confirm not already in_progress
  (dedup-safe, repeated nudge can't double-claim); the raw count overcounts
  gated/specialist/coordinate-first so assess + unclaim+--exclude-bead-id if
  device-bound/specialist/operator-gated/coordinate-first; stay idle-ready if
  nothing clean. (Clean-dispatchable filtering delegated to the worker.)

## Validation
- JS tests (node --test caco-self-nudge.test.mjs): 14/14 pass, incl. 3 new:
  - shouldNudgeIdleClaim unit test (idle+ready->true; active->false; ready 0/null/
    NaN/undefined->false; missing args->false).
  - kill-switch test (CACO_PI_SELF_NUDGE_CLAIM_DISABLED=1 -> plain idle text). integration test: idle worker + ready supply=5 -> nudge contains the
    claim-instruct + hold-suppression + dedup wording, and NOT the plain idle text.
  - fake caco extended (additive) to differentiate `bd list --ready`; existing idle
    tests unaffected (readySupply=0 when unset -> idle text preserved). Zero regressions.
- JS overlay change (no Rust). Docs updated in lockstep: pi-self-nudge.md
  (contract change + keystone) + README.md mention; docs/profiles.html up-to-date.

## Post-design-check tweaks (ctrl-approved)
- Capability-not-lane exclusion wording: a capable specialist bead you CAN run here
  (caco-web/caco-tui/backend-Rust/Android build+unit-test) is keepable; only exclude
  can't-run-here (device-bound), operator-gated, or coordinate-first (aligns with
  Harry's capable-specialist directive).
- CACO_PI_SELF_NUDGE_CLAIM_DISABLED op-gate kill-switch (reverts to plain idle text).
- Double-claim close-firing edge bounded by confirm-not-in_progress + sequential
  nudge processing + atomic no-ID claim.

## Diff summary
See the reintegration receipt for the landed squash SHA. Files:
.cacophony/pi/self-nudge/extensions/caco-self-nudge.mjs (+collectReadySupply,
shouldNudgeIdleClaim, idle-branch), .test.mjs (+2 tests, fake-caco --ready),
.cacophony/profiles/pi-self-nudge.md, README.md.
