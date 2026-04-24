# Session summary — bd-8299cc: private Android distribution comparison + recommendation

## Goal

Research the realistic options for privately distributing the Android
companion while keeping updates easy for end users, then turn that into a
published repo-local recommendation instead of leaving the answer trapped in a
bead description.

## Bead(s)

- `bd-8299cc` — `Research private Android app distribution options`

## Before state

Before this change:

- The repo already had Android APK build/upload workflows.
- The Pages site now surfaces APK downloads (bd-412eee), but there was no
  dedicated comparison of private-distribution strategies.
- Three closely related research beads existed in the queue:
  - `bd-8299cc` — broad comparison
  - `bd-01f27d` — private F-Droid repo evaluation
  - `bd-35fadf` — Google Play private distribution evaluation
- There was no published repo-local guidance on:
  - whether Play internal/closed testing or internal app sharing is the better
    update channel
  - how a private F-Droid repo changes onboarding and maintenance
  - which option best fits the “private + easy to update” requirement

## After state

Added a new published docs page:

- `docs/android-distribution.html`

It compares four options:

1. Google Play internal testing
2. Google Play closed testing
3. Google Play internal app sharing
4. Private/self-hosted F-Droid repository

The page now documents:

- onboarding flow for each option
- update delivery mechanism for each option
- operational overhead and maintenance trade-offs
- account / auth implications
- cost considerations
- a concrete recommendation

Recommendation landed in docs:

- **Best overall for easy private updates:**
  Google Play internal testing (or closed testing once the group outgrows the
  internal track)
- **Best for self-hosting / no Google dependency:**
  private F-Droid repo, but with materially higher setup and maintenance costs
- **Not recommended as the primary ongoing update channel:**
  Google Play internal app sharing, because it is better for ad-hoc QA than for
  continuous updates

Discoverability changes:

- `docs/index.html` now links to the Android distribution comparison from the
  homepage APK section
- `docs/wearable.html` now links to the comparison from the wearable install
  section

## Diff summary

Files touched:

- `docs/android-distribution.html`
- `docs/index.html`
- `docs/wearable.html`
- `.cacophony/agent/winmini-cacophony-caco-dev-wmi-2/summary/0026/summary.md`

Key content decisions:

1. **Play internal / closed testing recommended for ease-of-update**
   - Uses normal Play Store onboarding and update flow
   - Internal testing is best for small trusted groups
   - Closed testing is the natural next step for a larger private cohort

2. **Internal app sharing explicitly de-scoped as the main update answer**
   - Excellent for ad-hoc QA builds
   - Weak primary update channel because new builds imply new links and the
     sharing model is intentionally temporary

3. **F-Droid presented as viable but heavier**
   - Strongest privacy/self-hosting story
   - Requires repo hosting, signing-key handling, F-Droid client onboarding,
     and repo maintenance

4. **Official-source grounding**
   - Used official Google Play developer help docs and official F-Droid docs
   - Included source links directly on the page

## Verification

- `docs/validate-pages.sh` — PASS (`146 passed, 0 warnings, 0 failed`)
- manual repo-side sanity:
  - `docs/index.html` links to `android-distribution.html`
  - `docs/wearable.html` links to `android-distribution.html`
  - new page includes structural shell required by Pages validator

## Operator-takeaway

The repo now has a concrete, published answer to the “how should we privately
distribute Android builds?” question:

- If the goal is **private + easiest updates**, use **Google Play internal or
  closed testing**.
- If the goal is **private + self-hosted / no Google dependency**, use a
  **private F-Droid repo** and accept the extra operations burden.
- **Internal app sharing** is for one-off QA drops, not the main steady-state
  update path.

That recommendation is now documented on the Pages site and linked from the
Android-facing docs instead of living only in bead discussions.