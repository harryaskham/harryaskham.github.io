# Session summary — bd-5cc69c wear-track default + rollout-default + operator cadence docs

## Goal

Close the loop on tonight's first end-to-end Play push: bake the
empirical findings into `release-to-play.sh` so the next operator/agent
gets push-button rollouts without manual `--track wear:internal` overrides
or `--status rollout` flags. Land the operator-directed defaults:
"prefer internal testing releases over drafts", and "every ~10 major
android beads landed or every major mainline cacophony release" cadence.

## Bead(s)

- `bd-5cc69c` — release-to-play.sh: default wear leg to wear:internal
  track + flip status default to rollout (operator-directed).

## Before state

- `release-to-play.sh` defaulted `--track internal` for both phone and
  wear. Wear AAB upload to bare `internal` track failed with HTTP 400
  INVALID_ARGUMENT ("requires the Wear OS system feature
  android.hardware.type.watch") — fixed by manual `--track wear:internal`
  override at the second attempt.
- `release-to-play.sh` defaulted `--status draft`, requiring an
  operator-side Play Console click to actually roll out. Operator
  direction 2026-06-02: "don't leave things in draft for ages, prefer
  internal testing releases".

## After state

- `release-to-play.sh`:
  - New `WEAR_TRACK=""` default + `UPLOAD_WEAR_TRACK="${WEAR_TRACK:-wear:$TRACK}"`
    derivation at upload time. The phone leg keeps using `--track $TRACK`;
    the wear leg uses `--track $UPLOAD_WEAR_TRACK` so e.g.
    `--track internal` produces `internal` for phone and `wear:internal`
    for wear automatically.
  - New `--wear-track <name>` CLI flag for explicit asymmetric overrides
    (`wear:beta`, etc.).
  - `--status` default flipped from `draft` to `completed` (= rollout)
    per operator directive. The final status line now prints both
    `phone-track=<X> wear-track=<Y>` so the operator can read what
    actually shipped.
  - Help text expanded to document the wear: prefix rationale citing the
    HTTP 400 INVALID_ARGUMENT root cause.
- `companion/android/QA.md`:
  - "Play Store internal-testing release" section documents
    `--wear-track`, the `wear:` prefix requirement, the auto-derivation,
    and adds a verified end-to-end table for the 2026-06-02 v1.2.1050
    rollout (phone versionCode 12311 -> `internal`, wear versionCode
    1012311 -> `wear:internal`).
- `.cacophony/profiles/caco-android.md`:
  - Release runbook rewritten to lead with the new rollout-default
    invocation, document the SOPS materialize -> build -> verify ->
    Play upload -> shred pipeline step-by-step, list overrides
    (--status draft / --validate-only / --track / --wear-track /
    --skip-build / --skip-phone / --skip-wear / --release-name),
    pin the versionCode contract (phone = git commit count, wear =
    phone + 1_000_000), and record the operator-directed cadence
    ("every 10 major android beads landed or every major mainline
    cacophony release").
- New `ReleaseToPlayWearTrackSourceTest` (5 tests) source-pinning the
  WEAR_TRACK default, --wear-track override flag, derivation in the
  upload call, status-line format, QA.md + profile docs, and the
  rollout default flip with operator-quote rationale.

## Diff summary

- Code commit: pending final squash SHA from reintegration receipt.
- Files touched (4):
  - `companion/android/scripts/release-to-play.sh` (defaults +
    derivation + status line + help text).
  - `companion/android/QA.md` (Play release section).
  - `.cacophony/profiles/caco-android.md` (release runbook
    expansion).
  - `companion/android/app/src/test/java/com/cacophony/companion/ReleaseToPlayWearTrackSourceTest.kt`
    (new, 5 tests).
- Tests: +5 source-pin tests; no existing tests changed.
- Behavioural delta: next `nix develop --command
  ./scripts/release-to-play.sh` (no args) does what tonight's two
  separate calls did — builds + signs + uploads phone to `internal` AND
  wear to `wear:internal` AND immediately rolls both out to enrolled
  internal testers. No `--track wear:internal` / `--status rollout`
  needed in routine use.

## Embedded artefacts

- None this session.

## Operator-takeaway

The next release push for a major milestone is one command:
`cd companion/android && nix develop --command ./scripts/release-to-play.sh`.
That ships both phone and wear to Play internal testing immediately,
per the 2026-06-02 cadence directive ("every ~10 major beads or major
mainline cacophony release"). The wear: prefix, the rollout status,
and the form-factor track mapping are all now defaults so neither you
nor a future agent has to remember them.

bd-f55e2d (the larger operator notification + caco-choices surface to
PROMPT for a rollout) is still the natural follow-up — tonight's
release was operator-initiated; the choice surface would surface a
"v1.2.1051 is on main, promote to internal testing? (rollout / draft /
skip)" prompt automatically every N commits / major milestone.
