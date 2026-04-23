# Session summary — release CLI ergonomics (bd-fca3e1, partial)

## Goal

Address a multi-part bug report on `caco release list` / `caco
release status` ergonomics: (1) operators couldn't see job age in
the text output despite the data being in the JSON, (2) sister
subcommands had inconsistent JSON envelopes, (3) `--job-id` and
`--release-id` were rejected as natural-guess aliases for `--id`,
and (4) error formatting on not-found was the legacy boxed style
instead of the unified template.

This session takes the four CLI-only / cheap fixes; the JSON
envelope inconsistency and the deeper queued-job operational
issues are flagged for follow-up but left untouched.

## Bead(s)

- `bd-fca3e1` — caco release list shows 21 jobs STUCK in 'queued'
  state with no queued_at timestamp visible … (multi-issue test-
  user sweep)

## Before state

- `caco release list` text output: 5 columns
  (state/id/channel/strategy/node). Age field not shown — operator
  could not distinguish a 5-minute-old queued job from a 12-day-
  old stuck one without dropping into `--json`.
- `caco release status` rejected `--job-id` and `--release-id`
  with the bd-b76723 unknown-flag warning + a generic "--id is
  required for release status" error.
- Not-found error was rendered as `caco release status — error\n
  Error: release job not found: bogus` (boxed, divergent from the
  unified `error: <msg> (<hint>)` template).

## After state

- Text output gains a per-row age column (compact `1h2m` /
  `6d18h` format) and a `⚠ stuck` marker on queued jobs older
  than 24h. A footer summarises the stuck count and points at
  `caco release sync`. Verified live against the cluster:
  21/21 stuck jobs flagged correctly with ages spanning
  1d6h to 12d11h.
- `RELEASE_STATUS_ARGS` declares `--job-id` and `--release-id` as
  documented aliases for `--id`. `dispatch_release_status` reads
  them with `.or_else(...)` cascade; the missing-flag error now
  mentions all three forms and points operators at
  `caco release list`.
- Not-found error now flows through the standard `CliError::new`
  path: `error: release job not found: bogus (run `caco release
  list` to see queued/active jobs)` — single-line, no boxing,
  matches the bd-b7392e family fix.
- New `humanize_age_secs` helper covers the s/m/h/d ranges with
  negative-input clamping (avoids confusing "-3s" output when
  daemon clock is slightly ahead of CLI clock).

## Diff summary

- Files touched:
  - `crates/caco-cli/src/lib.rs` — `RELEASE_STATUS_ARGS` aliases,
    `dispatch_release_status` flag cascade + standard-error path,
    `dispatch_release_list` text-output upgrades,
    `humanize_age_secs` helper, +2 unit tests
- Tests: +2 / -0 / flipped 0
- Behavioural delta: text-mode operator visibility on stuck
  release jobs; alias affordance for `--job-id` / `--release-id`;
  unified error format for release-status not-found.

## Operator-takeaway

Run `caco release list --status queued` to immediately see the
21 stuck jobs with ages and the suggested `caco release sync`
remediation. The footer count gives an at-a-glance health
signal without needing `--json | jq` ceremony.

`caco release status --job-id <id>` and `--release-id <id>` now
work. Canonical remains `--id`.

Out of scope for this session (filed as future-session items):
- **Issue 1, deeper**: stuck-queued jobs are a release-pipeline
  health issue. The CLI now surfaces them clearly, but `caco
  release sync` may itself be stuck (see bd-503735). Operator
  attention recommended.
- **Issue 2**: `release list --json` envelope (`{ok, releases,
  meta}`) vs `release config --json` (`{ok, data, meta}`)
  inconsistency — third intra-namespace mismatch. Resolution
  needs a cross-cutting envelope conformance test + a deprecation
  window for the divergent shape; too invasive for this session.
- **Pattern**: 6 surfaces now want `--id` aliases. A dispatcher-
  level flag-aliases registry would be the right fix; this
  session takes the local one-surface approach because the
  registry needs a design pass touching every dispatch arm.
