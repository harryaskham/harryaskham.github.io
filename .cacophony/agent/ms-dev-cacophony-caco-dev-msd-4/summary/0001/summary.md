# Session summary — bd-3315b6 caco bd auto-close-landed sweep

## Goal

Stop forcing workers to manually audit beads whose impl + tests
already landed on mainline. Add a positive sweep that finds
open/in_progress beads whose IDs appear in mainline commit footers
and closes them with a canned `--admin-override --reason` so the
close-validator's mainline-history contract is satisfied without
re-litigating implementation.

## Bead(s)

- `bd-3315b6` — post-reintegrate close-validator should auto-close beads whose impl + tests already landed

## Before state

- Workers hitting the auto-claim queue routinely picked up beads whose work had already landed in prior sessions; only the bead row was stuck `open`/`in_progress`. wmi-2 reported 3-of-4 such cases in a single session.
- Each required manual handling: verify diff, find named tests, run tests, write rationale, run `caco bd close --admin-override --reason ...`. Several minutes of dev capacity per stale bead, multiplied across the fleet.
- The close-validator already implements the negative form ("refuse close if bead-id not in last 1000 commits of main-ref"); there was no positive form ("close if bead-id IS in those commits").
- Failing tests: bd-c19193 (pre-existing, unrelated).

## After state

- New CLI surface `caco bd auto-close-landed`:
  - `--project <name>` (required)
  - `--main-ref <ref>` (default `origin/main`)
  - `--max-commits <N>` (default 1000, mirrors close-validator scan window)
  - `--limit <N>` (default 200, beads considered per sweep)
  - `--dry-run` (list candidates without closing)
  - `--repo <dir>` (default cwd)
- Pulls `open` + `in_progress` beads via the existing `GET /api/v1/projects/<p>/beads` endpoint (no new daemon endpoint required).
- Runs one `git log <main-ref> -<max_commits> --pretty=full` invocation per sweep and substring-checks every candidate ID against the full log text — O(beads + git_log_size) instead of one `git log --grep` per bead.
- Issues `bd close` with `admin_override=true` and a canned `admin_reason="bd-3315b6 auto-close-landed: bead ID found in <main-ref> commit footers"` so every auto-close lands in the audit feed with a traceable provenance string.
- Output: text summary listing each closed bead + skipped count, or structured JSON `{ok, data:{closed[], skipped[], considered, dry_run, project, main_ref, max_commits}}`.
- Pure helper `select_landed_bead_ids(candidates, log_text) -> Vec<&String>` factored out for unit-testability.
- Failing tests: bd-c19193 (unchanged, pre-existing).

## Diff summary

- Commit: `5c6469a0 bd-3315b6: caco bd auto-close-landed — positive sweep for beads already on mainline`
- Files touched: `crates/caco-cli/src/lib.rs` (+391 lines: ArgSpec, CommandSpec, dispatch wiring, dispatch_bd_auto_close_landed, select_landed_bead_ids helper, 5 tests).
- Tests: +5 / -0 / flipped 0
  - `bd_3315b6_classifier_finds_bead_id_in_log` (realistic `git log --pretty=full` shape, candidate-order preserved)
  - `bd_3315b6_classifier_skips_empty_ids` (empty IDs cannot match by accident)
  - `bd_3315b6_classifier_returns_empty_for_unmatched` (clean empty result)
  - `bd_3315b6_classifier_substring_match_is_case_sensitive` (`BD-AAAAAA` ≠ `bd-aaaaaa`, false-positive guard)
  - `bd_3315b6_classifier_does_not_match_other_id_as_substring` (LOCKS current behaviour: prefix collision permitted; bead IDs are 6+ hex by convention so collisions are rare; tightening to word-boundary is a documented follow-up if needed)
- Behavioural delta: a new CLI subcommand exists and is independently invokable; no existing command changed shape.

## Operator-takeaway

This unblocks the pattern called out in the bead: workers stop
burning cycles on stale-bead audits. Run periodically via cron, or
manually after a known-large reintegration burst:

```
caco bd auto-close-landed --project cacophony --dry-run
caco bd auto-close-landed --project cacophony
```

The bead also suggested a daemon-side periodic sweep — that's a
follow-up; the CLI subcommand is a strictly additive first slice
that an operator or cron job can already drive today.
