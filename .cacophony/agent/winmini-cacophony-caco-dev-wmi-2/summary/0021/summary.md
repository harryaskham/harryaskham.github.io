# Session summary — bd-81c30e: cron show/list drift cleanup

## Goal

Burn down the fresh test-user bead on `caco cron list/show`.
The reported drifts were twofold: (1) `cron show --json`
emitted `{ok,data}` while its sibling `cron list --json`
emitted the gold-standard `{ok,data,meta}` envelope; and
(2) `cron list` had a misleading empty-state story around
filters / typoed queries. The operator-facing goal was to
make the cron read surface less surprising without turning
it into a larger read-vs-write unknown-flag policy change.

## Bead(s)

- `bd-81c30e` — `caco cron list/show ... drifts: cron show --json missing meta; cron list flag-consumption data-loss / misleading empty output`

## Before state

Live behaviour before the fix:

```text
$ caco cron show --name speaking-clock --json
{ "ok": true, "data": { ... } }
# missing meta field

$ caco cron list --name definitely-not-a-cron
no crons configured
# misleading: there ARE configured crons, the filter just matched none
```

Investigation note: the bead description also claimed
`--project/--status/--limit` produced an empty table after
the bd-b76723 warning. I re-ran those paths against the
current config and could not reproduce that part — they warn
and still show the full cron table. So the concrete,
reproducible UX bug in this area was the misleading empty
state for `--name` misses, plus the `show --json` envelope
drift.

## After state

```text
$ caco cron show --name speaking-clock --json | jq '{ok, has_meta: has("meta"), data_name: .data.name}'
{
  "ok": true,
  "has_meta": true,
  "data_name": "speaking-clock"
}

$ caco cron list --name definitely-not-a-cron
no crons match --name 'definitely-not-a-cron' (3 configured)
```

Validation:

- `cargo clippy --workspace --all-targets -- -D warnings` — clean
- `cargo test -p caco-cli --lib cron_show_json_includes_meta_envelope` — pass
- `cargo test -p caco-cli --lib cron_list_name_miss_reports_filter_not_global_absence` — pass
- `cargo test-small` — 183 passed, 0 failed

## Diff summary

Files touched:

- `crates/caco-cli/src/lib.rs`
- `.cacophony/agent/winmini-cacophony-caco-dev-wmi-2/summary/0021/summary.md`

Behavioural changes:

1. **Envelope convergence**
   - `dispatch_cron_show(..., json_requested=true, ...)` now emits a
     `meta` object so `cron show --json` matches the local
     `{ok,data,meta}` family shape already used by `cron list --json`.

2. **Less misleading empty state for filtered cron lists**
   - `dispatch_cron_list` now distinguishes:
     - no crons configured at all
     - configured crons exist, but `--name` matched none
   - The second case now reports
     `no crons match --name '<filter>' (<n> configured)`
     instead of the inaccurate `no crons configured`.

3. **Regression tests added**
   - Added a tiny temp-config helper and two caco-cli unit tests:
     - `cron_show_json_includes_meta_envelope`
     - `cron_list_name_miss_reports_filter_not_global_absence`
   - Both run on `with_big_stack(...)` because serializing a full
     config fixture in debug can otherwise overflow the default test
     thread stack (same pattern already used elsewhere in caco-cli).

Commits:

- `bd-81c30e: cron show/list drift cleanup`

## Operator-takeaway

This was a small but worthwhile surface-polish bead: the cron
namespace now has internal JSON-envelope consistency, and the
most misleading empty-state wording is gone. The scarier part of
the original report — unknown flags causing silent empty tables —
did not reproduce on current main, so I did **not** broaden the
unknown-flag policy for read commands here. If that behaviour is
seen again, it should get a focused reproduction bead with exact
argv + stdout/stderr so we can fix the real path rather than
guessing.