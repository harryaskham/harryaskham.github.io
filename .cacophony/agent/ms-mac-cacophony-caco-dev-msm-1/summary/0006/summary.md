# Session summary — caco choices list/current honesty fixes

## Goal

Close bd-914ceb — three closely related CLI honesty issues in `caco
choices list` and `caco choices current`, all sibling misses of the
bd-bc52ef family (silent-no-op-on-unknown-input) and the bd-b76723
family (unrecognised-flag warning + ignored).

## Bead(s)

- `bd-914ceb` — caco choices list --status accepts undocumented
  'unavailable' (works fine), and silently accepts any other bogus value
  (sibling of bd-cd2ec9 family)

## Before state

```
$ caco choices list --help | grep -- --status
  --status    Filter by status: active, resolved, all (default: all).
              ✗ omits 'unavailable' which is a populated, queryable state

$ caco choices list --status unavailable --limit 3
3 choice(s) (filter: unavailable):
  ...                                              # ✓ daemon supports it,
                                                   #   help just doesn't say so

$ caco choices list --status bogus
no choices (filter: bogus)                          # ✗ silent

$ caco choices current --project cacophony
warning: bd-b76723: caco choices current received unrecognised flag(s)
5 active choice(s):
  ... (helsinki-tendril-..., midi2hid-..., cacophony-...)
                                                    # ✗ returns whole cluster
```

## After state

```
$ caco choices list --help | grep -- --status
  --status    Filter by status: active, resolved, unavailable, all (...)

$ caco choices list --status bogus
error: unknown --status value 'bogus'. Allowed: active, resolved,
unavailable, all

$ caco choices list --status unavailable --limit 2
2 choice(s) (filter: unavailable):                  # unchanged ✓

$ caco choices current --project cacophony
2 active choice(s) (project: cacophony):
  choice-... (helsinki-cacophony-choices-general, 5 options)
  choice-... (helsinki-cacophony-choices-spec, 5 options)

$ caco choices current
5 active choice(s):                                 # unfiltered unchanged ✓
```

`cargo test-small` 57/57 PASS, `cargo clippy -p caco-cli --lib --tests`
clean.

## Diff summary

- Commit: 74312197
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: +1 (`choices_list_status_validator_includes_unavailable_and_rejects_bogus`)
- Behavioural delta:
  1. `--status` validator added to `dispatch_choices_list` rejecting
     anything outside `[active, resolved, unavailable, all]`.
  2. `CHOICES_LIST_ARGS --status` summary now mentions `unavailable`.
  3. `CHOICES_CURRENT_ARGS` introduced with `--project`; `current` is
     promoted from `mcp_leaf` to a full `CommandSpec` with args.
  4. `dispatch_choices_current` accepts `Option<&str>` project and
     applies a client-side filter against the `project` field of each
     active choice, recomputing the displayed count and JSON
     `data.count` to match.

## Operator-takeaway

The `--project` filter on `current` is client-side (filter the choices
array after the round-trip) rather than a daemon API addition, since
extending `GET /api/v1/choices` with a Query<{project}> would have
been a separate coordination step. If/when a future bead wants to push
the filter server-side (for very large clusters), the migration is one
commit: add `Query<CurrentChoicesQuery>` to `handle_current_choices`
and short-circuit the client-side filter when the daemon responds with
`server_filtered=true`.

Help-text-as-discovery: validator error messages enumerate the
allowed-list precisely so an operator who guesses the wrong filter
sees the full set without needing to read source. This pattern keeps
paying off — file follow-ups every time a sibling-miss surfaces.
