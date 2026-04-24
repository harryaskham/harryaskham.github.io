# Session summary — bd-18aa72 reimport_repairs surfacing in caco bd sync

## Goal

Surface the `reimport_repairs` counter (bd-408b35 / bd-f86e8a) in
`caco bd sync` human-readable output so an operator running sync sees
when the lenient `IndexMode::ReimportTruncate` path silently fixed up
historical journal mutations.

## Bead(s)

- `bd-18aa72` — Operator-visible counter: 'reimport repairs in last
  24h' across TUI / web / doctor (this slice covers the CLI surface;
  doctor-sensor and reconciler-footer slices remain as called out in
  msm-3's deferral note)

## Before state

- Wire side: `ReconcileResult.reimport_repairs` (bd-408b35) was
  already plumbed and tested in `crates/caco-beads/src/store.rs`
  with `skip_serializing_if = ReimportRepairCounts::is_zero`.
- CLI side: `dispatch_bd_sync` text renderer in
  `crates/caco-cli/src/lib.rs` rendered only `imported / exported /
  skipped / pulled / pushed` and silently dropped
  `reimport_repairs`. Operators running `caco bd sync` saw nothing
  about journal repairs even though daemon stderr was eprintln'ing
  the bd-f86e8a line per repair.
- Side issue: `caco-profile` invariant test
  `shipped_profiles_html_lists_every_canonical_profile` was failing
  pre-existingly because `.cacophony/profiles/stale-check.md` shipped
  but `docs/profiles.html` was missing the row.

## After state

- `caco bd sync` text renderer appends a single line when the wire
  envelope carries a non-zero `reimport_repairs` block; matches the
  JSON shape (omitted entirely on a clean journal).
- `docs/profiles.html` carries a `stale-check` row; the invariant
  test passes.

## Diff summary

- 2 files modified, 24 insertions:
  - `crates/caco-cli/src/lib.rs`: text renderer in `dispatch_bd_sync`
    appends a `reimport-repairs:` line when the wire-side struct is
    present (matches `skip_serializing_if = is_zero` so a clean
    journal stays silent).
  - `docs/profiles.html`: pre-existing failure unblocked — added a
    row for the `stale-check` mixin that's already shipped under
    `.cacophony/profiles/stale-check.md`. Required by the
    `shipped_profiles_html_lists_every_canonical_profile` invariant
    test in `caco-profile`.

Output shape on a dirty reconcile:

```
sync complete for cacophony
  imported:  3
  exported:  0
  skipped:   42
  pulled:    true
  pushed:    false
  reimport-repairs: 7 (empty-title: 5, oversized-title: 2) [bd-f86e8a / bd-18aa72]
```

A clean journal omits the line entirely (mirrors the JSON wire shape).

## Tests

- `cargo test-small`: 271 + 109 + 760 + 1 + 298 + 18 + 15 + 120 = all
  green.
- `caco-profile shipped_profiles_html_lists_every_canonical_profile`:
  green after `stale-check` row addition.
- One flaky test in `caco-tui` (`playback::tests::write_to_pipe_broken_pipe_detected`)
  failed in the full suite but passed in isolation — pre-existing,
  unrelated to this change.

## Out of scope

- Doctor sensor that warns on non-zero reimport_repairs in last 24h
  (requires rolling counter persistence across daemon restarts) —
  child slice 2 of msm-3's split.
- Reconciler commit-message footer — child slice 3.
- Web / TUI surfacing — separate beads / per-agent ruleset.

## Operator-takeaway

`caco bd sync` is now honest about journal repairs that previously
only landed in daemon stderr. If an upstream producer is generating
invalid mutations, you'll see the count grow on every sync and can
file a bead against the producer instead of letting placeholder
strings quietly populate the searchable index forever.

Side fix: `docs/profiles.html` was already drifted from
`.cacophony/profiles/stale-check.md`; the `caco-profile` invariant
test caught it on this commit. Docs row added so the next agent
doesn't waste a cycle on the same red light.
