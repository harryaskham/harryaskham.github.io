# Session summary — bd-fa6f8b: caco bd status surfaces draft column (sibling of bd-2e2be3 on different surface)

## Goal

Test-user filed: `caco bd status` per-project line shows
`open / in-progress / blocked / closed / ready` but
NEVER mentions `draft`. 28 cacophony drafts (31% of
inventory) invisible from routine status surface.
Total also undercounts (62 vs 90) — same shape as
bd-2e2be3 already-closed on `caco project status`.

## Bead(s)

- `bd-fa6f8b` — test-user filed. Closed.

## Before state

```
$ caco bd status --project cacophony
cacophony: 62 total — 3 open, 1 in-progress, 0 blocked,
           58 closed, 3 ready
                                              # no draft
```

## After state

```
$ caco bd status --project cacophony
cacophony: 90 total — 3 open, 1 in-progress, 0 blocked,
           58 closed, 3 ready, 28 draft
```

(Draft column rendered only when count > 0 — older
projects with 0 drafts retain the compact pre-bd-fa6f8b
shape.)

## Diff summary

- 1 file touched, +73 / −2:
  - `crates/caco-cli/src/lib.rs::format_beads_status`:
    read `p["draft"].as_u64().unwrap_or(0)` (forward-
    compat with older daemons) and append ", N draft"
    to the breakdown line when N > 0.
  - new unit test
    `format_beads_status_renders_draft_count_when_present`
    covering 3 cases:
    * project with drafts → "28 draft" rendered.
    * project with 0 drafts → no draft column.
    * older daemon payload missing `draft` field → no
      draft column (graceful zero-fill).

## Verification

- `cargo build --bin caco`: clean.
- `cargo test -p caco-cli --lib format_beads_status`:
  8 passed (including the new test).
- The daemon-side fix already landed in bd-2e2be3
  (ProjectBeadSummary.draft + total includes draft +
  serde(default) for older peers). This bead is
  purely the rendering-side complement.
- Live verification needs a daemon restart (the
  authoritative daemon on helsinki must redeploy to
  emit the new draft field). Backward-compat verified
  by code review: as_u64().unwrap_or(0) handles the
  pre-restart proxy path.

## Operator-takeaway

When a daemon-side struct field gains a counterpart
in render output, the rendering side should always
guard with `unwrap_or(0)` for forward-compat with
older daemons in the rolling-deploy window. This is
the third such guard added this session
(ProjectStatusSummary.beads_draft on caco project
status / bd-2e2be3, ProjectBeadSummary.draft on the
daemon-side, format_beads_status here).

Worth a follow-up sweep: any OTHER surface that
renders bead counts? Candidates to check:
- `caco bd stats` (bd-ee2961) — separate API, has
  its own counts shape; check whether drafts are
  represented in lifecycle metrics.
- Web UI — separate consumer of the same JSON;
  may need its own update if it doesn't already
  iterate generic keys.

The "render every category present in the JSON"
(generic) vs "render a hard-coded list of fields"
(specific) tension keeps recurring. Generic is
forward-compat by construction but loses control
over column order and conditional display ('only
when > 0'). Specific is what we have here. Either is
defensible; consistency within a surface matters more
than the choice between them.
