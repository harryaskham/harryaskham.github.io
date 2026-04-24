# Session summary — bd-491508 beads list description projection

## Goal

Apply the bd-eb1b56 metadata-only-by-default projection pattern to
`/api/v1/projects/<p>/beads` — the highest-impact RED finding from
the bd-c97e14 audit.

## Bead(s)

- `bd-491508` — Apply bd-eb1b56 projection pattern to
  `/api/v1/projects/<p>/beads` (omit description by default; surface
  description_len + ?include_description=true)

## Before state

- `BeadView { #[serde(flatten)] bead: Bead }` always shipped the full
  `description: String` for every bead in every list response.
- `caco bd list --json` returned multi-paragraph descriptions × 200+
  beads even though text-mode rendering never used the field.
- bd-c97e14 audit catalogued this as the largest list-payload
  regression on the daemon.

## After state

- Daemon `BeadListQuery` accepts `?include_description=true` (full
  body, back-compat) and `?description_preview=N` (UTF-8-safe
  truncation). Default is `Omit` — body cleared, `description_len`
  always present, `description_truncated` marker when truncated.
- New `DescriptionProjection` enum + `bead_view_from_parts_with_projection`
  + `bead_views_from_beads_with_projection` carry the mode through
  both `handle_list_beads` paths (ready=true and main filter branch).
- CLI `BD_LIST_ARGS` gains `--include-description` and
  `--description-preview` operator opt-ins.
- CLI auto-injects `?include_description=true` when `--grep` is set
  so the client-side `apply_grep_filter` (matches against
  id+title+description) keeps working transparently.
- Non-list call sites (assigned_beads etc.) keep `Full` body via
  back-compat thin-wrapper functions.

## Diff summary

- 2 files modified, 247 insertions, 4 deletions:
  - `crates/caco-daemon/src/beads.rs`: enum, helpers, struct field
    additions, handler plumbing, 2 new tests.
  - `crates/caco-cli/src/lib.rs`: 2 new ArgSpec entries, dispatcher
    plumbing, --grep auto-injection.
- Tests: cargo test-small all green.

## Out of scope (RED findings deferred to follow-ups)

- `/api/v1/messages/chat` body projection (bd-c97e14 audit RED #2)
- `/api/v1/outbox` payload projection (bd-c97e14 audit RED #3)

Both can be filed as separate beads using bd-491508 and bd-eb1b56 as
the reference impls.

## Operator-takeaway

Largest list-endpoint payload regression closed. JSON consumers that
need the full body opt in via `--include-description` (CLI) or
`?include_description=true` (HTTP). Operators using `caco bd list`
text mode see no difference. Operators using `caco bd list --grep`
see no difference (CLI auto-injects). Daemon now matches the
scratch-list (bd-eb1b56) and events (bd-0b47a7) projection contract.
