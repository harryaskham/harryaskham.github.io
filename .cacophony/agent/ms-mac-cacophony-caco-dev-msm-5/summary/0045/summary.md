# Session summary 0045 — bd-b7e47e: bead_attachments schema (slice 1)

## Goal

Structured storage for bead-attached artefacts (screenshots,
screen recordings, files, links) so bd-d7fb98's text-only
attachment markdown can graduate to a real table with retention.

## Bead(s)

- `bd-b7e47e` slice 1 — schema + CRUD only.

## Before state

- bd-d7fb98 slice 1 records attachment paths inside the bead
  description as a markdown bullet list. No mime/size/captured_by
  metadata, no janitor-friendly retention.

## After state

- New `bead_attachments` SQL table:
  `id, issue_id, kind, path, url, mime_type, size_bytes,
   captured_by, captured_at, description`.
- Indexes on `issue_id` and `captured_at`.
- Idempotent `migrate_add_bead_attachments` for legacy DBs.
- `caco_beads::model::BeadAttachment` struct with serde defaults.
- `BeadsStore::insert_bead_attachment`,
  `list_bead_attachments(issue_id)` newest-first,
  `prune_bead_attachments_older_than(cutoff) -> count` retention
  helper (janitor wiring is slice 2).

## Diff summary

- Commit: `9d8e81d1`.
- Files (2): caco-beads model.rs + store.rs (+170 lines).
- 212 caco-beads tests pass; downstream caco-cli/caco-daemon
  build; clippy clean.

## Operator-takeaway

Slice 1 lands the persistence floor. Slice 2 (filed under
bd-b7e47e too): `caco surface capture` command, `test-user`
profile `capture_artifacts: true` frontmatter, `bd create`
auto-capture when frontmatter set, inline rendering in
bd info / web / TUI, and janitor schedule that calls
`prune_bead_attachments_older_than`.
