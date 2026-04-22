# Session summary 0038 — bd-8b59a1: caco msg inbox --mute (slice 1)

## Goal

Give non-dev persistents and operators a way to suppress routine
broadcasts they don't care about (e.g. the 14+ `caco-dev-* notes`
operator nudges per session observed tonight).

## Bead(s)

- `bd-8b59a1` slice 1 — CLI flag + profile schema field.

## Before state

- All persistents received every broadcast; no opt-out
  mechanism. Each irrelevant broadcast cost an
  acknowledgement cycle.

## After state

- `caco msg inbox --mute <pattern[,pattern...]>` drops messages
  whose body contains any pattern (case-insensitive substring).
  Operator-runnable today.
- `ProfileComms.mute_broadcasts: Vec<String>` frontmatter field
  added (with `#[serde(default, skip_serializing_if =
  "Vec::is_empty")]`). Profile authors can declare a stable
  opt-out for their persistents.
- `apply_inbox_client_filters` extended with `mute_patterns: &[String]`
  parameter; existing 4 unit tests updated to pass `&[]`.
- compose.rs `ProfileComms { scope }` literal updated to
  include the new field.

## Diff summary

- Commit: `d7fc92ef`.
- Files (3): caco-cli lib.rs, caco-profile model.rs + compose.rs.
- `cargo build` and `cargo clippy` for caco-cli + caco-profile +
  caco-daemon: clean. caco-cli inbox-filter tests pass.

## Operator-takeaway

Persistents (or operators) can now run e.g.
`caco msg inbox --mute 'caco-dev-* notes'`
to suppress routine broadcasts. The profile-frontmatter
`comms.mute_broadcasts: [...]` field is plumbed through but
auto-applied opt-out at CLI invocation time (caller-id → profile
resolution) is bd-8b59a1 slice 2.

Trade-off honoured: filtering is opt-in by recipient, never
enforced by sender. Broadcasts remain promiscuous on the wire so
the operator can still reach everyone with a non-mutable message.
