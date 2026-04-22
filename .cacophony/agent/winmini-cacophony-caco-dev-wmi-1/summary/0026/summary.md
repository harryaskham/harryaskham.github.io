# Session summary — merge-queue mixin: cargo test --lib --workspace gate (bd-29bf2b)

## Goal

12+ broken-on-main waves this session from struct-field
additions that pass `cargo check --workspace --tests` but
break test fixtures (E0063/E0062). The existing gate
(`cargo check`) compiles but doesn't run tests — so fixture
sites with struct literals are never exercised.

## Bead(s)

- `bd-29bf2b` — [merge-queue mixin] add `cargo test --lib
  --workspace` to broken-on-main detection (P2 feature)

## Before state

- `cacophony-fast-tests` mixin's `check_command` was
  `cargo check --workspace --tests` — compiles test targets
  but doesn't run them.
- Struct-field additions (e.g. `on_revival`, `parent_bead_id`,
  `tmux_history_*`) pass the gate but break every peer's
  next build.

## After state

- `check_command` changed to `cargo test --lib --workspace`
  (~30s incremental on warm cache).
- `merge-queue.md` allowed-commands list updated to include
  `cargo test --lib --workspace` with rationale.
- Profile description + env-override docs updated.

## Diff summary

- Files touched (+5 / −3):
  - `.cacophony/profiles/cacophony-fast-tests.md`: frontmatter
    `check_command` + description.
  - `.cacophony/profiles/merge-queue.md`: allowed commands.

## Verification

- Config-only change; no Rust code touched.
- `cargo test --lib --workspace` runs clean on current main.

## Operator-takeaway

After this lands, every agent composing `cacophony-fast-tests`
will run `cargo test --lib --workspace` as part of the
pre-reintegration gate. This catches the exact class of
broken-on-main that produced 12 waves this session (struct
literal fixtures in `--lib` tests that `cargo check` skips).

Cost: ~30s incremental on warm cache (vs ~0s for check).
Benefit: eliminates the entire broken-on-main category that
consumed ~2h of peer repair time this session.
