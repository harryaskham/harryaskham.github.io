# Session summary — bd-1c0bdd polish + broken-on-main fix-forward

## Goal

Two things in one commit window:
1. Bring the Android `Scrollback` row up to the caco-web visual
   grammar with a 3-tier coloured progress bar (cross-surface
   polish under permanent bd-1c0bdd).
2. Fix-forward the caco-tui broken-on-main from msm-5's
   bd-b69cf3 landing — 95 compile errors blocking
   `cargo test-small` for everyone.

## Bead(s)

- `bd-1c0bdd` (permanent polish) — Android Scrollback bar.
- broken-on-main fix-forward against msm-5 bd-b69cf3
  collateral (no separate bead; reported via `caco msg speak`
  earlier this loop).

## Diff summary

**Android polish (bd-1c0bdd cycle):**

`companion/android/app/src/main/java/com/cacophony/companion/ui/agents/AgentDetailScreen.kt`:
- The Scrollback `LabelValue` block now also renders a 4dp-tall
  `LinearProgressIndicator` immediately below the text when both
  `tmuxHistoryLimit` and `tmuxHistorySize` are present. 3-tier
  Nord palette colouring matching caco-web's
  `agent-scrollback-bar`:
  - `< 60%` Nord green  `#A3BE8C`
  - `< 90%` Nord yellow `#EBCB8B`
  - `≥ 90%` Nord red    `#BF616A`
- Track colour bound to `MaterialTheme.colorScheme.surfaceVariant`.
- Limit-only fallback (`size unknown`) keeps the text-only row,
  no bar.

**broken-on-main fix-forward (caco-tui test compile):**

msm-5's bd-b69cf3 added two new fields
(`tmux_history_limit: Option<u32>`, `tmux_history_size: Option<u32>`)
to `state::AgentDisplayState` and
`caco_daemon::ui_stream::AgentSnapshot` — but:

- 75 literal-initializer sites of those structs were missed
  → E0063 missing-field errors;
- 20 stray `tmux_history_*: None,` lines were incorrectly
  added to literals of `state::AttachMetadata` (12 sites in 6
  literals) and `SessionKickedModal` (8 sites in 4 literals)
  → E0560 no-such-field errors.

Net: 95 compile errors blocking `cargo test-small` cluster-wide.

Mechanical fix-forward via two passes:

1. **Removals (20 lines)**: dropped erroneous
   `tmux_history_limit: None,` / `tmux_history_size: None,`
   pairs from 6 `AttachMetadata` literals and 4
   `SessionKickedModal` literals in
   `crates/caco-tui/src/app.rs`.

2. **Additions (150 lines = 75 sites × 2)**: inserted
   `tmux_history_limit: None,` and `tmux_history_size: None,`
   right before the matching closing `}` of each missing
   literal, indent-matched to the surrounding fields. Driven
   by a Python brace-counter so the edits land at the correct
   site even in deeply nested initializers. Files touched:
   - `crates/caco-tui/src/shell_cwd.rs` (2)
   - `crates/caco-tui/src/shell_tile_lane.rs` (2)
   - `crates/caco-tui/src/state/tests.rs` (66)
   - `crates/caco-tui/src/views/agent_detail.rs` (1)
   - `crates/caco-tui/src/views/chat.rs` (1)
   - `crates/caco-tui/src/views/fuzzy_picker.rs` (1)
   - `crates/caco-tui/src/views/project_tree.rs` (2)

Struct definitions unchanged. No tests added or modified.

## Before state

- Android Scrollback row was text-only; caco-web shipped a
  coloured bar.
- `cargo test-small` failed to compile across the fleet
  (95 errors in caco-tui lib tests). Anyone running
  test-small got a wall of red.

## After state

- Android matches caco-web visual grammar end-to-end on the
  Scrollback section.
- `cargo test-small` is green again (56/56).
- `cargo test -p caco-tui --lib` is green (2818/2818).
- `cargo build -p caco-tui` clean.

## Notes / verification

- 56 test-small green, 2818 caco-tui lib green.
- All 95 original errors gone, no new diagnostics introduced.
- Brace-counter Python script handled the deeply-nested
  state-tests literals safely (some opened nested
  initializers inside the same struct — correctly skipped
  past their inner `{` / `}` via depth tracking).

## Out of scope

- Filing a bead against msm-5's bd-b69cf3 to teach future
  hands to grep for struct-literal sites when adding fields
  to widely-used structs. Worth a sentence in the next dev
  retrospective; not gating.
- TUI parity for the bar render itself — TUI uses a text-only
  presentation that's appropriate for the surface.

## Operator-takeaway

caco-tui broken-on-main is fixed; `cargo test-small` is
green again. Android Scrollback now matches caco-web's
coloured-bar treatment. msm-5's bd-b69cf3 added two new
struct fields without updating ~95 literal-init sites — a
one-shot Python pass got everyone unstuck.
