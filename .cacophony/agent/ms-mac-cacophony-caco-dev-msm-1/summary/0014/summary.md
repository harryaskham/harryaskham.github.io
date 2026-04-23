# Session 0014 — bd-61e600: TUI quick-file bead popup

## Goal
Implement the webapp's quick-file bead popup in the TUI, bound to
bare `b`; move the full bead-create dialog to `Shift+B`.

## Decisions
- **Two-field minimal popup**: Project picker + freeform Text. Mirrors
  the webapp's `showQuickBeadModal()` (app.js ~2754).
- **Wire to existing `expand_beads` endpoint** (`/api/v1/projects/<p>/beads/expand`)
  rather than the structured create-bead path. The daemon LLM already
  structures freeform text into one or more beads and persists them.
- **Ephemeral state**: no draft restoration. Successful submit closes;
  Esc closes without persistence. Failure re-enables for retry.
- **Key extraction refactor**: Pulled the legacy bare-`b` handler body
  (bd-aa6b1c, bd-249b5c, bd-687e93, bd-f53223, bd-a02f7f, bd-351b11)
  into `open_full_bead_create_dialog()` so `b`/`B` paths are clean.

## Code
- `crates/caco-tui/src/state/mod.rs`
  - `QuickFileBeadDialog` struct + `QuickFileBeadField { Project, Text }`
  - `quick_file_bead_dialog: Option<QuickFileBeadDialog>` on AppState
- `crates/caco-tui/src/app.rs`
  - `open_quick_file_bead_dialog()` helper
  - `open_full_bead_create_dialog()` extracted helper (formerly bare-b body)
  - `submit_quick_file_bead()` -> client.expand_beads via tokio::spawn
  - `handle_quick_file_bead_dialog_key()` Esc/Enter/Tab/typing/arrows
  - `render_quick_file_bead_overlay()` 70x17 centered modal
  - Wired `KeyCode::Char('b')` -> quick-file, `Char('B')` -> full
  - Close/retry hooks in BeadExpandSucceeded / BeadExpandFailed
  - 7 new tests (open, no-projects, esc, typing, tab, arrow, empty-submit)
  - 3 pre-existing bead_create_* tests updated to use Shift+B

## Tests
- `cargo test -p caco-tui --lib`: 2870 passed, 0 failed
- `cargo test-small`: 146 passed
- `cargo clippy -p caco-tui --lib --no-deps`: clean (no new warnings)

## Operator Acceptance Criteria
- [x] Quick-file popup opens on `b` press
- [x] Shows minimal form for quick file bead creation (project + text)
- [x] Regular beads modal accessible via Shift+B
- [x] Esc closes the quick-file popup

## Constraints Honored
- No docker builds (pure Rust TUI work)
- Merge-queue mixin: test-small + targeted + clippy
- Speaking claim/close via `caco msg speak`
