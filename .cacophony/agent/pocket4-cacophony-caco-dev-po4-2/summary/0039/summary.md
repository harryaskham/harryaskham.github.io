# Session summary — board-visibility preflight no longer blocks fresh closeout just because sync pulses are active

## Goal

Unblock the real control-plane issue discovered while trying to close already-landed beads. The newly filed bead `bd-8869e9` tracked that the CLI board-visibility preflight was over-blocking claim/close when the authoritative board was otherwise fresh/current but happened to be inside a brief background sync pulse.

## Bead(s)

- `bd-8869e9` — [control-plane] board visibility preflight over-blocks fresh active-primary closeout
- directly unblocked during this session:
  - `bd-c040e9` — [profile-audit] validation guidance contradicts project queue policy

## Before state

- `bd-c040e9` was already landed on `main`, but repeated `caco bd close` attempts kept failing.
- The authoritative board on `ms-mac` frequently reported:
  - `sync_status=fresh`
  - `ahead=0`
  - `behind=0`
  - brief `sync_in_progress=true` pull pulses
- `crates/caco-cli/src/lib.rs::bd_board_status_allows_mutation(...)` treated **any** `sync_in_progress=true` as a hard preflight blocker, even when the board was otherwise fully fresh.
- The actual beads mutation path already had proper `sync_in_progress` handling in the daemon/beads layer, so the CLI preflight was stricter than the real mutation safety model.

## After state

- Filed and claimed `bd-8869e9` for the control-plane bug.
- Updated `crates/caco-cli/src/lib.rs::bd_board_status_allows_mutation(...)` so fresh/current board views with `ahead=0` and `behind=0` are no longer rejected solely because `sync_in_progress=true`.
- Added/updated targeted tests in `crates/caco-cli/src/lib.rs`:
  - `bd_board_status_allows_mutation_accepts_fresh_non_primary_view_during_sync_bd_8869e9`
  - `bd_board_status_allows_mutation_accepts_fresh_active_primary_during_sync_bd_8869e9`
- Rebuilt the actual `caco` binary (not just `caco-cli`) from this checkout.
- Used the patched binary to close previously stranded `bd-c040e9`, proving the fix directly against the real operational symptom.

## Diff summary

- Files touched:
  - `crates/caco-cli/src/lib.rs`
- Validation (queued, first-party):
  - `caco test run --wait true --command "cargo test -p caco-cli bd_board_status_allows_mutation_accepts_fresh_non_primary_view_during_sync_bd_8869e9 -- --nocapture" --cwd "$PWD"`
  - `caco test run --wait true --command "cargo test -p caco-cli bd_board_status_allows_mutation_accepts_fresh_active_primary_during_sync_bd_8869e9 -- --nocapture" --cwd "$PWD"`
  - `caco build run --wait true --command "cargo build -p caco-cli" --cwd "$PWD"`
  - `caco build run --wait true --command "cargo build -p caco" --cwd "$PWD"`
- Behavioural delta:
  - bead-mutation preflight now blocks only genuinely stale/diverged board views, not fresh board views that merely have a transient sync pulse in progress
  - the real mutation path remains the source of truth for lock-busy `sync_in_progress` responses

## Operator-takeaway

This was the exact control-plane bug behind the earlier “landed but can’t close” churn. The repo now matches operational reality better: if the board is fresh/current and not ahead/behind, the CLI no longer treats a background sync pulse as proof that mutation must be impossible. That let the previously stuck `bd-c040e9` close immediately once the correct binary was rebuilt.
