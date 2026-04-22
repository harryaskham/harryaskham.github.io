# Session summary — bd-290559 Android scrollback meter

## Goal

Surface the per-agent tmux scrollback usage (added to the daemon
detail JSON in bd-7ef076) on the companion Android `AgentDetailScreen`
so operators can see how close a long-running agent is to the
100k-line ceiling.

## Bead(s)

- `bd-290559` — [bd-7ef076 follow-up] android: render
  `tmux_history_limit` + `tmux_history_size` in agent detail.

## Diff summary

`companion/android/app/src/main/java/com/cacophony/companion/state/Models.kt`:
- `AgentSnapshot`: two new optional fields,
  `tmuxHistoryLimit: Long?` and `tmuxHistorySize: Long?`.
- `fromJson` reads `tmux_history_limit` and `tmux_history_size`
  off the daemon detail blob, gating each on `json.has(...)` so
  pre-bd-7ef076 daemons (or list-endpoint payloads which omit
  these fields) decode cleanly to `null`. Negative or zero
  ceilings are filtered to `null` to avoid bogus 0% renders.

`companion/android/app/src/main/java/com/cacophony/companion/ui/agents/AgentDetailScreen.kt`:
- New row inserted after the existing `Updated` `LabelValue`:
  - `agent.tmuxHistoryLimit?.let { ... }` — entirely suppressed
    when the daemon didn't supply a ceiling (e.g. agent has no
    tmux session, or older daemon).
  - When both fields present: `12 345 / 100 000 lines (12%)`.
  - Limit-only fallback: `100 000 lines (size unknown)` for
    agents whose live `tmux display-message #{history_size}`
    probe failed.
- New private helper `formatLineCount(Long)` — splits at every
  three digits with a narrow no-break space (`\u202F`). No ICU
  / NumberFormat dependency; matches the typographic feel of
  the existing TUI/web counters.

## Before state

- The daemon already exposed `tmux_history_limit` /
  `tmux_history_size` on per-agent detail (bd-7ef076), but the
  companion app silently ignored them.

## After state

- Open any agent detail screen → "Scrollback" row right under
  "Updated", e.g. `Scrollback  37 412 / 100 000 lines (37%)`.
- Renders gracefully across daemon versions: missing fields →
  row hidden; live size missing → "size unknown"; agent has no
  tmux session → row hidden.

## Notes / verification

- `cargo test-small` 52/52 green (Rust side untouched apart
  from earlier merges).
- Android module has no `gradlew` in the worktree and the
  Gradle build needs the SDK + Java toolchain; verified the
  edits by structural review (additive `Long?` fields in a
  `data class` with serde-default-equivalent decode, plus an
  additive Compose `LabelValue` row inside the existing
  conditional block style). Companion bot routinely lands
  similar surface-only edits in bulk.

## Out of scope

- Bytes-used estimate. Same reasoning as bd-7ef076: noisy
  signal vs the line counter, deferred until a surface
  actually wants it.
- Showing the metrics on the agent list / grid card. Detail
  screen only.

## Operator-takeaway

Companion Android agent detail now shows `Scrollback NN / 100 000
lines (NN%)` directly under the Updated row. Pairs with
bd-7ab1b9 (caco-web, claimed by wmi-1) and bd-b69cf3 (caco-tui,
claimed by msm-5) for full surface coverage of the bd-7ef076
metrics.
