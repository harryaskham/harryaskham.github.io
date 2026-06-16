# Session summary — Fix Android pico "Waiting for pico snapshot" hang (bd-636fd1)

## Goal

Fix the Android pico view hanging on "Waiting for pico snapshot…" instead of
showing the agent. After the websocket connects (state Idle), the host's backfill
snapshot frame was being dropped by the client parser, so `latestSnapshot` stayed
null and the view never advanced.

## Bead(s)

- `bd-636fd1` — Fix Pico Android stuck at 'waiting for pico snapshot' screen (P1 bug)
- Related lineage: `bd-bcc919` (pico ws robustness), `bd-4c9275` (daemon /session fix)

## Before state

- Failing tests: none.
- Root cause: the picophony host serializes its backfill snapshot as a FLATTENED
  frame `{"kind":"snapshot","transcript":[...],...}` (transcript at the root —
  confirmed in `crates/caco-picophony/src/view.rs` test asserting the wire shape
  `{"kind":"snapshot","transcript":[{"User":...},{"Assistant":...}]`). But the
  Android `PicoSessionProtocol.snapshotFromHostFrame` `kind:snapshot` branch only
  looked for a NESTED `data`/`snapshot` object (`root.optJSONObject("data") ?:
  root.optJSONObject("snapshot")`), found none, and returned null — dropping every
  snapshot. The generic daemon tmux-pane bridge `{"type":"snapshot","content":
  "<pane text>"}` (`crates/caco-daemon/src/pty_stream.rs`) was also unhandled.

## After state

- Failing tests: none. `PicoSessionClientSourceTest` 9/9 green (added 3 bd-636fd1
  cases); full `:app:testDebugUnitTest` build SUCCESSFUL in the Android Nix devshell.
- `snapshotFromHostFrame` now falls back to `root` for a flattened `kind:snapshot`
  frame (`... ?: root`), so the real picophony backfill snapshot parses and the
  view renders. Defensive `type:snapshot` + `content` handling maps the tmux-pane
  capture into a single Note transcript item (new `picoSnapshotFromPaneContent`),
  with an empty pane returning null so the caller keeps waiting for real output.

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `ui/pico/PicoSessionClient.kt` — `kind:snapshot` `?: root` fallback +
    `type:snapshot/content` -> Note via `picoSnapshotFromPaneContent`.
  - test `PicoSessionClientSourceTest.kt` — +3 tests (flattened kind:snapshot,
    pane-content -> Note, empty pane -> null); import `PicoTranscriptItem`.
- Tests: +3, -0, flipped 0.
- Behavioural delta: pico backfill snapshots now render instead of hanging on
  "Waiting for pico snapshot…".

## Embedded artefacts

- None. The fix is a wire-format parser correction validated by deterministic JVM
  unit tests against the exact frame shapes the daemon/picophony host emit (cited
  from the Rust serialization tests); no emulator AVD is provisioned on this node
  and a static screenshot cannot exercise a live snapshot stream.

## Operator-takeaway

This was a cross-surface wire-format mismatch hiding behind a UI symptom: the
Android client assumed a nested `{kind:snapshot,data:{...}}` envelope, but the
canonical picophony host flattens the AgentView fields at the root. Pinning the
parser against the Rust serializer's actual output (and the alternate tmux-pane
bridge shape) is the durable guard. This is exactly the API-boundary contract gap
that unit tests on one side miss — the kind of thing that would also be caught by
a shared cross-surface snapshot-frame fixture.
