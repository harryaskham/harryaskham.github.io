# Session summary — Android pico transcript auto-scroll (bd-1fc12e)

## Goal

Continue Android pico chat UX parity with macOS. This slice makes the transcript
follow live streaming output: when the user is already at the bottom, new content
(assistant text deltas, tool runs, status) auto-scrolls into view; when the user
has scrolled up to read history, it does NOT yank them. md2-0-endorsed as my
non-overlapping pure-UI lane (transport/FFI is bd-257184).

## Bead(s)

- `bd-1fc12e` — Android pico transcript: auto-scroll to bottom when following
  live output (macOS parity). Filed + claimed + implemented + validated this
  session. Sibling of the other landed pico slices this session.

## Before state

- Failing tests: none in the pico lane.
- PicoTranscript (LazyColumn) had stable keys preventing scroll JUMP on snapshot
  replacement, but no auto-follow: streaming output appeared below the fold and
  the user had to scroll manually.

## After state

- Failing tests: none in the pico lane. `:app:testDebugUnitTest --tests
  *.PicoAgentViewSourceTest` BUILD SUCCESSFUL (1m37s), including the new
  `picoShouldAutoScrollFollowsOnlyAtBottomBd_1fc12e` unit test.
- PicoTranscript uses a rememberLazyListState; a derivedStateOf `atBottom`
  (computed via the pure `picoShouldAutoScroll`) gates a LaunchedEffect that
  animates to the last item only when the user is at/near the bottom, on each
  content change. Conservative: scrolling up to read history is preserved.

## Diff summary

- Code/content commits: one commit (bd-1fc12e); final landed squash SHA from the
  reintegration receipt.
- Files touched:
  - `companion/android/app/src/main/java/com/cacophony/companion/ui/pico/PicoAgentView.kt`
  - `companion/android/app/src/test/java/com/cacophony/companion/PicoAgentViewSourceTest.kt`
- Tests: +1 / -0 / flipped 0.
- Behavioural delta: transcript follows live output when at the bottom; no
  transport/snapshot change (pure UI).

## Validation note

Pure decision logic (`picoShouldAutoScroll`) unit-tested; the LazyListState
LaunchedEffect wiring is source-pinned + compiles clean. The actual scroll
behavior is best confirmed on an emulator with a live streaming session (deferred
under host load); the conservative at-bottom gate means the worst case is "does
not follow", never "yanks the user".

## Operator-takeaway

Seven slices this session toward a seamless, fast pico agentic experience.
Also deduped two canonical team beads I had unknowingly re-implemented
(bd-7cc19a slash-autocomplete, bd-a78a95 send-failure) after fixing a ready-queue
read bug (caco bd list JSON is data.beads, not data.items). Next high-value gap
is the model picker, blocked on md2-0 adding a select_model source method.
