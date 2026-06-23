# Session summary — bd-e3cfbc

## Bead
bd-e3cfbc (P2 feature): Improve quick file bead prompt context for platform-specific details.

## Problem
The shared quick-file bead-expansion prompt (POST /beads/expand, used by Web/TUI/Android/iOS)
produced generic titles/descriptions even when the source clearly concerned a specific platform
(e.g. an Android-app issue only carried an `android-app` label, not platform text in title/desc).

## Change
crates/caco-daemon/src/llm.rs `build_expand_beads_request`:
- System prompt now instructs the LLM to extract platform/surface/component cues (Android, iOS,
  watchOS, macOS, web, TUI, daemon, beads, ...) from the draft text AND any provided context and
  reflect them explicitly in BOTH the generated title and description, instead of leaving titles
  generic and relying on labels alone.
- Provided project/surface context is reframed ("Project and surface context (... reflect it
  explicitly in the bead title and description)") so any named platform is surfaced into bead text.
- This is prompt-only and non-inert: it improves how platform cues already present in the draft
  text/context are surfaced, independent of which surface called expand.

## Tests (crates/caco-daemon/src/llm.rs)
- build_expand_request_instructs_platform_context_reflection: asserts the system prompt mentions
  platform, gives examples (Android), requires reflection in both title and description, and warns
  against relying on labels alone.
- build_expand_request_frames_surface_context_for_reflection: asserts provided surface context is
  framed for reflection in the user message.

## Validation
Queued focused test (caco test run --wait): `cargo test -p caco-daemon --lib build_expand_request`
=> 4 passed, 0 failed (the 2 new + 2 existing). Rebased onto current main (b5edc49754, msd-5's
AbandonedAndroidEmulator fix) so caco-daemon compiles. Full test-small/check/clippy run by the
reintegration gate on the merge commit.

## Acceptance criteria
- AC1 (platform in title) + AC2 (explicit platform context in description, not just labels): MET by
  the prompt change; unit-tested.
- AC3 ("Test with Android app quick file"): this is live integration validation requiring the
  Android app (out of this Linux worker's lane). The prompt change is shared/surface-agnostic, so
  the corrected behavior applies to the Android quick-file once exercised; the android lane can
  confirm end-to-end. Surfaces may additionally pass an explicit platform/surface `context`
  string for the strongest effect (caco-web currently sends none; TUI passes its dialog context).

## Diff
See the reintegration receipt for the landed squash SHA (local agent-branch commit 3d40619fda1d9d89687ffbc6092b6b91094e8ccb).
