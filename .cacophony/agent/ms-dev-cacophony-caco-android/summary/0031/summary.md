# Session summary — caco-android profile gradle assembleRelease reintegration gate

## Why this skips bead-first discipline

Same outage as previous tick: `caco bd create` / `caco bd list`
still rejecting cacophony-project queries with "beads proxy to
the active primary is temporarily unavailable" (helsinki :12100
timeout). Operator broadcast earlier this run said "assume you
are unblocked and continue progressing things, set up /loops as
needed and work your assigned tasks" — I'm proceeding without a
bead anchor on the strength of that. Retroactive bead will be
filed when the bd endpoint recovers.

## Goal

Close the loop on the broken-on-main symptom from the previous
slice burst. Source-pin tests in
`companion/android/app/src/test/java/com/cacophony/companion/`
are pure JVM file-readers — they read kt files as text and grep
for marker strings. They do NOT compile against the Android SDK.
So six real compile errors in MainActivity / BeadDetailScreen /
ChatSidebar / SettingsScreen / TermuxAgentTerminal /
WatchAgentDetailScreen / WatchBeadDetailScreen all landed on
main unnoticed through the bd-5d55e5/fdd6a2/d8e11f/0c1fc1/
a8d39a/ae30e2/6fc691 / 83c33d slice burst. The broken-on-main
was caught only when `release-to-play.sh` ran
`gradle :app:assembleRelease` ahead of the Play push attempt,
and the hot-fix landed as the previous slice.

This slice closes the loop: a `before_reintegration` hook on
the `caco-android` profile runs
`gradle :app:assembleRelease :wearable:assembleRelease` under
`nix develop` so a future caco-android slice with import / scope
/ type errors is blocked AT the agent's reintegrate step instead
of landing on main and surfacing hours later.

## After state

- `plugins/caco-agent/agents/android-build-gate.sh` (new,
  executable): bounded pre-reintegration gate. Mirrors the
  fast-test-gate.sh queue-backed pattern (bd-d981f9) — routes
  through `caco build run --wait --command "..."` when the
  daemon build queue is reachable so the build is CPU-bounded
  on shared hosts, falls back to inline
  `nix develop --command bash -c "gradle :app:assembleRelease
  :wearable:assembleRelease"` only when the queue is
  unreachable. Cheap routing — checks the diff against
  `origin/main` and skips entirely when the agent branch did
  not touch `companion/android/`, so Pi / docs / Rust-only
  slices don't pay an Android build tax. Honors the same
  `CACO_REINTEGRATION_ABORT_ON_FAILURE` /
  `CACO_REINTEGRATION_GATE_QUEUE` /
  `CACO_REINTEGRATION_GATE_TIMEOUT_SECS` env knobs as
  fast-test-gate.
- `.cacophony/profiles/caco-android.md`: new `hooks:` block in
  the YAML frontmatter declaring
  `before_reintegration: [{ type: command, command:
  plugins/caco-agent/agents/android-build-gate.sh, timeout:
  1800 }]`. Comment in the profile explains the broken-on-main
  background. hook_mixins / reintegration mode / lifecycle
  intentionally unchanged.

## Operator-takeaway

The next caco-android agent reintegration will run
`gradle :app:assembleRelease :wearable:assembleRelease` under
`nix develop` before the merge is allowed. Pre-flight skips
when the branch did not touch `companion/android`, routes
through the daemon build queue when reachable for CPU-bound
shared-host safety, and blocks reintegration with a clear
"BLOCKED — gradle assembleRelease failed" message when the
build fails.

The retroactive bead (when bd recovers) will document this hook
and the bd-5d55e5..8c33d / hot-fix evidence chain that motivated
it. If the hook proves too slow on operator hosts the
`CACO_ANDROID_BUILD_GATE_INLINE_TIMEOUT` env (default 900s)
can be tuned or the hook disabled entirely with
`CACO_REINTEGRATION_ABORT_ON_FAILURE=false` for a one-off
emergency reintegration.
