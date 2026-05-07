# Session summary — transcription tools hubs

## Goal

Add visible global and per-project Transcription hub entries to the operator frontends requested by `bd-c6ab5e`: TUI, caco-web, and Android. The intent is an observability landing page for transcript-related feed events, speech/STT history, and transcript scratchpad discovery while richer transcript feed events continue to land.

## Bead(s)

- `bd-c6ab5e` — caco tui, web, android should have Transcription section under Tools, with global and per project

## Before state

- TUI had Audio/Summaries/other Tools entries, but no global or per-project Transcription hub.
- caco-web had feed and summaries views but no first-class Transcription view with global/project scope.
- Android More/Tools exposed Scratchpad and Speech, but no Transcription subpage.
- `docs/transcription.md` described STT commands and transcript sinks but not the new cross-frontend hubs.

## After state

- TUI has `Cluster > Tools > Transcription` and `Project > Tools > Transcription`, with persistent workspace selection, breadcrumbs, shell-context resolution, and a hub view filtering cached feed events plus transcript-like scratchpads by scope.
- caco-web has a Transcription view with a project selector, transcript event count, speech/STT history count, and recent transcription-related feed entries.
- Android More/Tools has a Transcription subpage scoped by the selected project, showing transcript-related feed events and speech active/idle state.
- README and the transcription guide now mention the TUI/caco-web/Android global and per-project Transcription tools hubs; the styled HTML sibling was updated and validated.

## Diff summary

- Commits: agent-branch commit `2842641b09`; final mainline squash SHA is assigned during reintegration.
- Files touched: `crates/caco-tui/src/nav.rs`, `crates/caco-tui/src/views/transcription.rs`, `crates/caco-tui/src/app.rs`, `crates/caco-web/static/index.html`, `crates/caco-web/static/app.js`, `companion/android/app/src/main/java/com/cacophony/companion/MainActivity.kt`, `README.md`, `docs/transcription.md`, `docs/transcription.html`, plus TUI workspace/breadcrumb/nav support files.
- Tests: +1 TUI unit test for transcription feed scope filtering.
- Validation: `cargo fmt --all -- --check`; queued `tj-80bd67a2` passed `cargo test -p caco-tui bd_c6ab5e --lib`; queued `tj-3f4f1530` passed `cargo test -p caco-tui nav --lib`; `node --check crates/caco-web/static/app.js`; Android/web source assertions; `docs/validate-pages.sh` passed after `docs/sibling-update.sh transcription`.
- Behavioural delta: operators now have consistent Transcription hub entry points in the TUI, web dashboard, and Android companion without invoking provider calls from those hubs.

## Operator-takeaway

This is a thin but consistent cross-surface hub: it makes transcript observability discoverable now, and it gives future feed-mesh transcript event work a stable place to render richer live transcript rows later.
