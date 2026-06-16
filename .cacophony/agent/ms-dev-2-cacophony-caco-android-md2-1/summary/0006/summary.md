# Session summary — Separate watch profile (host/token/port) (bd-a120bd)

## Goal

Let the Android app publish a SEPARATE daemon profile to the paired Wear OS
watch — its own host/port/token — instead of always relaying the phone's own
config, because the watch may not be on the phone's tailnet and might need a
funnel/relay address.

## Bead(s)

- `bd-a120bd` — Add watch profile with separate host/token/port configuration (P2)

## Before state

- Failing tests: none.
- The phone always pushed its own daemon config to the watch:
  `wearRelay.publishDaemonProfile(cfg.host, cfg.port, cfg.token)` at all 4
  MainActivity relay sites and the Settings "Push node token to watch" button.
  There was no way to point the watch at a different host/port/token.

## After state

- Failing tests: none. New `WatchProfileTest` 4/4 green; full
  `:app:testDebugUnitTest` build SUCCESSFUL.
- New `connection/WatchProfile.kt`: `WatchProfile(enabled, host, port, token)`
  model + SharedPreferences load/save + a pure `resolveWatchDaemonTarget(...)`
  that returns the watch profile's host/port/token when it is enabled AND has a
  usable host + token, else the phone's config.
- MainActivity now publishes the resolved target via a `publishWatchProfileToWatch`
  helper at all 4 relay sites (refresh-request, onConfigChanged, first-load,
  onResume).
- Settings "Watch App" section gains a "Separate watch profile" editor: an enable
  Switch plus host/port/token fields and a Save (& push) button; the manual push
  button also uses the resolved target.

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `connection/WatchProfile.kt` (new) — model, storage, `resolveWatchDaemonTarget`.
  - `MainActivity.kt` — `publishWatchProfileToWatch` helper; 4 relay sites use it.
  - `ui/settings/SettingsScreen.kt` — watch-profile editor in WatchAppSection;
    manual push uses the resolved target.
  - test `WatchProfileTest.kt` (new) — 4 decision tests (enabled/usable,
    disabled fallback, blank host/token fallback, trimming).
- Tests: +4, -0, flipped 0.
- Behavioural delta: when a watch profile is enabled and usable, the watch
  receives that host/port/token instead of the phone's config.

## Embedded artefacts

- None. The pure decision logic is unit-tested; the SharedPreferences storage and
  the Settings UI require an Android Context / emulator (no AVD on this node), so
  those are validated by a clean `compileDebugKotlin` plus the decision tests.

## Operator-takeaway

The whole feature funnels through one pure function, `resolveWatchDaemonTarget`,
so "which connection does the watch use" is a single tested decision rather than
scattered logic. All four phone→watch publish paths and the manual push button
route through it, so enabling the watch profile consistently redirects the watch
to the funnel/relay address. Storage + UI are conventional; the watch side is
unchanged (it just receives a different host/port/token).
