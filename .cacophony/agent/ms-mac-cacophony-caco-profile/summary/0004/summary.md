# Session summary — persistent specialist inbox fallback cadence

## Goal

Respond to the operator report that workers were still being stopped by inbox nudges after the Pi-native inbox watcher work.

## Bead

- `bd-06d9b5` — `[profile] Move persistent specialist inbox nudges to Pi-native inbox fallback cadence`

## Findings

- The Pi-native inbox mixin/overlay already landed under `bd-ec3e34` and is composed through `.cacophony/agents/pi-common.yaml`.
- Shared `persistent.yaml` and `persistent-observer.yaml` already use a sparse daemon/prompt fallback inbox cadence of `1800` seconds.
- `.cacophony/agents/persistent-specialist.yaml` still had `cadence.inbox_poll_interval_secs: 300`, leaving specialist persistent workers subject to five-minute daemon/prompt-level inbox nudges even though `pi-inbox` is now primary.

## Change

- Updated `.cacophony/agents/persistent-specialist.yaml` to `inbox_poll_interval_secs: 1800` with comments explaining that Pi-native `pi-inbox` is primary and prompt-level inbox nudges are fallback only.
- Updated `AGENTS.md` and `README.md` to include `persistent-specialist.yaml` in the documented sparse fallback cadence contract.

## Validation

- `git diff --check` passed using a clean macOS tool environment after the ambient Nix toolchain hit dyld code-signature failures.

## Operational note

Existing live persistent specialist agents may need `caco agent recreate` or equivalent profile-artifact refresh to pick up the new cadence; editing the checked-in snippet does not rewrite already materialized live prompts/config in-place.
