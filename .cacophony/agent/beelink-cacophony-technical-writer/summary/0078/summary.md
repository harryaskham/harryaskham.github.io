# Session summary — TTS caller identity Pages docs

## Goal

Document the TTS daemon caller-identity contract in operator-facing Pages documentation so muted-policy diagnostics, traces, and `caco tts status --explain` expectations match the implemented `x-caco-caller` forwarding behavior.

## Bead(s)

- `bd-6e4c2d` — Document TTS daemon real caller identity in Pages

## Before state

- Failing tests: none known for this docs-only change.
- Relevant metrics: `./docs/validate-pages.sh` had not yet been run in this revived session.
- Context: `SPEC.md` and `README.md` already described real caller identity for TTS daemon speech, but Pages surfaces still discussed mute skips and TTS daemon behavior without explaining that `node-token` is only an auth fallback.

## After state

- Failing tests: none observed.
- Relevant metrics: `./docs/validate-pages.sh` passed with 3313 passed, 0 warnings, 0 failed.
- Context: Pages docs now state that feed-originated speech forwards the real sender via `x-caco-caller` / `X-Caco-Caller`, and that traces, muted-policy diagnostics, audits, and `tts status --explain` should name the source agent or inner feed sender when known.

## Diff summary

- Commits: `5ffc33340`.
- Files touched: `docs/api.html`, `docs/cli.html`, `docs/macos-development.md`, `docs/macos-development.html`, `docs/notifications.md`, `docs/notifications.html`.
- Tests: +0 / -0 / flipped 0; docs validation passed.
- Behavioural delta: Documentation-only. No application code or behavior changed.

## Operator-takeaway

Operators investigating silent or muted TTS should now see the same identity contract across Pages: agent-authored speech diagnostics should identify the real agent where known, and a visible `node-token` caller is a diagnostic drift signal rather than expected behavior.
