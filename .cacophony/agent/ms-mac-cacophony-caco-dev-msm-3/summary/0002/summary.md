# Session summary — web Choices tab response extraction

## Goal

Make the web app's Choices tab show active choices instead of always
appearing empty even when the daemon has live pending choices.

## Bead(s)

- `bd-1311fa` — Web app: Choices tab is always empty despite there being active choices.

## Before state

- `GET /api/v1/choices/list` returns the standard SuccessEnvelope:
  `{ok: true, data: {choices: [...], count, status_filter}, request_id, ...}`
- `loadChoices()` in `crates/caco-web/static/app.js` did:
  `state.choices = data.choices || data.data || data || []`
- `data.choices` is undefined, `data.data` is the envelope's inner object (truthy), so the assignment stored the *object* — not an array — into `state.choices`.
- The next line, `if (Array.isArray(state.choices))`, short-circuited and skipped `renderChoices()` / `updateChoicesBadge()`. The Choices tab therefore stayed at its initial empty state.
- Failing tests: none (no JS-side test infra in caco-web).

## After state

- `loadChoices()` now reaches into `data.data.choices` first, with `data.choices` and bare-array fallbacks, mirroring the existing pattern used by `loadAgentLogs` / `loadAgentDiff` (`data.X || data.data?.X`).
- `renderChoices()` is called unconditionally so the empty-state and badge update even when the response shape is unexpected.
- `cargo build -p caco-web` clean (pure JS change, no Rust touched in the static asset).

## Diff summary

- Commits: `56ca6b1a`
- Files touched: `crates/caco-web/static/app.js`
- Tests: 0 (no JS test harness exists; Android client test in `AppStateStoreTest.kt` already covers a similar shape).
- Behavioural delta: web `/choices` tab now populates from active choices and the badge count updates.

## Operator-takeaway

The Android client was unaffected because it explicitly reads `payload.choices` from the SSE envelope shape, not the REST `/choices/list` shape. If the TUI ever shows the same symptom, look for the same `data.X || data.data` pattern — it is a recurring footgun any time a handler returns an object that is itself truthy.
