# Session summary — bd-ea9bf0 web chat full-history local view

## Goal

Add an explicit web chat path for loading broader local message history into the browser-side chat view, so operators are not limited to the bounded snapshot/SSE tail when reviewing past conversations.

## Bead(s)

- `bd-ea9bf0` — Build full chat history view with local data

## Before state

- Web chat rendered `state.chatMessages` from UI snapshot `chat_history` plus SSE-fed rows, bead/chat synthetic cards, reintegration cards, and speech events.
- A time-window control already existed, but broader history required whatever rows were already in the snapshot/SSE model; there was no explicit operator action to fetch a fuller daemon-backed chat history into the local browser model.
- TUI/CLI already had deeper message-history/threading surfaces; this slice targets the caco-web/browser chat view only.

## After state

- Added a `Load full history` button to the web Chat view.
- Added a chat search box that filters rendered rows by sender, body, project, and event type.
- `loadFullChatHistory()` fetches bounded full-body message history from existing daemon HTTP endpoints:
  - selected project when a project filter/channel is active;
  - the active agent's project when an agent channel is active;
  - otherwise every configured project, bounded per project by the existing chat endpoint `limit` parameter.
- Fetched rows merge/dedupe into `state.chatMessages` using stable IDs/local IDs/fallback keys, preserving existing SSE/snapshot rows and local delivery rows.
- The renderer applies project/channel/type/time-window/search filters to loaded rows.
- When loaded history exceeds the render cap, the UI shows an explicit truncation banner (latest N of M filtered messages) instead of silently pretending only rendered rows exist.
- The normal `/api/v1/ui/snapshot` and SSE flow remains unchanged; this is a browser-local reader action backed by existing daemon message-history APIs.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched:
  - `crates/caco-web/static/app.js`
  - `crates/caco-web/static/index.html`
  - `SPEC.md`
  - `README.md`
  - `AGENTS.md`
- Behavioural delta:
  - web Chat view now has a full-history fetch action and local search.
  - renderer cap increases to 500 after history is loaded and always shows truncation status when filtered results exceed the cap.
  - docs/specs now record the web chat-history browsing contract.

## Validation

- `node --check crates/caco-web/static/app.js` passed.
- Source contract grep confirmed the new controls/helpers are present:
  - `chat-load-history-btn`
  - `chat-search-filter`
  - `loadFullChatHistory`
  - `fetchFullChatHistoryForProject`
  - `chatHistoryLoadedAt`
- Manual diff review completed.

## Operator-takeaway

The browser Chat view now has an explicit "Load full history" path that pulls bounded daemon-backed history into local state, deduplicates it with existing live rows, and makes it searchable/filterable without changing daemon APIs or relying on local filesystem state.
