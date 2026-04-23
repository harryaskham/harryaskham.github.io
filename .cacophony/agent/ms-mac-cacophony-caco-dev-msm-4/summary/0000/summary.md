# bd-bb75b5 — webapp chat /commands now actually run

## Goal
Stop silently swallowing slash commands typed into the webapp
chat box. Operators were picking commands from the autocomplete,
hitting send, and seeing no observable effect because the literal
`/foo` text was just being posted as a chat message.

## Bead(s)
- bd-bb75b5 (P2 task) — bare title, no acceptance criteria.
  Implemented client-side dispatch for the existing
  `SLASH_COMMANDS` set already wired into the autocomplete.

## Before state
- `crates/caco-web/static/app.js::sendChat` always shipped the
  raw input body to one of three message endpoints
  (`messages/speak`, `messages/broadcast`, `messages/send`).
- `SLASH_COMMANDS` (10 entries) was used only by the autocomplete
  popup; nothing read the parsed command on submit.
- Effect: typing `/dispatch bd-xxx` and hitting send posted the
  literal string `/dispatch bd-xxx` as a broadcast/speak. No
  command ran.

## After state
- `sendChat` checks for a leading `/` and, when present, calls
  the new `dispatchSlashCommand(raw)` handler instead of falling
  through to the message endpoints.
- `dispatchSlashCommand` is a pure client-side router covering
  all 10 commands the autocomplete already advertises:
  - `/clear` blanks the input.
  - `/help` toasts the SLASH_COMMANDS list.
  - `/inbox`, `/agents`, `/beads`, `/who` switch the hash route
    to the matching tab (`/beads` preserves project context).
  - `/speak <body>` and `/broadcast <body>` reuse the existing
    speak / broadcast REST endpoints.
  - `/dispatch <bead-id>` POSTs `beads/{id}/dispatch`.
  - `/claim <bead-id>` POSTs `beads/claim` with `{bead_id}` body
    (matches the actual server route, not `/beads/{id}/claim`).
- Unknown commands surface an explicit
  `Unknown command: /foo. Type /help for the list.` toast
  instead of silently posting them as chat text.
- Empty-arg commands (`/speak`, `/broadcast`, `/dispatch`,
  `/claim` with no operand) print a usage hint toast.
- Successful commands clear the input and hide the autocomplete
  dropdown via the existing `hideSlashSuggest()` helper.

## Diff summary
- `crates/caco-web/static/app.js` (+145):
  - `sendChat` early-return + slash-command branch.
  - New `dispatchSlashCommand(raw)` handler.

No daemon contract change. All endpoints used already exist.

## Tests
- The webapp static JS bundle has no JS test runner in-tree.
- `node --check crates/caco-web/static/app.js` — clean parse.
- `cargo build -p caco-web` / `clippy -p caco-web --all-targets -- -D warnings` — clean.

## Operator-takeaway
After binary roll, slash commands typed into the webapp chat box
take effect: `/beads`, `/agents`, `/inbox` jump to the
corresponding tab; `/speak <body>` / `/broadcast <body>` post
narration; `/dispatch <bead>` and `/claim <bead>` actually
dispatch and claim. Mistyped commands surface a clear error
toast instead of vanishing into the chat log as inert text.
