# Session summary — web agent group facet

## Goal

Deliver a bounded first slice of the broad agent-groups UI request by exposing a derived agent-group facet in the web Agents table, while splitting the remaining cross-surface and group-chat contract work into follow-up child beads instead of falsely closing the parent.

## Bead(s)

- `bd-974818` — Web Agents table exposes derived agent group facet
- Parent coordination bead: `bd-90539c` — Display agent groups across all UI surfaces

## Before state

- Web Agents table displayed ID, Project, Node, State, Bead, Runtime, Profile, Started, and Actions, but did not show the TUI-style group context for an agent.
- Agent table search did not match the derived node/status group context.
- Agent CSV export did not include any agent group field.
- Parent bead `bd-90539c` was broad: all UI surfaces plus group-scoped chat and management affordances.

## After state

- Web Agents table now includes a sortable Group column derived from node plus lifecycle/status group.
- Agent search matches the derived group label.
- Agent CSV export includes a Group column.
- The parent bead was split and blocked on child beads: `bd-974818`, `bd-3ee7bf`, and `bd-f28e79`; this session owns only the web table slice.

## Diff summary

- Code/content commits: `1fc6f5a2a` (`bd-974818: expose agent group in web agents table`)
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched: `crates/caco-web/static/app.js`, `crates/caco-web/static/index.html`, `crates/caco-web/static/style.css`
- Tests: source-only checks passed: `node --check crates/caco-web/static/app.js` and `git diff --check`
- Behavioural delta: web users can now see, sort, search, and export an agent's derived group context without changing chat semantics.

## Operator-takeaway

The broad “agent groups everywhere” request is now safely split. This landing gives the web Agents table visible group context immediately, while separate follow-ups own the canonical agent-group/chat contract and native/mobile surface parity.
