# Session summary — bd-df639c bd_mutate_request helper

## Goal

Eliminate hand-rolled mutating-HTTP dance in caco-cli triage paths so
new bd verbs can't drift back to leaking raw 'daemon returned NNN:'
shape (the bd-33b6d9 leak).

## Bead(s)

- `bd-df639c` — Audit caco-cli for hand-rolled mutating HTTP

## Before state

- 4 callsites in dispatch_bd_triage_action + interactive triage loop
  each manually built req \u2192 .json \u2192 .send \u2192 status-check \u2192
  extract_daemon_error_message
- Bug shape (wrong method, missing fallback, etc) had to be guarded
  per-callsite

## After state

- New `bd_mutate_request(client, method, url, token, project, node,
  body, op_label)` helper centralises the dance
- All 4 triage callsites migrated; behaviour preserved
- Regression test `bd_triage_actions_route_through_bd_mutate_request`
  asserts op_labels + helper stay present
- 39 other PATCH/POST/PUT callsites in lib.rs already use the proper
  `daemon_request_with_context` + `bd_send_request` plumbing; not
  in scope (the bead specifically targets the hand-rolled gap)

## Diff summary

- Commits: b18e41430645
- Files: `crates/caco-cli/src/lib.rs` (+89 -55)
- Tests: +1

## Operator-takeaway

When a fix establishes a "right way to do this", file a follow-up
bead pointing at the helper so the next agent picks the helper not
the copy-paste. bd-df639c was that follow-up for bd-33b6d9 and the
pattern worked exactly as intended.
