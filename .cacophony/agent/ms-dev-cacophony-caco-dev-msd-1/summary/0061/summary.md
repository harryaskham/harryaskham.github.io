# bd-b0a722 summary

## Bead
- bd-b0a722 — [pi-inbox] Deliver inbox notifications without stopping active Pi work

## Changes
- Changed .cacophony/pi/inbox/extensions/caco-inbox.mjs so every delivered inbox prompt uses Pi delivery options with deliverAs set to the configured non-stopping mode (default followUp), including idle deliveries.
- Preserved the busy de-duplication gate so only one follow-up is queued while the agent is busy, then delivery resumes after agent_end.
- Added regression coverage proving new inbox messages are delivered as followUp, busy notifications queue as a single followUp, and explicit steer mode still uses delivery options rather than a normal prompt turn.
- Updated pi-inbox profile/README/AGENTS/docs HTML to document non-stopping follow-up delivery and live-agent refresh implications.

## Validation
- node --test .cacophony/pi/inbox/extensions/caco-inbox.test.mjs — passed locally.
- caco test run --wait --command "node --test .cacophony/pi/inbox/extensions/caco-inbox.test.mjs" — passed as tj-ba9a3754.
- git diff --check — passed.
- ./docs/validate-pages.sh — passed locally, 3313 checks.
- caco test run --wait --command "cargo run -p caco-profile --bin caco-docs-gen" — passed as tj-8b49be5a; docs/profiles.html already up-to-date.

## Operational note
- Already-running Pi agents need recreation/profile artifact refresh before the changed .cacophony/pi/inbox overlay is materialized into their workspaces.
