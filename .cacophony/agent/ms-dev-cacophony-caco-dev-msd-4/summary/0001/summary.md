# Session summary — bd-b9b8af lifecycle inbox suppression

## Goal

Stop per-node `caco restart` announcements from flooding every
controller's inbox during fleet-wide version bumps without losing
the audio narrator's ability to surface them.

## Bead(s)

- `bd-b9b8af` — Restart-broadcast spam: every `caco restart` produces 1-2 broadcasts that flood every controller's inbox

## Before state

- Failing tests: bd-c19193 (pre-existing, unrelated).
- A prior partial fix had already de-duped the dual `speak`+`broadcast` emit down to speak-only — that addressed the bead's suggestion #3 (drop the redundant emit).
- The remaining N×1 noise (one speak per node × 5 nodes per restart) still landed in every controller's inbox because:
  - The hook calls `caco msg speak` from a shell, which produces sender `<node>:cacophony:harry` (no `:hook` suffix) — the existing `is_lifecycle_message` body+sender pattern matcher only suppresses *direct-send* injection for lifecycle messages, not inbox visibility.
  - There is no public `caco msg system` CLI (only an internal `Message::system` constructor for daemon-emitted lifecycle events), so the hook had no way to opt into the existing `kind: System` inbox suppression.
  - `read_inbox_query`'s `exclude_system` gate filtered only on `m.kind != 'system'`, missing any other lifecycle-tagged content.

## After state

- Failing tests: bd-c19193 (unchanged, pre-existing).
- `MessageStore::read_inbox_query` now treats body-prefix `[lifecycle]` as semantically equivalent to `kind: system` for default-inbox suppression on both the direct-message and broadcast/speak SQL branches. `include_system: true` and `kind: Some(System)` opt back in as before.
- The daemon-restart hook in `.cacophony/automation.yaml` now prefixes the announcement body with `[lifecycle] ` so it benefits from the new suppression.
- Audio narrator path is untouched — lifecycle-prefixed speak messages still hit the global TTS surface, so operators still hear restart announcements; only inbox polling is quieter.
- Three new regression tests cover broadcast suppression, direct-message suppression, and the `include_system` opt-in.

## Diff summary

- Commits: `4a287857 bd-b9b8af: suppress [lifecycle]-prefixed broadcasts/speaks from default inbox`
- Files touched:
  - `crates/caco-daemon/src/messaging.rs` (+2 SQL clauses, +3 tests, +89 lines total)
  - `.cacophony/automation.yaml` (+13 comment lines, body-prefix `[lifecycle] ` added to one MSG variable)
- Tests: +3 / -0 / flipped 0 — `inbox_default_excludes_lifecycle_prefixed_broadcasts`, `inbox_default_excludes_lifecycle_prefixed_direct`, `inbox_include_system_returns_lifecycle_prefixed_broadcasts`.
- Behavioural delta: any message (broadcast, speak, or direct) whose body starts with `[lifecycle]` is now excluded from the default `caco msg inbox` view across the fleet. The first practical use is restart announcements; the mechanism is also available to any other shell-side hook that wants to emit operator-audible-but-inbox-quiet messages.

## Operator-takeaway

This closes bd-b9b8af's remaining gap (suggestions #1 and #2 from the
original report) without growing CLI surface area. The body-prefix
contract is documented in the automation.yaml comment and in the
`read_inbox_query` SQL; future automation that wants the same
treatment can adopt it by prefixing `[lifecycle]` to the message body.
If we later add a proper `caco msg system` CLI we can migrate the
restart hook to that and drop the body-prefix dance, but the prefix
remains useful for shell scripts and webhooks that don't have a
typed message-kind selector.
