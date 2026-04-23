# Session summary — docs/profiles.html: shipped profiles, mixins, frontmatter fields

## Goal

Bring `docs/profiles.html` into alignment with the actual shipped profile fleet under `.cacophony/profiles/` (54 profiles vs the ~25 the docs covered) and with the frontmatter and hook-phase fields that real profiles use day-to-day. The page was operator-misleading: it showed a generic skeleton, missed every mixin shape, omitted authorization scope details, and silently dropped half the shipped profiles.

## Bead(s)

- `bd-31b6be` — Update profiles docs to cover shipped profiles and undocumented frontmatter fields

## Before state

- `docs/profiles.html`: 331 lines, ~25 of 54 shipped profiles documented, no mixin section, frontmatter table missing `disallowedTools`, `type`, `environment`, `restart`, `stop_hook_delay`; `authorization` row showed only the wrong values (`worker`, `controller`); `reintegration` row had no field-shape detail.
- Hook phases table mentioned `on_stop`, `on_session_end`, `on_notification` only by trigger time, no operational notes.
- No section explaining the persistent vs one-shot vs mixin distinction even though it is structurally fundamental to how the daemon treats each profile.
- Failing tests: none (docs-only).

## After state

- `docs/profiles.html`: 375 lines, all 54 shipped profiles listed and grouped by category (worker / one-shot, persistent coordinators, mixins), each with a one-line purpose.
- Frontmatter table now documents `disallowedTools`, `type`, `environment`, `restart`, `stop_hook_delay`; `authorization` row links to `authorization-scopes.md` and lists all three scopes (`worker`, `project_controller`, `cluster_controller`); `reintegration` row covers the `mode` enum, `allowed_modes`, and the `,recorded` suffix.
- Hook phases table now annotates `on_stop` (`stop_nudge_text` + `stop_hook_delay`), `on_session_end` (block premature exits), and `on_notification` (inbox / watchdog / choice resolution).
- New section "Persistent vs One-Shot vs Mixin Profiles" explains the three operationally distinct shapes and what `reintegration: none` actually means for persistent agents.
- Failing tests: none. `cargo test-small` green (57 passed).
- HTML still parses cleanly (Python `html.parser` round-trip).

## Diff summary

- Commit: ceffc1bc4
- File: `docs/profiles.html` (+72 / −28)
- No code changes. Pure documentation.
- Behavioural delta: none for the runtime; operator-readability of `docs/profiles.html` materially improved.

## Operator-takeaway

`.cacophony/profiles/` has grown to 54 profiles split across one-shot workers, persistent coordinators, and pure mixins, but the docs page lagged badly — anyone reading it would have walked away with the wrong mental model of what fields real profiles use and what the persistent / mixin shapes mean for the lifecycle. Worth setting up a periodic doc-vs-disk drift check (e.g. a `repo-health` sweep or a CI test that asserts every profile in `.cacophony/profiles/` appears in `docs/profiles.html`) so this doesn't slide again.
