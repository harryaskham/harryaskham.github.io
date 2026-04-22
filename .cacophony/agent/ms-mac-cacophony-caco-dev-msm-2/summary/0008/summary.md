# Session summary — docs/profiles.html frontmatter expansion (bd-31b6be)

## Goal

`docs/profiles.html` listed 17 frontmatter fields; the shipped `caco_profile::Profile` struct has 35+. Bring the public docs closer to reality without trying to enumerate every shipped profile in one pass.

## Bead(s)

- `bd-31b6be` — Update profiles docs to cover shipped profiles and undocumented frontmatter fields. Promoted from draft → open before claiming.

## Before state

Frontmatter table covered: name, description, agent_types, skills, mcp_servers, permission_mode, memory, hooks, reintegration, persistent, banned_modes, authorization, watchdog, stop_nudge_text, git_remotes, shell_rc, include_caco_instructions, stale_timeout_secs, reintegration_checks, hook_mixins.

Hook-phases table actually already listed on_stop / on_session_end / on_notification — the bead's claim that they were missing was stale.

## After state

Frontmatter table now also covers: env, message_subscriptions, comms, model/provider/effort, lifecycle, composes, composes_well_with, completion, nudges, permitted_actions, allowed_lifecycle_operations, cross_project_bead_permissions, background_image, voice, initial_prompt, pi_extra_config_dirs, short_name_strategy.

Reintegration Modes section gains a paragraph explaining the composable `recorded` suffix (e.g. `direct,recorded`) and the session-recording mixin requirement (bd-d48494).

- Docs-only change; no test impact.
- `git diff --stat`: 1 file, +19 lines.

## Diff summary

- Commit: `8f81a151`
- Files touched: `docs/profiles.html`.
- Tests: 0 (docs).

## Out of scope

- Per-shipped-profile reference page — there are 30+ profiles in `.cacophony/profiles/` and `configs/profiles/`; a generated docs section is the right shape, deserving its own bead.
- Persistent vs `reintegration: direct` lifecycle explainer — separate prose work.

## Operator-takeaway

Operators reading the docs now see the fields they actually encounter in shipped profiles and understand where the `recorded` suffix's summary-artefact requirement comes from.
