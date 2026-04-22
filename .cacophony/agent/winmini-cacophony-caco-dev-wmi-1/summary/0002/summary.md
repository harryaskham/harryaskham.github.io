# Session summary — Plumb config-declared profile short_name_strategy

## Goal

Close half of bd-eef5da: at the two `resolve_short_name` callsites in
`caco-daemon/lib.rs`, the third resolution level (`profile_strategy`)
was hardcoded to `None`, so any `short_name_strategy` declared on a
config profile entry was silently ignored. Wire it up for the
config-declared case (`Config.profiles[].short_name_strategy`),
defer profile-frontmatter plumbing to a follow-up.

## Bead(s)

- `bd-eef5da` — short_name_strategy from profile is never applied;
  profile_strategy hardcoded None at both callsites
- (parent investigation: `bd-34d0b8`)
- (related: `bd-bb3b5f` — original short_name strategy work)
- Filed follow-up for the frontmatter half (queued via outbox while
  beads primary was unreachable; controllers will surface the new ID).

## Before state

- Failing tests: none in `caco-daemon::short_name` (4 passing).
- Both callsites in `caco-daemon/src/lib.rs` (persistent-launch
  ~line 4549, API-spawn ~line 25869) passed `None` for
  `profile_strategy` to `resolve_short_name`. Comment cited
  "Profile-level strategy resolved via bridge output (future)".
- Symptom was masked by bd-34d0b8's AdjNoun fallback, but the third
  resolution level was effectively dead code.

## After state

- Failing tests: none. `caco-daemon::short_name` 4 passing + 2 new
  module-level tests = 8 total.
- New helper `lookup_profile_short_name_strategy(config, profile_name)`
  in `caco-daemon/src/lib.rs`:
  - Returns `Option<&caco_config::ShortNameStrategy>` from a
    config-declared profile entry by name.
  - Handles composite profile names (`"a+b"`) produced by
    `caco_profile::compose_profiles` by scanning components in
    declared order; first component with a non-None strategy wins.
  - Safe defaults: returns `None` for unknown names, missing
    `profiles:` section, or `None` input.
- Both callsites hoist the lookup to a `let` before the
  `AgentCreateRequest` struct literal moves `profile_name`, then pass
  the cloned strategy into `resolve_short_name`.
- Profile frontmatter (`.cacophony/profiles/*.md`) plumbing is filed
  as a separate follow-up (queued in outbox).

## Diff summary

- Commit: `c573260a`
- Files touched:
  - `crates/caco-daemon/src/lib.rs` (+102 / -4):
    - new `lookup_profile_short_name_strategy` helper (33 lines incl.
      docstring)
    - hoist + wire at 2 callsites (8 lines)
    - 2 new unit tests
- Tests: +2 / -0 / flipped 0 (caco-daemon short_name surface: 6 → 8)
- Behavioural delta: agents whose effective profile is a name listed
  in `Config.profiles` with `short_name_strategy: <variant>` set will
  now receive that strategy instead of falling through to the
  cluster-wide AdjNoun default. No change for agents whose strategy
  is set on the persistent decl or project agent_defaults (those
  already won the chain). No change for profiles that exist only as
  files on disk — that is the follow-up.

## Embedded artefacts

(none — pure backend change)

## Operator-takeaway

Half of bd-eef5da is closed: the dead "profile_strategy" slot in the
short-name resolution chain is now live for config-declared profile
entries, and is verified by tests that lock both the single-name and
composite-name paths. The remaining frontmatter half is a small but
distinct change (touches `caco-profile::Profile` and the bridge
output type), filed as a follow-up so workers can pick it up
without scope creep here. The cluster-wide AdjNoun fallback from
bd-34d0b8 remains the safety net for unconfigured paths.
