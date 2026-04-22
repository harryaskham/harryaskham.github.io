# Session summary — Profile frontmatter short_name_strategy plumbed through bridge

## Goal

Close the second half of bd-eef5da. Frontmatter
`short_name_strategy: adj_noun` in `.cacophony/profiles/*.md` should
be honoured by the daemon's short-name resolution chain. Before this
change, only config-declared profile entries (Config.profiles[]) were
plumbed (bd-eef5da, this session); profile files on disk were not.

## Bead(s)

- `bd-c5783b` — [bd-eef5da follow-up] short_name_strategy from profile
  frontmatter (.md) not yet plumbed
- (parent: `bd-eef5da` — closed earlier this session)
- (related: `bd-bb3b5f` original strategy work, `bd-34d0b8` AdjNoun
  fallback)

## Before state

- Failing tests: none. caco-profile bridge: ~120 passing; caco-daemon
  short_name: 11 passing.
- `caco_profile::Profile` had no `short_name_strategy` field — the
  YAML frontmatter parser silently dropped it.
- The two `resolve_short_name` callsites in `caco-daemon/lib.rs`
  consulted only the config-declared lookup added in bd-eef5da.

## After state

- Failing tests: none. caco-profile bridge: +2 passing; caco-daemon
  short_name surface: 14 passing (+3 new).
- `caco_profile::Profile.short_name_strategy: Option<String>` parses
  from frontmatter as a raw string (so this crate stays free of
  `caco-config::ShortNameStrategy`).
- `BridgeOutput.short_name_strategy: Option<String>` round-trips it
  through every bridge target (Claude, Codex, Pi, Test).
- `compose_profiles` composes it via last-one-wins, matching the
  pattern used by other scalar overrides (background_image, voice,
  initial_prompt, ...).
- `agent::ResolvedProfile.short_name_strategy` carries it into the
  daemon spawn paths.
- Both spawn callsites (persistent-launch and API-spawn) prefer the
  bridge-output value parsed by `parse_short_name_strategy_str()`,
  falling back to the bd-eef5da config-declared lookup.
- Tolerated frontmatter aliases: `adj_noun`, `adj-noun`, `AdjNoun`,
  surrounding whitespace. The `llm` strategy is not expressible as
  bare frontmatter shorthand because it requires a companion key —
  config-declared profile entries handle that case.
- Unknown frontmatter values return `None` instead of crashing,
  gracefully falling through to the cluster-default.

## Diff summary

- Commit: `8662aa61`
- Files touched:
  - `crates/caco-profile/src/model.rs` (+19): new field
  - `crates/caco-profile/src/bridge.rs` (+~50): new field on
    BridgeOutput, populate at four bridge sites, +2 unit tests
  - `crates/caco-profile/src/compose.rs` (+10): last-one-wins compose
  - `crates/caco-profile/src/lib.rs` (+1): test fixture field
  - `crates/caco-daemon/src/agent/types.rs` (+12): new field on
    ResolvedProfile
  - `crates/caco-daemon/src/agent/profile.rs` (+2): plumb through
    `resolve_profile` and the composite path
  - `crates/caco-daemon/src/lib.rs` (+~70): new
    `parse_short_name_strategy_str()`, prefer bridge-output value at
    both callsites, +3 unit tests
- Tests: +5 / -0 / flipped 0 (caco-profile: 2, caco-daemon: 3)
- Behavioural delta: agents whose profile file frontmatter declares
  `short_name_strategy: adj_noun` now receive the configured strategy
  instead of falling through to the cluster-default. Backwards-compat
  preserved: profiles without the field, or with an unknown value,
  behave exactly as before.

## Embedded artefacts

(none — pure backend change)

## Operator-takeaway

Both halves of the original bd-eef5da gap are now closed. The full
3-level resolution chain (`persistent decl > project agent_defaults >
profile`) is live for both config-declared profile entries (this
session, earlier) and profile-file frontmatter (this commit). The
parser intentionally accepts only the bare-string variants (AdjNoun,
plus aliases) — the `Llm { llm: <key> }` variant remains the
config-declared route, since it needs an associated map. Worth
calling out in caco-doctor: any profile file that wants a custom
short-name strategy should add `short_name_strategy: adj_noun` to its
frontmatter.
