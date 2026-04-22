# Session summary — bd-de0282 extend eager profile validation

## Goal

Follow-up to bd-0977ba: cover the two remaining `ProfileSelection`
surfaces that previously fell through to spawn-time failure.
Re-use the same pure helper, same hard-error severity, same
remediation-hint shape.

## Bead(s)

- `bd-de0282` — [bd-0977ba follow-up] eager-validate profile refs
  in agents.presets[].profile and rule SpawnActionParams.profile.

## Before state

- `agents.presets[].profile`: validated only by the spawn path;
  typo broke every spawn that picked the preset.
- `modes.<name>.rules[N].then.spawn.profile` and
  `.then.spawn_and_claim.profile`: validated only at rule-fire
  time; typo silently failed the rule.

## After state

Both surfaces now hard-reject at config-load time with the same
remediation-hint phrasing as bd-0977ba. Errors include the
preset id (for preset.profile) or full rule path (for rule.then.*)
so operators can navigate straight to the fix site.

## Files touched

- `crates/caco-config/src/validate.rs` (+~190 / -5).
- `crates/caco-config/tests/config.rs` (+3 one-liner profile
  declarations to keep three pre-existing YAML-parse tests
  green under the new eager check).

## Diff summary

Three production-code edits in `crates/caco-config/src/validate.rs`:

1. Inside the existing preset loop in
   `validate_config_with_extra_profiles`, add a check that calls
   `missing_profiles_in_selection(preset.profile, &profile_names)`
   and emits one hard error per missing entry naming the preset
   id and missing profile.
2. `validate_modes` and `validate_then_action` gain a
   `profile_names: &HashSet<&str>` parameter so the rule-path
   action validators can call the same shared helper. This is
   the same threading pattern used by `validate_concurrency`
   already.
3. `validate_then_action`'s `Spawn` and `SpawnAndClaim` arms now
   each call `missing_profiles_in_selection` on the params'
   optional profile field and emit hard errors per missing entry.

Test additions: a new `preset_and_rule_profile_bd_de0282`
submodule under `validate::tests` with 8 tests covering accept /
reject / no-profile / composite-with-missings for both preset
and rule-action surfaces. Uses the same pattern as the
bd-0977ba tests (synthetic fixture, extra_profile_names supplied
directly, no I/O).

Test-fixture updates required because the new eager check is now
applied to existing tests:

- `validate::tests::accepts_valid_burndown_mode`: previously
  passed because the rule-action profile was unchecked. Now
  declares `worker` in `config.profiles` so the eager check sees
  it. Single-block addition, no behavioural change to what the
  test exercises.
- `tests/config.rs::spec_documented_burndown_mode_yaml_parses`,
  `all_then_action_variants_parse_from_mapping_yaml`,
  `then_action_yaml_tag_form_still_works`: each appends a
  `profiles: [{ name: worker }]` block to the YAML literal so
  validation continues to pass. Tests still exercise the same
  YAML parsing / round-trip logic; the new block is a one-liner.

Severity choice — hard error not warning, same reasoning as
bd-0977ba: presets and rule defaults are templates that affect
every spawn picking them up.

## Operator-takeaway

`caco config validate` (and `caco config validate --strict`) now
catches typos in three more profile-reference surfaces:
`agents.presets[].profile`, `modes.<name>.rules[N].then.spawn
.profile`, and `modes.<name>.rules[N].then.spawn_and_claim
.profile`. Workflow unchanged; previously-silent failure modes
become eagerly surfaced. Existing valid configs continue to
validate; existing invalid configs that would have crashed at
spawn-/rule-fire time now fail at config-load with a clear path
to the offending field.

## Validation

- `cargo test -p caco-config preset_and_rule_profile_bd_de0282`:
  8/8 PASS.
- `cargo test -p caco-config`: 739 lib + 290 integration + 1 doc
  PASS green (delta: +8 in lib, +0 in integration; the existing
  burndown-mode and YAML-parse tests still pass after the
  one-liner profile additions).
- `cargo clippy -p caco-config --all-targets -- -D warnings`:
  clean.
- `cargo test-small`: blocked by a pre-existing caco-tui compile
  failure on main (`state::AgentDisplayState` missing
  `tmux_history_limit` / `tmux_history_size` fields, presumably
  bd-7ef076 in-flight). Verified the breakage exists on
  `origin/main` independent of this commit (git stash + rebuild
  from clean main reproduced the same errors). Not in scope to
  fix here.

## Notes / follow-ups

- bd-7ef076 caco-tui compile failure is blocking workspace-wide
  test-small for everyone; would be worth a quick fix or revert
  by whoever's mid-flight on it.
- Remaining unchecked profile-reference surfaces in the data
  model (per a quick grep): none in the validate-relevant types.
  `agents.presets[].command` and `scopes` / `mcp_servers` (the
  bead's item 3) are different shapes and pair with bd-aac755 /
  bd-8a56ce already in_progress.
- bd-845653 / bd-58ff27 / bd-c24ff7 / bd-0977ba still on main
  but bead-close blocked until the daemon picks up bd-845653
  (commit-message bead-id harvester) on next operator restart.
