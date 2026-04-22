# Session 0017 — bd-e8288c (auto-apply ProfileComms.mute_broadcasts)

## Goal

bd-8b59a1 slice 1 shipped `caco msg inbox --mute <pattern>` plus the
`ProfileComms.mute_broadcasts` frontmatter field. This slice closes
the loop: when no explicit `--mute` is supplied, resolve the caller's
composed profile and apply `comms.mute_broadcasts` automatically.

Profile-author writes once:

```yaml
# .cacophony/profiles/caco-doctor.md frontmatter:
comms:
  scope: project_wide
  mute_broadcasts:
    - "caco-dev-* notes"
```

…and `caco msg inbox` from inside that agent silently drops those
broadcasts. Explicit `--mute` always wins (operator override).

## Bead(s)

- **bd-e8288c** — primary.

## Before state

- `caco msg inbox --mute pat1,pat2` worked (bd-8b59a1).
- `ProfileComms.mute_broadcasts: Vec<String>` field existed and
  parsed (bd-8b59a1) but **wasn't read anywhere** — `compose_comms`
  in `caco-profile/src/compose.rs` discarded it (always returned
  `mute_broadcasts: Vec::new()`), so the field was effectively
  inert.
- Non-dev persistents had no opt-out path for routine
  `caco-dev-* notes` operator broadcasts beyond passing
  `--mute "caco-dev-* notes"` on every invocation.

## After state

- `compose_comms`: now unions `mute_broadcasts` across the profile
  chain, dedups, preserves first-seen order. Returns
  `Some(ProfileComms{...})` only when at least one profile carries
  a `comms` block (preserves the historical "absent block" behaviour).
- New helper `derive_mute_patterns_from_caller_profile()` in
  caco-cli: best-effort lookup chain
  1. `CACO_AGENT_ID` / `CACOPHONY_AGENT` env var,
  2. scan `<runtime>/agents/*/<id>/agent.json`,
  3. read `profile` (`+`-joined chain) and `checkout_path`,
  4. for each profile name, load
     `<checkout>/.cacophony/profiles/<name>.md`,
  5. `caco_profile::compose_profiles` then return
     `composed.comms.mute_broadcasts`.
  Any failure in any step returns an empty Vec — auto-mute is
  best-effort and must never block reading the inbox.
- `caco msg inbox` arm: when explicit `--mute` is empty, falls back
  to `derive_mute_patterns_from_caller_profile()`. Explicit override
  always wins.

## Tests

- New: `compose_comms_unions_mute_broadcasts` (caco-profile) — locks
  the union/dedup/order contract across a 3-profile chain.
- New: `compose_comms_none_when_all_omitted` (caco-profile) —
  preserves the legacy "absent block" return.
- New: `derive_mute_patterns_returns_empty_without_env` (caco-cli) —
  best-effort: no env var → empty Vec, no panic.
- New: `derive_mute_patterns_returns_empty_for_unknown_agent`
  (caco-cli) — best-effort: unknown agent ID → empty Vec, no panic.

## Validation

- `cargo test -p caco-profile compose_comms`: 3/3 PASS.
- `cargo test -p caco-cli --lib derive_mute`: 2/2 PASS.
- `cargo test -p caco-cli --lib doctor_strict`: 1/1 PASS (re-checked
  after fixing the `#[test]` attribute I accidentally moved during
  edit-merge).
- `cargo test-small`: 4257+ tests across 8 binaries, 0 failures.
- `cargo clippy -p caco-cli -p caco-daemon -p caco-beads -p caco-web
  -p caco-profile --all-targets -- -D warnings`: clean.

## Diff summary

```
crates/caco-profile/src/compose.rs               | ~+50 / -10
crates/caco-cli/src/lib.rs                       | ~+110 / -2
.cacophony/agent/.../summary/0017                | (new)
```

## Operator-takeaway

Profile-authors of non-dev persistents: add to your profile's
frontmatter:

```yaml
comms:
  scope: project_wide          # or whatever you already have
  mute_broadcasts:
    - "caco-dev-* notes"
    - "any other recurring substring"
```

Inside the agent, `caco msg inbox` will now silently drop matching
broadcasts. Explicit `--mute pat1,pat2` still overrides — useful for
one-off "look at everything" reads.

## Coordination

- Spoke claim with planned scope.
- Will speak completion + reintegrate.

## Notes for next time

- Discovered + fixed an inert-field bug in `compose_comms` while
  implementing this. Pattern: always grep for actual readers when
  validating a "shipped slice 1" before composing on top of it. The
  field existed, parsed, serialized — but `compose_comms` was the
  composition step and it dropped the value. Worth a follow-up to
  audit other `compose_*` helpers in `caco-profile` for similar
  discard-on-compose bugs.
- The `derive_mute_patterns_from_caller_profile` helper is the
  first CLI-side composed-profile reader. If a third caller needs
  the same lookup, promote it to a shared module (e.g.
  `caco-cli::profile::resolve_caller_profile`) returning a full
  composed `Profile`.
