# Session summary — bd-2dc0c3: caco ps validator parity across all four filters

## Goal

Make `caco ps` reject bogus values for `--kind`, `--state`, and
`--project` the same way it already rejects bogus `--node`,
eliminating the silent-degrade-to-empty-result pattern that hid
operator typos behind a generic "no matching jobs" message.

## Bead(s)

- `bd-2dc0c3` — `caco ps validator INCONSISTENCY within single
  surface: --node bogus is gold-standard but --kind / --state /
  --project all silently return 'no matching jobs' (1 of 4
  filters validates)`.

## Before state

- `caco ps --node bogus` →
  `caco ps — no jobs on node 'bogus' (local node is 'helsinki')`
  (gold-standard).
- `caco ps --kind bogus` → `caco ps — node: helsinki / no
  matching jobs` (silent).
- `caco ps --state bogus` → silent.
- `caco ps --project nonexistent` → silent.

Worst form of the silent-filter pattern: same surface, same
function, four filter parameters, one validates and three don't.
Operator can't tell typo from genuinely-empty result.

## After state

`dispatch_ps` declares two `const` sets at function entry:

- `KNOWN_KINDS = ["service", "sidecar", "agent", "repair"]`
- `KNOWN_STATES = ["running", "stopped", "unhealthy", "unknown",
  "completed", "failed", "degraded", "resolved"]`

Upfront validators check `--kind`, `--state`, and `--project`
before any daemon calls and return a clear error naming the
valid set:

- `caco ps: unknown --kind 'bogus'. Valid kinds: service,
  sidecar, agent, repair`
- `caco ps: unknown --state 'bogus'. Valid states: running,
  stopped, …`
- `caco ps: project 'nonexistent' is not configured. Configured
  projects: cacophony, picasso-health, …`

`--project` validates against `config.projects` (cacophony has
no concept of ad-hoc projects today; every project lives in
config).

## Diff summary

- `crates/caco-cli/src/lib.rs`:
  - `dispatch_ps`: added `KNOWN_KINDS` / `KNOWN_STATES` const
    declarations and three upfront validators
    (`--kind`, `--state`, `--project`) before the existing
    `--node` short-circuit.
  - 1 new test:
    `dispatch_ps_validates_kind_state_and_project_filters` —
    source-greps the dispatcher body for the validator error
    wording and the `KNOWN_*` set declarations so a future
    refactor can't silently delete the validators and
    re-introduce the silent-degrade bug.
- `cargo test -p caco-cli --lib
   dispatch_ps_validates_kind_state_and_project_filters`: pass.
- `cargo test-small`: 162 pass.
- `cargo clippy -p caco-cli --tests`: clean (16 pre-existing
  warnings unchanged from baseline).

## Operator-takeaway

The pattern from this bead is a cross-cutting class: any
list-by-X subcommand should validate every filter parameter
against either a known enum (kind/state/level/etc.) or an
existence check (project/agent/node). bd-bc3d7d (caco ls
--kind/--project/--agent all silent) is the same family on
the next surface; the same `KNOWN_*` const + early-return
validator pattern would unblock that bead too.

Longer term, the bead suggests a shared "enum-or-fk filter
validator" helper for the bd subsystem so each subcommand
plugs in rather than hand-rolling the check. That's the
right shape but out of scope here — left as a follow-up
opportunity for whoever picks up bd-bc3d7d.

Out of scope: the operational signal in the bead description
(bd-18f43b 118-agent transition) is unrelated to this fix.
