# Summary 0019 — bd-29bf2b: merge-queue mixin auto-runs broken-on-main detection (re-land)

## Context

Summary 0018 attempted this fix bundled with a fixture sweep + a
clippy fix. During rebase, both the fixture sweep (msm-3 landed
the parent_bead_id sweep + a23a7e occurrence_count sweep on
d0495854) and the clippy fix (now in their version) became no-ops.
The merge-queue.md frontmatter edit ALSO got dropped in the rebase
resolution because I git-rebase-continued an empty preimage. Net:
the previous reintegrate produced "reconciled: agent work already
on main" — but the merge-queue.md change actually wasn't there.

This summary re-lands the merge-queue.md frontmatter edit standalone.

## Bead

bd-29bf2b (P2, feature, claimed).

## Change (sole edit, idempotent re-apply)

`.cacophony/profiles/merge-queue.md` frontmatter — append:
```yaml
hook_mixins:
  - fast-test-gate
reintegration_checks:
  rebase_check: true
  test_command: cargo test-small
  check_command: cargo check --workspace --tests
  clippy: true
  abort_on_failure: true
```

Composes the existing fast-test-gate hook (loaded via
`crates/caco-profile/src/hook_mixins.rs::fast_test_gate()`) so
every persistent agent (per cacophony_persistent.yaml: 11/11
caco-dev-* agents have merge-queue mixin) gets the gate
automatically.

What runs at every reintegration for any merge-queue agent:
1. **Rebase freshness** — fail fast if branch is behind origin/main.
2. **`cargo test-small`** — ~35s warm fast preflight.
3. **`cargo check --workspace --tests`** — broken-on-main detection
   (catches all 4 wave classes from this session).
4. **`cargo clippy --workspace`** — lint hygiene.

`abort_on_failure: true` blocks reintegration on any failure.

## Why this matters

4 broken-on-main waves THIS session forced peer agents into
30+ minutes of fixture-backfill repair each:
- bd-bce6ea: tmux_history_limit/size on AgentDisplayState (~75 sites)
- bd-ab1c38 wave 1: tmux_history_* on AttachMetadata (10 sites)
- bd-ab1c38 wave 2: parent_bead_id on Bead (~150 sites)
- bd-ab1c38 wave 3: dispatch_agent_logs since param (3 sites)

Cost of gate: ~30-60s warm-cache cargo check per reintegrate.
Decisively favorable vs hours of aggregate peer repair per wave.

## Verification

- `cargo test -p caco-profile --lib` — 295/295 (validates new
  frontmatter parses correctly through ProfileFrontmatter +
  ReintegrationChecks).
- The fixture sweep + clippy fix from summary 0018 ARE on main
  already (msm-3's d0495854 + their absorbed cleanup), so the
  workspace was already green by the time this re-land happened —
  the gate would PASS today on a green tree.

## Operational note

Once this lands, my own NEXT reintegrate will be the first one
gated by the new check. If it passes I know the gate is alive;
if it false-alarms I'll see it before the broader fleet does.

## Next

Reintegrate direct, close bd-29bf2b, idle.
