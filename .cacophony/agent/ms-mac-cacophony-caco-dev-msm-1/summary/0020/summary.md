# Session 0020 — bd-d76232 (caco agent introspect --show env)

## Goal

Slice 3 of the bd-5d2d83 introspect family. `--show env` reads the
composed `Profile.env` (HashMap<String,String>) and renders sorted
`KEY=VALUE` pairs. Default-on redaction of likely-secret keys
(TOKEN/SECRET/PASSWORD/KEY substring match, case-insensitive); the
`--reveal-secrets` flag is the explicit escape hatch.

Self-filed follow-up.

## Bead(s)

- **bd-d76232** — primary (self-filed bd-5d2d83 follow-up).
- **bd-7d7bbd** — filed retro: `dispatch_operator_actions_list`
  defined twice on main (bd-6b7b30 vs bd-8fe920 reintegrated in
  parallel). Fixed inline.

## Before state

- `caco agent introspect --show env` returned `not yet implemented`.
- `--show profile` (bd-5d2d83) and `--show hooks` (bd-4b2a82) worked.
- Main was broken: `dispatch_operator_actions_list` had two
  implementations (3-arg direct-URL from bd-8fe920 vs 2-arg
  exec-caco-bd-list from bd-6b7b30) both registered as
  `operator-actions list` arms — `cargo check -p caco-cli` failed.

## After state

- `AGENT_INTROSPECT_ARGS` gains `--reveal-secrets`. `--show`'s
  summary updated to enumerate `profile|hooks|env`.
- `dispatch_agent_introspect` signature gains `reveal_secrets: bool`
  parameter. Match accepts `profile|hooks|env`; other values still
  error explicitly with the supported list.
- New `is_secret_env_key(key) -> bool` helper. Case-insensitive
  substring match against TOKEN / SECRET / PASSWORD / KEY. False
  positives accepted by design — `--reveal-secrets` is the escape
  hatch.
- `--show env` rendering:
  - text: sorted `KEY=VALUE` lines, banner indicates redaction state
    ("env (secrets redacted; --reveal-secrets to show):" vs
    "env (--reveal-secrets ON; do not paste into bug reports):").
  - json: `{ agent_id, profile_chain, reveal_secrets, env: {...} }`.

## Inline broken-on-main absorbed (bd-7d7bbd filed)

- Removed the duplicate `dispatch_operator_actions_list` (bd-6b7b30,
  2-arg, ~100 lines) keeping the bd-8fe920 3-arg version which uses
  the daemon URL directly. Removed the corresponding duplicate arm
  in the dispatcher. Filed bd-7d7bbd retroactively for visibility.

## Tests

- New: `is_secret_env_key_matches_common_patterns` — locks
  case-insensitive matching of GH_TOKEN / DB_PASSWORD / API_KEY /
  AWS_SECRET_ACCESS_KEY etc. and ensures PATH / USER /
  CACO_PROJECT / NODE_NAME don't trigger.
- New: `agent_introspect_help_json_advertises_reveal_secrets` —
  locks help-json contract for MCP discoverability.
- Updated: `agent_introspect_rejects_unsupported_show_section` now
  uses `tools` (since `hooks` is now supported).

## Validation

- `cargo test -p caco-cli --lib agent_introspect`: 3/3 PASS
  (rejects, advertises_id_and_show, advertises_reveal_secrets).
- `cargo test -p caco-cli --lib is_secret_env_key`: 1/1 PASS.
- `cargo test-small`: 4257+ tests across 8 binaries, 0 failures.
- `cargo clippy -p caco-cli -p caco-daemon -p caco-beads -p caco-web
  -p caco-profile --all-targets -- -D warnings`: clean.

## Diff summary

```
crates/caco-cli/src/lib.rs                       | ~+150 / -120
.cacophony/agent/.../summary/0020                | (new)
```

## Operator-takeaway

```bash
caco agent introspect --id <id> --show env
# env (secrets redacted; --reveal-secrets to show):
#   CACO_PROJECT=cacophony
#   GH_TOKEN=****
#   PATH=/usr/local/bin:...

caco agent introspect --id <id> --show env --reveal-secrets
# env (--reveal-secrets ON; do not paste into bug reports):
#   CACO_PROJECT=cacophony
#   GH_TOKEN=ghp_...
#   PATH=/usr/local/bin:...

caco agent introspect --id <id> --show env --json --reveal-secrets \
  | jq '.env | to_entries | map(select(.key | startswith("CACO_")))'
```

`--show tools` is the only remaining unwired section. File when
needed (would enumerate `composed.mcp_servers` per phase + their
provided tool sets).

## Coordination

- Spoke claim with planned scope.
- Filed bd-7d7bbd retro for the `dispatch_operator_actions_list`
  dup so bd-6b7b30 / bd-8fe920 reintegrators know what happened.
- Will speak completion + reintegrate.

## Notes for next time

- Three sequential follow-ups to my own bd-5d2d83 (hooks, env, +
  rebased dedup fix). Pattern: single-author surface with growing
  feature set lets me reuse helpers (load_composed_profile_for_…,
  derive_hooks_attribution, is_secret_env_key) and ship faster as
  the surface matures.
- The broken-on-main dup was reintegrated by parallel agents on
  the same operator-action surface. Nothing prevents two beads
  ('caco operator-actions list') with different shape from
  reintegrating in any order. Worth thinking about a pre-merge
  symbol-collision check; file as a future bead if it recurs.
