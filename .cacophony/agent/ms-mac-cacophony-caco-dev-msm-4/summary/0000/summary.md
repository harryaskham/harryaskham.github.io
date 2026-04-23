# bd-8a56ce — profile build-time lint covers all canonical sets

## Goal
Close the remaining gaps in the bd-8a56ce profile-frontmatter
build-time lint so a typo in any canonical-set field (not just
hook_mixins / mcp_servers / permission_mode) fails the build
loudly with a precise diagnostic.

## Bead(s)
- bd-8a56ce (P2 feature). Primary item (hook_mixins) and two of
  three secondary items (mcp_servers, permission_mode) had
  already landed via existing scaffolding in
  crates/caco-daemon/build.rs (bd-aac755 + earlier bd-8a56ce
  slices). This slice adds the last two: authorization.scope
  and reintegration.mode / allowed_modes.

## Before state
- crates/caco-daemon/build.rs validated `hook_mixins`,
  `mcp_servers`, and `permission_mode` at compile time per
  profile in `.cacophony/profiles/*.md`.
- `authorization.scope` was caught only at runtime by serde's
  enum rename, which surfaces less clearly than a build error
  pointing at file + value.
- `reintegration.mode` and `reintegration.allowed_modes` were
  not validated at build time at all; bad values manifested as
  reintegration failures later in the agent lifecycle.

## After state
- New `KNOWN_AUTH_SCOPES` and `KNOWN_REINTEGRATION_MODES`
  constants in build.rs synced with
  caco-profile::canonical lists.
- New `extract_nested_scalar_field` and
  `extract_nested_list_field` helpers handle the
  `parent:\n  child: value` and
  `parent:\n  child:\n    - item` block-form YAML shapes that
  authorization/reintegration use.
- New `check_reintegration_mode_token` splits on commas so the
  `direct,recorded` (bd-d48494) composition is accepted as long
  as every component is in the known list.
- Two new lint passes per profile in build.rs:
  - `authorization.scope` against KNOWN_AUTH_SCOPES.
  - `reintegration.mode` + every entry of
    `reintegration.allowed_modes` via the comma-tolerant
    token check.
- Two new sync tests in
  crates/caco-daemon/src/agent/tests.rs:
  - `known_auth_scopes_are_in_sync_with_build_rs`.
  - `known_reintegration_modes_are_in_sync_with_build_rs`.
  Each fails loudly with a message naming both files to update
  when the canonical list and build.rs sync table drift.

## Diff summary
- crates/caco-daemon/build.rs (+~150):
  - Two new const tables, two helpers, one token validator, two
    new lint loops per profile.
- crates/caco-daemon/src/agent/tests.rs (+~50):
  - Two new sync tests mirroring the existing
    `_hook_mixins_in_sync` / `_mcp_servers_in_sync` /
    `_permission_modes_in_sync` patterns.

## Tests
- `cargo build -p caco-daemon` clean (the build itself is the
  primary acceptance: every current profile in
  `.cacophony/profiles/` passes the new lints).
- `cargo clippy -p caco-daemon --all-targets -- -D warnings`
  clean.
- 2/2 new sync tests pass.

## Operator-takeaway
After binary roll, a profile that mistypes
`authorization.scope: workr` or
`reintegration.mode: direct,recordedz` will fail
`cargo build -p caco-daemon` with a `bd-8a56ce: profile … sets
unknown reintegration.mode component …` panic naming the file
and the offending value. The `recorded` modifier composition
(`direct,recorded`) is preserved. No runtime change.
