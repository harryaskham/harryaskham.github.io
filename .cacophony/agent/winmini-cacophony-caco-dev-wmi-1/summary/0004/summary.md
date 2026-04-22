# Session summary — Authorization-scopes capability matrix (bd-607960)

## Goal

bd-607960: caco-ctrl (project_controller) reported that the actual
capability boundary of `project_controller` was unclear. SPEC §7.2.1
describes scopes abstractly. This session produces an authoritative,
empirically-grounded capability table operators can read, and pins
the underspecified branches with negative-case tests.

## Bead(s)

- `bd-607960` — Authorization scope 'project_controller' on
  persistents: actual capability boundary is unclear, need test+docs
- (related: bd-012d92 scope-claim plumbing, bd-315545 all_projects,
  bd-00d930 cross_project_bead_permissions, bd-71fbf8
  --authorization-scope CLI flag)

Also touched (investigation only, released back to open):

- `bd-33ca18` — caco msg speak emits both 'speak' AND 'broadcast' —
  could not reproduce; left detailed audit findings on the bead and
  unassigned it for needs-repro from the operator. The local DB never
  duplicates: `handle_msg_speak` writes one row + one feed event; peer
  ingest only materializes MessageBroadcast/MessageSent feed events
  into project_messages, not MessageSpeak.

## Before state

- Failing tests: none. caco-daemon scope coverage: 36 tests across
  worker / project_controller / cluster_controller / lifecycle.
- No docs cheat-sheet for per-scope capabilities. Operators had to
  read `check_agent_scope` to know what a project_controller could do.
- Two enforcement branches were untested: persistent endpoints
  (`/api/v1/persistent/*`), and the `Default deny` arm at the end of
  `check_agent_scope`.

## After state

- Failing tests: none. +7 scope tests (43 total covering the
  enforcement matrix).
- New docs page: `docs/authorization-scopes.md` — capability matrix
  with ✓/✗/▢/◐/◑ legend, derived directly from
  `check_agent_scope`, `check_agent_lifecycle_scope`, and
  `check_project_scoped_endpoint`. Includes the operator-quick-checks
  table the bead reporter asked for.
- New tests pin the persistent-endpoint and default-deny branches so
  the docs cannot drift from enforcement code:
    - worker_scope_denies_persistent_endpoints
    - project_controller_scope_allows_persistent_endpoints
    - cluster_controller_scope_allows_persistent_endpoints
    - worker_scope_denies_other_agent_attach
    - project_controller_scope_allows_other_agent_attach
    - worker_scope_denies_unrecognised_path_default_deny
    - project_controller_scope_denies_unrecognised_path_default_deny

## Diff summary

- Commit: `084c2ea4`
- Files touched:
  - `docs/authorization-scopes.md` (+185, new): capability matrix
  - `crates/caco-daemon/src/lib.rs` (+~75): 7 new tests
- Tests: +7 / -0 / flipped 0
- Behavioural delta: zero. Pure docs + characterisation tests.

## Embedded artefacts

(none — pure docs + tests change)

## Operator-takeaway

`docs/authorization-scopes.md` is now the canonical reference for
"what can scope X do". The matrix is grounded in the actual
enforcement code, and characterisation tests prevent drift. The
follow-up `caco auth check` CLI command suggested by the bead is
intentionally NOT in this commit — it crosses the cli/daemon
boundary and merits its own bead (would need a new
`GET /api/v1/auth/check` endpoint returning the caller's claims plus
a derived allowlist). The bead text mentions this explicitly in the
commit message so the operator can file it whenever convenient.

The bd-33ca18 (speak/broadcast dup) audit was inconclusive and
released back to open with detailed code-audit findings comment-style
appended to the description, plus three open hypotheses (caller-ID
inconsistency on inbox poll vs speak; version drift per bd-5c2a98;
direct_send hooks). Needs operator repro to make further progress.
