# PR-mode reintegrate warns on local-mirror push (bd-1ef80b)

## Goal

Stop `pr_review` / `pr_auto_merge` reintegration paths from
silently no-op'ing when the configured `remote` is the daemon's
local checkout mirror (not a real forge URL). Operators were
reading the success message ("agent branch pushed; PR pending forge
integration") as "PR will appear shortly" and discovering hours
later that nothing reached GitHub.

## Bead(s)

- `bd-1ef80b` — PR-mode reintegration succeeds locally but never
  reaches forge (no GH PR opened).

## Before state

`reintegrate_pr_review` and `reintegrate_pr_auto_merge` in
`crates/caco-daemon/src/reintegration.rs` push the agent branch
to `req.remote` and unconditionally return:

> "agent branch pushed; PR for review into main pending forge
> integration"

When the configured remote is the daemon's local mirror (e.g.
`/Users/harryaskham/.cacophony/daemon/checkouts/cacophony`), the
push lands on the mirror only. `gh pr create` is never invoked
in either path. Operators see "success" + "PR pending" and assume
the PR will appear.

The bead acceptance offered two valid paths:
1. complete the forge integration so PR mode actually opens a GH PR;
2. downgrade pr_review to a no-op-with-warning until it works.

This change implements (2) by classifying the remote and emitting
an explicit warning when it is not a forge URL. Path (1) — calling
`open_or_update_pr` from the PR modes — is a follow-up; the wiring
already exists for the direct+create-pr path but threading the
forge-base argument through PR-mode requires API surface changes
that are best done together with bd-c1c272 (forge-integration
epic).

## After state

New helper `remote_url_is_forge(&str) -> bool` recognises
`https://`, `http://`, `git@`, `ssh://`, `git://` (with leading
whitespace tolerated). Anything else — including absolute paths,
relative paths, and `file://` URLs — is treated as a local mirror.

Both `reintegrate_pr_review` and `reintegrate_pr_auto_merge` now
inspect the resolved remote URL and return a loud warning message
when the push only reached a local mirror:

> "bd-1ef80b: agent branch pushed to LOCAL MIRROR only (origin =
> /Users/.../checkouts/cacophony); NO forge PR was opened. Use
> direct,recorded for now, or wire a forge-URL remote (e.g. an
> https://github.com/... origin) before invoking pr_review again."

The forge-URL happy path message is unchanged so a forge-wired
remote still reads as "pending forge integration" until the
follow-up wiring lands.

## Diff summary

- `crates/caco-daemon/src/reintegration.rs`:
  - New `pub(crate) fn remote_url_is_forge` helper next to
    `resolve_local_remote_path`. Doc-comment cites the
    operator-confusion failure mode.
  - `reintegrate_pr_review` branches its message on
    `remote_url_is_forge` of the resolved remote URL.
  - `reintegrate_pr_auto_merge` does the same for its
    success message.
  - 4 new unit tests:
    `remote_url_is_forge_https_github`,
    `remote_url_is_forge_ssh_github`,
    `remote_url_is_forge_rejects_local_mirror`,
    `remote_url_is_forge_trims_whitespace`.

## Operator-takeaway

PR-mode reintegrations against the daemon's local mirror now
explicitly say "LOCAL MIRROR only — NO forge PR opened" and tell
the operator to use direct,recorded or wire a forge URL.
Forge-URL remotes still show the existing "pending forge
integration" message; full forge integration remains follow-up
work tracked by the PR-modes acceptance criteria 1 (the forge
wiring path) of this bead, which we explicitly did NOT take in
this change. Acceptance criterion 2 (downgrade to
no-op-with-warning) is satisfied.
