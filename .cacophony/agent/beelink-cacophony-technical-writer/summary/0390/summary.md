# Technical-writer review summary

## Goal

Add the new `caco project reintegrate` / `caco project land` command (bd-1d2935)
to the Extended CLI Reference, which had the project table but not the new verb.

## Bead(s)

- `bd-c63005` — technical-writer documentation maintenance.
- Documents bd-1d2935 part-3b (caco project reintegrate --checkout / project land).

## Before state

- `docs/cli-extended.html` `caco project` table listed list/show/status/create but not the new `reintegrate`/`land` command. (AGENTS.md:197 was already updated by the implementing dev; the bd-6114dd agent-subset note at AGENTS.md:198 stays accurate — this command is for manual non-agent checkouts, a different case.)

## After state

- The `caco project` table now includes `caco project reintegrate --checkout <path> --reason <r>` (land a human/config-only edit from a manual project checkout via the same publish-or-refuse direct-reintegration machinery; gated on clean worktree + non-agent branch + audited --reason; agent-branch operators routed to `caco agent reintegrate`) and its `caco project land` alias.
- Verified against caco-cli/src/lib.rs PROJECT_REINTEGRATE_ARGS + dispatch_project_reintegrate (bd-1d2935). validate-pages passed.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `docs/cli-extended.html`.
- Behavioural delta: documentation only.

## Operator-takeaway

The CLI reference now lists the first-party manual-checkout land path, so
operators landing config-only edits do not resort to agent cherry-pick proxies.
