# Technical-writer summary — document the agent-facing PR-landing contract (no direct gh pr create)

## Goal

Make explicit the agent-facing reintegration/PR contract the operator reinforced:
agents land PRs through the first-party `caco` lifecycle surfaces (`caco agent
reintegrate --mode pr_*`, `caco agent complete`, and the manual `caco pr` tool),
which resolve the project's configured GitHub username/host/forge remote from
config and open the PR there — agents do NOT run `gh pr create` themselves, so the
configured project remote/identity is always respected (the design intent behind
the bd-6e9810 forge-URL push fix). The docs described the gh-wrapper mechanism but
never stated this agent-facing contract explicitly.

## Bead(s)

- No implementation bead — operator-reinforced documentation contract clarification
  (technical-writer maintenance). Related: bd-6e9810 (the implementation wiring fix,
  assigned to caco-dev-msd-1).

## Before state

- `docs/reintegration-policy.md` + `README.md` documented the gh-wrapper resolution
  order and that agents finish through `caco agent complete`/`reintegrate`, but did
  not explicitly state that agents must not run `gh pr create` directly / that the
  caco surfaces own PR creation with project-config identity against the forge
  remote (not the daemon-local mirror).
- `./docs/validate-pages.sh` green (4139 passed).

## After state

- `docs/reintegration-policy.md` "GitHub CLI wrapper reuse" section gains an explicit
  paragraph: agents do not run `gh pr create` themselves; PR creation is owned by the
  first-party lifecycle surfaces which invoke `gh` once through the project wrapper so
  the configured project GitHub identity/host/forge remote publishes the agent PR
  branch and opens the PR — not the daemon-local mirror `origin`.
- `docs/reintegration-policy.html` mirrors the new paragraph; `md-sibling-sha` marker
  refreshed (sha=b9ce4b09f536, in-sync).
- `README.md` reintegration paragraph gains a matching one-sentence contract.
- `./docs/validate-pages.sh` → 4139 passed, 0 warnings, 0 failed.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `docs/reintegration-policy.md`, `docs/reintegration-policy.html`
  (sibling, marker refreshed), `README.md`. Docs-only.
- Tests: n/a; validation via `docs/validate-pages.sh` + `docs/sibling-update.sh`.
- Behavioural delta: documentation only.

## Operator-takeaway

The agent-facing PR-landing contract is now explicit in both the README and the
reintegration-policy doc: agents never hand-roll `gh pr create`; the first-party
caco surfaces own PR creation and always use the project's configured
GitHub identity/host/forge remote from config, so the configured project is
respected even though agent checkouts use a daemon-local mirror `origin`. Once
bd-6e9810 lands the forge-URL push fix, I'll re-verify this section reads correctly
against the new push-destination behavior.
