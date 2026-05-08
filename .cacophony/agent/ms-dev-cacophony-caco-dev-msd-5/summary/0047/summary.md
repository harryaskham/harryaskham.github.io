# Session summary — GitHub SSH port 443 fallback

## Goal

Unblock direct reintegration and first-party rebase/retry flows when daemon-managed canonical checkouts for GitHub projects cannot reach `github.com:22`, without manually editing canonical checkouts or operator SSH configuration.

## Bead(s)

- `bd-bf5b9c` — GitHub SSH port 22 timeout blocks direct reintegration fetch

## Before state

- Failing path: multiple workers (`bd-5f8717`, `bd-c7864d`) had validated local changes but direct reintegration or `caco agent rebase` failed before publish because the daemon canonical checkout fetched `ssh://git@github.com/harryaskham/cacophony.git` via `github.com:22` and timed out.
- Evidence: reintegration receipts showed `rejected_no_publish`, `checkout_mutated=false`, no published refs, and Git follow-up errors saying the remote repository could not be read after the port-22 timeout.
- Constraint: do not edit `$CACOPHONY_DIR/daemon/checkouts/<project>` or global/operator SSH config by hand; the fix needed to be repo-owned and deterministic.

## After state

- GitHub SSH remotes with configured project identities now generate a daemon-managed `core.sshCommand` / `GIT_SSH_COMMAND` that adds `-o HostName=ssh.github.com -o Port=443` for default-port GitHub SSH URLs.
- Non-GitHub remotes keep their existing SSH command behavior.
- Existing canonical checkouts can accept a repo-owned SSH-command hash upgrade without being fenced as destructive checkout identity drift, while still preserving the normal drift guard for all other checkout-defining parameters.
- Focused validation passed: `tj-0807c0dd` (`cargo test -p caco-daemon bf5b9c -- --nocapture`) ran 3 tests successfully.

## Diff summary

- Commits: `16c486a0bc` (code/docs fix; summary artefact committed separately and intentionally not listed as a behavioural commit)
- Files touched: `crates/caco-daemon/src/checkout.rs`, `SPEC.md`, `README.md`, `AGENTS.md`
- Tests: +3 focused daemon tests for GitHub SSH command routing, non-GitHub preservation, and existing-checkout SSH-command upgrade convergence.
- Behavioural delta: Cacophony now owns the GitHub SSH port-443 fallback in checkout/reintegration plumbing instead of relying on ambient SSH config or manual canonical checkout repair.

## Operator-takeaway

The repeated bd-5f8717 / bd-c7864d pre-publish failures were caused by daemon-owned Git fetches still using `github.com:22`; this change routes configured GitHub project identity SSH through `ssh.github.com:443` and lets live canonical checkouts converge to that safer command through normal first-party lifecycle paths.
