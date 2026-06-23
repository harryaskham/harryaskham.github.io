# Technical-writer review summary

## Goal

Document the new per-profile reintegration.backend override (bd-259349), the
mechanism that enables per-agent PR-mode opt-in without flipping a whole project.

## Bead(s)

- `bd-c63005` — technical-writer documentation maintenance.
- Documents bd-259349 (per-profile reintegration.backend override).

## Before state

- The per-profile `reintegration.backend` override was undocumented in AGENTS.md, reintegration-policy.md/.html (only the project-level `integration.backend` was documented). This is the flip mechanism gated item.

## After state

- AGENTS.md:196 + reintegration-policy.md (new resolution rule 6) + .html now document: a profile may pin `reintegration.backend` (`local_merge`|`pull_request`) in frontmatter, taking precedence over the project `integration.backend` for that agent only (else config-profile fallback, else project backend), enabling per-agent PR-mode opt-in without flipping the project — and the inverse (a release profile pinning `local_merge` under a `pull_request` project default). Because the backend (not the mode) is the per-project gate, a profile pinning `local_merge` stays direct under a `pull_request` project default.
- Verified against caco-profile/src/model.rs (ReintegrationDefaults.backend) + caco-cli resolve_agent_reintegration_policy precedence. Sibling marker refreshed; validate-pages passed.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `AGENTS.md`, `docs/reintegration-policy.md`, `docs/reintegration-policy.html`.
- Behavioural delta: documentation only.

## Operator-takeaway

The flip mechanism (per-agent PR backend opt-in) is now documented. cacophony's
own config is still local_merge — when it flips, document the live posture then.
