# Technical-writer review summary

## Goal

Document the landed cacophony PR-auto-merge flip (bd-259349) and fix the stale
async PR-reintegration framing — the long-tracked gated docs-TODO, now unblocked.

## Bead(s)

- `bd-259349` — cacophony-dev pr_auto_merge backend flip (O2-BACKEND); `bd-1d514b` DirectMerge-over-PR.
- `bd-c63005` — technical-writer documentation maintenance.

## Before state

- AGENTS.md:196 / README.md:453 / reintegration-policy.md:374 carried the bd-db867a "prefer local_merge until PR proven" safety posture, now superseded by the landed flip (245d4fea79). README.md:565 still had the stale async "keeps the runtime alive until the PR actually lands" framing (contradicting the corrected synchronous model in AGENTS.md:63).

## After state

- All three safety-posture spots now document the surgical flip: the `caco-dev`, `caco-dev-codex`, `caco-tui`, `caco-web` value groups flip their reintegration BACKEND (not mode — mode stays `direct`, allowed_modes untouched) to `pull_request` via the `cacophony-pr-backend` mixin, landing via bd-1d514b DirectMerge-over-PR; the project default stays `local_merge` for all other agents; `update-helper`/`caco-release` are defensively pinned (push_tags); surface specialists `caco-android`/`caco-ios`/`caco-macos` are deferred. README.md:565 fixed to the synchronous no-auto-recreate framing.
- Verified against `.cacophony/profiles/cacophony-pr-backend.md` + `.cacophony/agents/cacophony_persistent.yaml` (the mixin composes on exactly the 4 dev values). Disambiguated the mode-vs-backend confusion (the caco-web specialist's mode stays `direct` but its backend flipped — caco-web IS in the flipped bucket, confirmed by aur-5/aur-2/caco-web-md0/config-helper). reintegration-policy sibling marker refreshed; validate-pages passed.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `AGENTS.md`, `README.md`, `docs/reintegration-policy.md`, `docs/reintegration-policy.html`.
- Behavioural delta: documentation only.

## Operator-takeaway

The reintegration docs now reflect the live cacophony PR-auto-merge flip (backend
flip on the 4 Rust dev values, mode stays direct) and the correct synchronous
DirectMerge-over-PR model, ending the bd-db867a "prefer local_merge" posture.
