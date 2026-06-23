# Session summary — caco-mobile indeterminate-write lesson + PR-mode/triage board-hygiene (caco-dev-s24-0)

## Goal

Capitalize on a context-rich idle session (per Harry's "heavy context = good time to self-improve" directive) by capturing the clearest operational friction I hit this session as durable signal: a profile lesson for future phone/light-dev workers plus a draft bead for the underlying product gap. The session itself was all source-only board-hygiene and PR-mode analysis from the S24 phone node (no code lands until this profile-doc change).

## Bead(s)

- `bd-de38f0` — caco bd update --append-description / create can return a transport error on an indeterminate write that actually persisted; naive retry duplicates content (draft, filed via reflect-session).
- Context beads from the broader session (not landed by me): `bd-6e9810` (PR forge-push blocker, root-caused + handed to caco-dev-msd-1), `bd-3a9fb8` (caco pr bypasses project gh wrapper, filed), `bd-7ca67b` (low-power client-only umbrella triage completed).

## Before state

- Failing tests: none (no compiler-backed change).
- `.cacophony/profiles/caco-mobile.md` "Tooling reliability on constrained nodes" documented MCP flaps, large-payload bounded queries, and beads_proxy_unavailable retries — but NOT the indeterminate-write hazard.
- Context: hit the indeterminate-write hazard twice this session (bd-6e9810 append 503'd-but-persisted then a retry duplicated the block; bd-7ca67b append transport-errored-but-persisted), and had to manually dedup once.

## After state

- Failing tests: none.
- `.cacophony/profiles/caco-mobile.md` now carries an additive "Indeterminate writes (verify before retry)" bullet: on a transient 5xx/transport error during a caco bd mutation, re-read and check for your content marker before retrying, because append/create may have already persisted.
- Context: future caco-mobile workers get the verify-before-retry discipline inline; the product-side dedup/idempotency gap is tracked as the bd-de38f0 draft.

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt (single profile-doc commit).
- Files touched: `.cacophony/profiles/caco-mobile.md` (one additive bullet in the "Tooling reliability on constrained nodes" section).
- Tests: +0 / -0 / flipped 0 (Markdown-only; verified by inspection + `git diff --check`).
- Behavioural delta: documentation-only; no runtime behavior change. Adds operator-observed guidance.

## Operator-takeaway

On constrained nodes a `caco bd` write that returns a transport 5xx may have already landed — the dangerous reflex is to retry, which duplicates the append or files a duplicate bead. The durable fix (idempotency / explicit indeterminate_update status) is bd-de38f0; until then, verify-before-retry is the rule, now documented in caco-mobile so the next phone worker does not relearn it the hard way.
