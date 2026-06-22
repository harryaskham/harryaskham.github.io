# Session summary — AGENTS.md: ACA Attic Nix cache is now RETIRED (operator decision reversal)

## Goal

Reflect Harry's NEW operator decision in the canonical coordination doc: the ACA-hosted Attic Nix binary cache is now actually RETIRED (ACA settings changes made it untenable), reversing the earlier "Attic cache is active / not-retired" clarification. Record the interim workaround and the future local-atticd plan so agents don't try to use the dead cache.

## Bead(s)

- No bead — operator-flagged directly (Harry, ms-mac:cacophony:harryaskham): "the ACA cache is now actually retired (new decision; ACA settings changes made it untenable). On other nodes ... nix --option substituters '' ... eventually atticd running locally on ms-dev, ms-dev-2, ms-dev-3, but for now we have no attic cache."

## Before state

- AGENTS.md ACA bullet (line ~191) said the Attic Nix binary cache hosted on ACA "is NOT retired, and remains in active use — do not remove, disable, or stop using" it (a prior clarification landed by another agent, which my own duplicate edit had been discarded in favor of).
- That is now wrong per Harry's reversal.

## After state

- The bullet now states the ACA-hosted Attic Nix binary cache is ALSO retired (operator decision; ACA settings made it untenable), there is currently NO Attic binary cache, the interim workaround is `nix --option substituters ''` (so nix does not fail/hang on the dead substituter), and a local `atticd` is planned for ms-dev / ms-dev-2 / ms-dev-3 — until then no shared Attic cache, and do not re-add the retired ACA Attic cache as a substituter.

## Diff summary

- Code/content commit: landed squash SHA from the reintegration receipt.
- Files touched: `AGENTS.md` (one bullet reversed: Attic cache active -> retired + workaround + future plan).
- Tests: none (docs-only).
- Behavioral delta: documentation only; no code change. (Repo grep confirms the Attic substituter is operator/system-level Nix config, not referenced in-repo, so no build-config change is needed.)

## Embedded artefacts

None.

## Operator-takeaway

The ACA Attic Nix binary cache is dead as of this operator decision — do NOT rely on it or re-add it as a substituter. Until local atticd stands up on ms-dev/ms-dev-2/ms-dev-3, run nix with `nix --option substituters ''` to avoid hanging/failing on the retired cache. This reverses the prior "Attic cache is active" doc note.
