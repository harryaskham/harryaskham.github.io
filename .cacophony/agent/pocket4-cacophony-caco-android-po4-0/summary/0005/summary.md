# Session summary — Complete ACA Attic cache RETIRED docs (bd-79bf91) + close obsolete sibling

## Goal

Operator-confirmed board-burndown docs task: Harry retired the ACA-hosted Attic Nix binary cache (2026-06-22). The bead asked to flip the now-incorrect "cache is NOT retired / do not remove" caveat to "retired" across AGENTS.md/SPEC.md/docs, document the interim cacheless override, and note the planned local atticd. On grounding in current main I found the core caveat-flip + the settled-multi-cache resolution were ALREADY landed by recent commits; the only unmet item was the forward-looking "planned local atticd" note, which I added. Also closing the obsolete sibling bead so the two don't land contradictory prose.

## Bead(s)

- `bd-79bf91` — Docs: ACA Attic Nix binary cache is RETIRED — update AGENTS.md/SPEC/docs + interim override + planned local atticd (claimed + completed)
- `bd-3ded41` — Docs: clarify ACA-legacy framing is about compute nodes (obsolete sibling — closed as superseded-by-bd-79bf91; its "cache still active" premise was reversed by the retirement)

## Before state

- Failing tests: none (docs-only).
- AGENTS.md:191 + :233 already stated the Attic cache is retired + the settled per-node multi-cache resolution (landed by recent docs commits incl. d3937f9d1 "cacheless final settled framing", c6de3782d). The stale "is NOT retired / do not remove / remains in active use" bd-7dcbdc caveat was already gone (0 grep matches in AGENTS.md + SPEC.md). docs/ audited clean. The ONLY gap: the bead-title "planned local atticd (ms-dev/ms-dev-2/ms-dev-3)" was not documented anywhere.
- bd-3ded41 open/unassigned with a now-reversed premise (document cache as active) — risk of a future contradictory edit.

## After state

- Failing tests: none.
- AGENTS.md:191 canonical ACA-cache note now also documents the planned local `atticd` on ms-dev/ms-dev-2/ms-dev-3 (not yet running) — completing the bead's stated scope. No other docs change needed (already correct).
- bd-3ded41 closed as superseded-by-bd-79bf91.

## Diff summary

- Code/content commit: this reintegration's squash SHA from the receipt.
- Files touched: `AGENTS.md` (one-sentence addition to the canonical ACA-cache note). No code; non-caco-dev echo reint-gate (no cargo).
- Tests: +0 / -0. Behavioural delta: none (docs accuracy only).

## Operator-takeaway

The ACA Attic binary cache is retired; the fleet's settled state is the standard full multi-cache (cache.nixos.org + 5 cachix caches) restored on ms-dev/ms-dev-2 via nixos-rebuild — NOT a re-add of the Attic cache — with a local atticd planned as the eventual durable first-party cache. The bd-7dcbdc "cache is active, do not remove" caveat is fully reversed across the docs. Grounding in current main before editing avoided redundant work (the core flip was already landed) — the only real remaining work was the atticd note + retiring the contradictory sibling bead.
