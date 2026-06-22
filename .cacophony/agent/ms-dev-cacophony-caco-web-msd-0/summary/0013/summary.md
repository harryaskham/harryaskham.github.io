# Session summary — extend ACA-legacy/Attic-cache clarification to PRODUCTION-ROLLOUT.md

## Goal
Apply Harry's clarification (the ACA-legacy notes are about the old `caco-aca`
compute nodes, NOT the actively-used Attic Nix binary cache hosted on ACA) to the
deploy doc that still lacked it.

## Bead(s)
- None (operator-directed documentation clarification from Harry's direct message).

## Before state
- Failing tests: none (docs-only).
- AGENTS.md and README.md ALREADY carry this clarification on current main (landed
  independently while I was preparing the same edit — my AGENTS/README edits were
  redundant and were resolved to main's version during rebase).
- deploy/aks/PRODUCTION-ROLLOUT.md:2203 still said "ACA and caco-aks are both fully
  torn down ... ACA is now legacy/reference material only" with no Attic-cache
  exception — readable as the cache being torn down too.

## After state
- Failing tests: none.
- PRODUCTION-ROLLOUT.md now states the teardown/legacy status covers the ACA
  compute nodes only and that the Attic Nix binary cache hosted on ACA remains a
  separate, actively-used substituter that is NOT torn down — matching the
  AGENTS.md/README.md clarification already on main.

## Diff summary
- Code commit: final landed squash SHA from the reintegration receipt.
- Files touched: deploy/aks/PRODUCTION-ROLLOUT.md (1 sentence). No Rust, no code.
- Behavioural delta: documentation consistency; the third ACA-legacy note now
  matches the two canonical ones already on main.

## Embedded artefacts
- (none — text-only clarification.)

## Operator-takeaway
Another agent/Harry already landed the AGENTS.md+README.md clarification; this just
brings the third ACA-legacy note (a deploy doc) into consistency so no reader
concludes the active Attic cache was torn down. Landed via the docs-only
--skip-hooks fast path.
