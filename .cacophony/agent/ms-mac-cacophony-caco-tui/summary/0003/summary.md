# Session summary — immediate bead visibility after web creation

## Goal

Fix a web-app UX bug where creating a bead succeeded server-side but the new bead did not appear in the bead table until a later snapshot refresh. The aim was to make bead creation feel immediate and local while preserving the normal snapshot-driven update model for everything else.

## Bead(s)

- `bd-e425b4` — Fix bead creation in web app to immediately appear in table

## Before state

- Creating a bead from the web UI showed success, but the new bead only appeared after the next `loadSnapshot()` cycle.
- The create flows (`createBead()` and `quickBeadDirect()`) always depended on a follow-up snapshot reload instead of integrating the just-created server response into local UI state.
- Under active filters, a just-created bead could still remain invisible even if it had already been returned by the create endpoint.

## After state

- The web UI now merges the created bead response directly into `state.beads` as soon as the create request succeeds.
- Newly created beads temporarily bypass current bead-table filters so they visibly appear immediately in the table instead of waiting for the next server snapshot.
- Optimistic create no longer fabricates partial `bead_stats` when the page has not yet hydrated snapshot stats; it only patches those counts when real snapshot stats are already present.
- Static web tests now pin the optimistic create path so the helper and filter-bypass contract do not silently regress.

## Diff summary

- Commits: `9a639fb8`
- Files touched: `crates/caco-web/static/app.js`, `crates/caco-web/src/tests.rs`
- Tests: `cargo test -p caco-web --lib`, `node -c crates/caco-web/static/app.js`, `cargo test-small`, `cargo check --workspace --tests`
- Behavioural delta: after bead creation succeeds, the bead table updates immediately in-browser using the returned server payload, and the created bead is visible even when current filters would otherwise hide it.

## Operator-takeaway

This was a small but high-leverage web UX fix: bead creation now feels instant instead of eventually consistent. The important detail is that the local optimistic patch respects the existing snapshot model instead of replacing it, so the UI gets faster feedback without inventing a second long-lived source of truth.
