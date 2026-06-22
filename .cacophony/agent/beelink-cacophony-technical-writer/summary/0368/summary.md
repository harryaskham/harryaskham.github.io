# Technical-writer summary — cacheless Nix workaround: public-cache form (correction)

## Goal

Document the interim cacheless-Nix workaround correctly across the build docs, and
CORRECT an initial mistake. The ACA Attic cache (`redhill` on
`*.azurecontainerapps.io`) is retired/dead (504/000). The first form documented was
a bare empty `--option substituters ''`; per android-md2-0's empirical ms-dev test
(relayed by caco-ctrl), that fully-cacheless form FAILS FAST on cold-store nodes
because un-buildable fetched derivations (e.g. the Android SDK) can't be source-built
offline. The robust fleet-wide form keeps the public nixpkgs cache and drops only the
dead redhill cache: `--option substituters 'https://cache.nixos.org'`.

## Bead(s)

- No implementation bead — operator/controller-directed documentation correction
  (technical-writer maintenance). Related: peer draft bd-8139f7.

## Before state

- `deploy/aca/README.md` (landed) and the pending AGENTS.md + README build-section
  caveats all used the bare `--option substituters ''` form, which is wrong for
  cold-store builders.

- Reconciled with tui-md2-1's already-landed AGENTS.md section (83cff983d), which I
  am the canonical owner of per ms-dev-2-ctrl: corrected their bare `''` workaround
  in the Implementation-Rules ACA bullet (~L191) to the robust form, made the
  AGENTS.md Development-Workflow entry a short pointer to that canonical note (no
  duplicate/competing section), and corrected the README "Build and Run" caveat and
  the `deploy/aca/README.md` note.
- All three surfaces now document the robust form: `nix --option substituters
  'https://cache.nixos.org' <args…>` (keep the public nixpkgs cache, drop only the
  dead redhill cache), fetching un-buildable derivations like the Android SDK from
  the public cache and only source-building the cacophony-specific ex-redhill paths.
  They explicitly warn that bare `--option substituters ''` is fully cacheless and
  only works on already-warm stores (fails fast on cold-store builders).

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `AGENTS.md`, `README.md`, `deploy/aca/README.md` (the cacheless
  caveat in each, corrected to the public-cache form). No `docs/` HTML siblings.
- Tests: n/a (docs-only); verified no AUTOGEN/previous-summary churn in AGENTS.md.
- Behavioural delta: documentation only.

## Operator-takeaway

The documented cacheless workaround is now the fleet-robust public-cache form
(`--option substituters 'https://cache.nixos.org'`), not bare `''`. Revert these
notes once a self-hosted `atticd` lands on ms-dev/-2/-3. (Separately, the
reintegration gate's nix wrapper hangs on the dead host substituter for
cargo-gated reints — host `/etc/nix/nix.custom.conf` cleanup is the top blocker,
operator-owned; docs-only reints skip the cargo gate and are unaffected.)
