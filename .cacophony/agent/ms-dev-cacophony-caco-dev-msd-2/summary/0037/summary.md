# Session summary — bd-a7af61 (docs: cacheless nix workaround correction)

## Change
AGENTS.md:191 — corrected the cacheless nix workaround from bare `nix --option substituters ''`
to the robust `nix --option substituters 'https://cache.nixos.org'` variant (keep the public
nixpkgs cache, exclude only the dead redhill/ACA substituter), per ms-dev-2-ctrl + caco-tui-md2-0
corrections grounded in android-md2-0's empirical ms-dev test. Bare '' fails fast on COLD-store
builders (un-buildable fetched derivations like the Android SDK can't be source-built offline);
bare '' only works on warm stores. Docs-prose only; repo build-config remains host-only no-op.

## Land posture
Docs-only / zero-Rust → auto-skips the reintegration gate (bd-45114c). HELD pending the
fleet-wide GATE-CLEAR (ctrl: the daemon test-queue nix wrapper hangs on the dead ACA substituter
in host /etc/nix/nix.custom.conf). Confirming with ctrl whether a gate-auto-skipping docs reint
is safe during the hold, else waiting for GATE-CLEAR.

## Diff
One file: AGENTS.md (the cacheless workaround line, ~191).
