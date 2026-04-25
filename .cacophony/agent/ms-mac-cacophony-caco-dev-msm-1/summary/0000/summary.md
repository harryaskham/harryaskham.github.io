# Session summary — macOS app install/run shortcuts (bd-a3f852)

## Goal

Make Cacophony.app trivially installable and launchable on a developer
machine via three independent operator surfaces (justfile, caco
actions, flake apps), so neither the operator nor a future agent has
to hand-roll the `nix build` + `cp` + `codesign --force --deep
--sign -` + `open` dance every time.

## Bead(s)

- `bd-a3f852` — Add install/run shortcuts for the macOS app
  (justfile + caco action + flake apps).
- (parent: `bd-6d67e0`; depends on landed `bd-aa8a1a` Nix package.)

## Before state

- Building and installing the app required this hand-typed sequence:
  `nix build .#cacophony-macos-app && rm -rf
  /Applications/Cacophony.app && cp -R
  $(readlink -f result)/Applications/Cacophony.app
  /Applications/ && chmod -R u+w /Applications/Cacophony.app &&
  codesign --force --deep --sign - /Applications/Cacophony.app
  && open -a /Applications/Cacophony.app`.
- Forgetting any step (chmod, codesign) produced confusing
  Gatekeeper / "permission denied" errors on relaunch.

## After state

Three operator-facing surfaces, all wrapping the same dance:

1. **justfile** (developer + CI):
   - `just macos-app-build` — `nix build .#cacophony-macos-app`.
   - `just macos-app-install` — build, copy to `/Applications`,
     chmod, ad-hoc codesign, log version.
   - `just macos-app-run` — install (idempotent), kill any prior
     running instance, `open -a`.
   - `just macos-app-uninstall` — kill + `rm -rf`.

2. **`.cacophony/actions.yaml`** (operator role):
   - `install-macos-app`, `run-macos-app`,
     `uninstall-macos-app` — wrap the matching `just` recipes,
     `roles_permitted: [operator]`, 600s timeout.

3. **flake apps** (zero-checkout dev):
   - `nix run .#cacophony-macos-app` — copies the bundle into
     `~/Library/Caches/com.cacophony.macos/Cacophony.app` (since
     /nix/store paths cannot host a LaunchServices-managed `.app`),
     ad-hoc signs, and `open -W -a` blocks until the app exits.
   - Darwin-only via `lib.optionalAttrs pkgs.stdenv.isDarwin`.

End-to-end verified: uninstall → install → run produced PID 74371
running from `/Applications/Cacophony.app/Contents/MacOS/Cacophony`
on this machine.

## Diff summary

- Files touched:
  - `justfile` — four new `macos-app-*` recipes (~55 lines).
  - `.cacophony/actions.yaml` — three new operator actions.
  - `flake.nix` — `apps.<system>.cacophony-macos-app` runner under
    a Darwin gate, using a `pkgs.writeShellScript` that copies the
    bundle to `~/Library/Caches` before `open`-ing it.
  - `companion/macos/README.md` — new "Operator one-liners"
    section at the top of the build docs.
- Tests: no Rust changes; YAML validated; `just --list` shows the
  new recipes; `nix eval .#apps.aarch64-darwin.cacophony-macos-app`
  resolves; full `uninstall → install → run` cycle exercised
  locally and confirmed running.
- Behavioural delta: zero impact on Linux / non-darwin; on macOS
  the install/run flow drops from a 6-step dance to a single
  command with three equivalent invocation styles.

## Operator-takeaway

`just macos-app-run` is the path to use day-to-day. The flake app
runner (`nix run .#cacophony-macos-app`) is the right pick for
"give me the app from a fresh checkout without polluting
/Applications". `caco actions run install-macos-app` is the right
pick for operator-from-controller scenarios. All three converge on
the same `nix build .#cacophony-macos-app` derivation and the
same ad-hoc `codesign --sign -` step — change the dance in one
place and add a recipe/action that calls into it rather than
hand-rolling another variant.
