## Goal
Teach caco-web-observe to find a usable Chromium binary in managed
Nix shells where Playwright's bundled `/opt/google/chrome/chrome`
path does not exist.

## Bead(s)
- bd-412e5b — Let caco-web-observe use system Chromium in managed Nix shells.

## Before state
The helper unconditionally invoked `npx @playwright/cli`, which
looks for Chrome at `/opt/google/chrome/chrome`. On NixOS that path
is empty so the helper failed before exercising the workspace; the
operator had to fall back to a manual headless Chromium/CDP harness.

## After state
- Detects a usable Chromium with this priority:
  1. `CACO_WEB_OBSERVE_CHROMIUM` (explicit override)
  2. `PLAYWRIGHT_CHROMIUM_EXECUTABLE_PATH` (already set)
  3. PATH search for `chromium`, `chromium-browser`, `google-chrome`,
     `google-chrome-stable`.
- Injects `PLAYWRIGHT_CHROMIUM_EXECUTABLE_PATH` (and `CHROME_PATH`,
  `CHROMIUM_PATH` for older CLI versions) into the spawned
  playwright command so the bundled binary path is bypassed.
- If nothing is found AND the helper is running inside a Nix shell
  (detected via `IN_NIX_SHELL`/`NIX_BUILD_TOP`/`NIX_PROFILES`), emits
  a clear remediation hint to both the log and stderr — better than
  Playwright's cryptic "executable doesn't exist".
- 4 new unit tests (serial-marked because they mutate env):
  finds-on-PATH, explicit-override priority, missing → None,
  in_nix_shell smoke detection.

## Diff summary
- `crates/caco-web/src/bin/caco-web-observe.rs`: new helpers
  `detect_system_chromium`, `which_in_path`, `in_nix_shell`,
  `apply_chromium_env`; `run_pw` now calls `apply_chromium_env`
  before spawning. 4 new tests.
- `crates/caco-web/src/bin/caco-web-dev-server.rs`: bonus
  broken-on-main clippy fix (`format!("{err}")` →
  `err.to_string()`) so caco-web --all-targets clippy is green.

## Operator-takeaway
caco-web-observe now works on NixOS workers without manual
PLAYWRIGHT_CHROMIUM_EXECUTABLE_PATH gymnastics. Set
CACO_WEB_OBSERVE_CHROMIUM if your Chromium is at a non-standard path.
Bonus: caco-web --all-targets clippy is green again.
