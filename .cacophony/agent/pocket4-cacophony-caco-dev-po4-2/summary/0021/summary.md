# Session summary — Pi launch settle now recognizes interpreter-launched package paths

## Goal

Fix the ms-mac persistent-agent launch failure where Pi runtimes were being
classified as a post-bootstrap shell fallback (`observed bash`) instead of a
real Pi runtime. The goal was to tighten the runtime-settle detector so it can
recognize the actual Pi process shape on macOS and stop killing healthy
persistent Pi/support agents under a false shell-fallback diagnosis.

## Bead(s)

- `bd-99e12b` — ms-mac Pi persistent agents launch into bash instead of configured Pi runtime

## Before state

- Affected ms-mac persistent agents included config-helper, caco-doctor-msm,
  caco-aks, and caco-tui.
- The launch verifier rejected them with an observed-bash / shell-fallback
  error even though the issue seemed concentrated in Pi/support profiles rather
  than a generic node outage.
- The detector already knew how to treat interpreter-launched runtimes like
  Node/Codex specially, but its command-line matching for Pi was too literal:
  it only treated basename `pi` as a Pi runtime.
- On macOS, Pi can be launched under an interpreter with an installed package
  path like `.../@mariozechner/pi-coding-agent/.../cli.js`, whose basename is
  not `pi`.

## After state

- The launch detector now treats `pi-coding-agent` package-path arguments as a
  valid Pi runtime match when the expected runtime is `pi`.
- The generic runtime-settle logic and shell-fallback guard remain unchanged;
  only the Pi-specific interpreter-path recognition was widened.
- Added a focused regression test that pins this exact package-path form so the
  detector cannot silently regress back to basename-only matching.

## Diff summary

- Files touched:
  - `crates/caco-daemon/src/agent/health.rs`
  - `crates/caco-daemon/src/agent/tests.rs`
- Validation:
  - `cargo test -p caco-daemon process_cmdline_contains_runtime_pi_package_path_positive -- --nocapture`
  - `cargo test -p caco-daemon process_cmdline_contains_runtime_nonexistent_pid -- --nocapture`
  - `cargo build -p caco-daemon`
- Behavioural delta:
  - interpreter-launched Pi runtimes on macOS are less likely to be misread as
    a settled interactive shell after the wrapper exits
  - persistent Pi/support agents should no longer fail solely because the
    process tree exposes `pi-coding-agent/.../cli.js` rather than a basename
    `pi`

## Operator-takeaway

This fix does not weaken the shell-fallback guard; it makes the guard more
accurate for Pi on macOS. The runtime verifier now recognizes the real Pi
package-path process shape it was previously missing, which should reduce the
false 'launched into bash' failures that were keeping ms-mac persistent support
agents from staying up.
