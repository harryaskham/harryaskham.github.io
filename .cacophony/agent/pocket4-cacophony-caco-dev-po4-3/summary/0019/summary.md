# Session Summary — bd-60b065 (detect_capability ghostty-under-tmux env fast-path) via operator Xvfb directive

> Landed under bd-60b065, the complementary env fast-path follow-up to bd-597c4d.
> bd-597c4d (runtime kitty-capability handshake) was landed+closed by msm-3 at dab36c05d during the helsinki beads outage.
> This commit adds the synchronous GHOSTTY_BIN_DIR/RESOURCES_DIR env branch that fires at SurfaceManager construction before the runtime handshake. Layered: env fast-path (here) then runtime probe (dab36c05d).

## Goal
Operator (Harry) broadcast: headless nodes should "be creative — use Xvfb or
WSLg etc." to spin up a virtual DISPLAY + terminal surface and become
graphics-capable, instead of declining graphics/detection beads for lack of a
display. Explicitly unblocks the detect_capability follow-ups bd-597c4d /
bd-55ae44 that headless nodes (po4/cs-2) had declined.

## What I did

### Proved headless graphics capability on pocket4 (NixOS)
pocket4 has `Xvfb`, `kitty` 0.47.0, and `ghostty` 1.3.1 in the nix profile.
Proven recipe (broadcast to the fleet, independently reproduced by po4-1/po4-2):
```
Xvfb :99 -screen 0 1280x800x24 &
DISPLAY=:99 kitty bash -lc 'env | grep -E "^TERM=|^KITTY_" > /tmp/out.txt'
```
Inside that kitty child: `TERM=xterm-kitty`, `KITTY_WINDOW_ID=1`, `KITTY_PID`,
`KITTY_INSTALLATION_DIR` — the full surface `detect_capability()` inspects.
(po4-1 refinement: capture to a file, not an outer pipe — kitty routes child
stdout to its own pty.) Only cosmetic transparency/compositing warnings under
Xvfb; no functional failure.

### bd-597c4d — root cause (empirically reproduced), FIXED
Using the Xvfb harness I reproduced the exact failing scenario and pinned the
real cause, which differs from the original report:
- **kitty under tmux already worked**: tmux preserves `KITTY_WINDOW_ID`/
  `KITTY_PID` into the child, hitting the tertiary branch (bd-23e67d).
- **ghostty under tmux FAILED**: tmux overwrites `TERM_PROGRAM` (→ `tmux`) and
  `TERM` (→ `xterm-256color`), and ghostty sets NO `KITTY_*` — it exposes
  `GHOSTTY_BIN_DIR` / `GHOSTTY_RESOURCES_DIR` / `GHOSTTY_SHELL_FEATURES`
  instead. So all three existing branches (TERM_PROGRAM, TERM substring,
  KITTY_*) missed → `detect_capability()` returned `None` for a fully
  graphics-capable ghostty terminal.

Fix (`crates/caco-tui/src/kitty.rs`):
- Added a quaternary detection branch for ghostty's own env vars
  (`GHOSTTY_BIN_DIR`/`GHOSTTY_RESOURCES_DIR`), mirroring the kitty `KITTY_*`
  branch — these survive tmux/socat/relay scrubbing.
- Taught `is_relay_environment()` about the ghostty-env-without-direct-
  TERM_PROGRAM relay case.
- Refactored precedence into a pure `detect_capability_from_env(...)` helper so
  it is deterministically unit-testable without racy `std::env::set_var`; the
  public `detect_capability()` reads real env and delegates.

### Validation
- **Unit**: `detect_capability_from_env_precedence_bd_597c4d` covers all four
  branches + negatives. Full caco-tui lib suite **4105 passed / 0 failed**;
  `cargo clippy -p caco-tui --lib -- -D warnings` clean.
- **Empirical (the operator's whole point)**: a throwaway probe calling the
  real `detect_capability()` inside Xvfb + real ghostty:
  - (A) ghostty direct → `cap=Kitty` (via TERM_PROGRAM/TERM).
  - (B) ghostty UNDER tmux (the failing scenario) → `cap=Kitty relay=true` via
    `GHOSTTY_BIN_DIR`, with `TERM=xterm-256color TERM_PROGRAM=tmux
    KITTY_*=None`. Before this change (B) was `None`.
  Probe removed after validation (not shippable); only `kitty.rs` changed.

## Coordination
- Broadcast the proven Xvfb recipe — unblocks all headless nodes' detection-
  class graphics validation (po4-1, po4-2 confirmed it works on their nodes).
- Handed bd-55ae44 to po4-1 (they have the same Xvfb capability and offered),
  with guidance to dedup against bd-597c4d if it's the same ghostty gap.
- Noted that true VISUAL flicker/placement beads (bd-12879e sidebar — now owned
  by msm-1; bd-5d83b1) still want a real compositor for human before/after, but
  detection logic is now in reach from headless nodes.

## SPEC
- SPEC 20.7 graphics capability detection / terminal-graphics enablement.

## Diff
Agent-branch code commit `81fd93c6d`. See reintegration receipt for the landed
squash SHA.

## Beads
- bd-597c4d — fixed + empirically validated; claim+close after the helsinki
  beads-proxy flap (bd-a9419e) clears (proxy was returning "temporarily
  unavailable" throughout this session).
