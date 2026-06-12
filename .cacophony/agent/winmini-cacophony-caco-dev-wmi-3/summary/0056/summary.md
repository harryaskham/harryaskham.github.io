# bd-d235c5 — Ctrl-C in TUI agent-pane attach mode (regression guard + investigation)

## Bead
bd-d235c5 (input-handling/regression/tui, P0; filer harryaskham). Report: in attached mode, Ctrl-C is caught by the TUI and quits the app instead of being forwarded to the attached agent pane. AC: in attached mode ALL keyboard input incl. Ctrl-C passes through; the TUI must not quit.

## Investigation (caco-tui crates/caco-tui/src/app.rs)
Traced the full attach-mode key path:
- Main loop `drain_quit_events` (pre-drains Ctrl-C/Ctrl-R before render) correctly DEFERS (re-injects) the chord when `attached = tmux_attached || shell_attached.is_some() || ssh_attached.is_some()` (bd-dbebb2/bd-effb36), so it only outer-quits when NOT attached.
- `handle_key` attach branches all FORWARD + `return` before the global quit arms: tmux_attached → `forward_key_to_tmux` + return; shell_attached → `forward_key_to_shell` + return; ssh_attached WITH matched `attached_pane_id` → `forward_key_to_ssh` + return. All ssh-attach sites co-set `attached_pane_id` (bd-207cc4).
- `detach_if_target_not_visible` intentionally detaches when the attached agent/pane is no longer the visible pane or the inner tab moved off `Attach` (bd-3dd999); the Terminal→attach flow normalizes to the `Attach` tab (16204/17622/17672), consistent with that guard.

CONCLUSION: on current main the core attach path (tmux + ssh-with-matched-pane, Attach inner tab) ALREADY forwards Ctrl-C without quitting. A new headless regression test reproducing the exact scenario PASSES on current main. The only outer-quit-while-ssh-set path is the stale `attached_pane_id` mismatch, which `detach_if_target_not_visible` legitimately treats as navigated-away (detach → quit is by design, not the bug). This area had heavy recent fixing (bd-2f8efd, bd-060e65, bd-dbebb2, bd-effb36, bd-dbf8ae, bd-207cc4), so the reported regression was very likely already resolved before the operator's running build.

## Change
Added regression guard test `handle_key_ctrl_c_while_attached_does_not_quit_bd_d235c5` (caco-tui app.rs tests): asserts Ctrl-C while tmux-attached AND while ssh/PTY-attached (matched pane, Attach tab) does NOT set `should_quit`, does NOT open the exit-confirm dialog, and does NOT detach. This locks in the AC contract so a future change to the key router cannot silently reintroduce the quit-on-Ctrl-C regression.

No production code change: a speculative global-arm guard was prototyped but reverted — it was inert on the real path (`detach_if_target_not_visible` pre-empts the only path it would guard) and adding dead guard code to this sensitive, heavily-annotated input router was not worth the blast-radius risk.

## Validation (daemon test queue, --cwd at checkout)
- `cargo test -p caco-tui --lib handle_key_ctrl_c_while_attached_does_not_quit` (tj-dba99b38): PASSED.
- `cargo clippy -p caco-tui --lib` (tj-f2a8a291, during fix prototyping): PASSED, clean.
- rustfmt-clean; `git diff --check` clean.

## Operator follow-up
Could not reproduce the quit-on-Ctrl-C on the core attach path on current main (test proof). Messaged harryaskham: please confirm on a current build; if it persists, the exact attach flow (tmux vs ssh/unified-PTY, which inner tab, fresh attach vs after navigation) + build SHA will pin any remaining trigger. Reopen with repro if it persists; the guard test prevents future regressions of the verified-correct contract.

## Diff
See the reintegration receipt for the landed squash SHA.
