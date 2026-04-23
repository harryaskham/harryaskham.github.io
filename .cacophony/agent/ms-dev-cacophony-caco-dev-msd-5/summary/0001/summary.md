# Session summary — bd-542d54 forensic snapshot pin / rotate

## Goal

Add the operator-pin and rotation layer on top of the bd-6ac6b5 reconciler
snapshot infrastructure so postmortem evidence (e.g. the snapshot dir that
captures the moment the destructive-shrink guard tripped) cannot be silently
rotated away by the daemon's retention cron. Also wire the auto-pin hook into
the bd-53f5a7 abort path so this happens without operator intervention.

## Bead(s)

- `bd-542d54` — Forensic snapshot retention policy: 7d rolling + indefinite
  pin for most-recent-post-incident snapshots
- (rooted in postmortem epic `bd-cf99b7`; pairs with `bd-6ac6b5` snapshot
  infrastructure and `bd-53f5a7` shrink-cap abort)

## Before state

- Failing tests: none in scope
- `.beads/snapshots/<UTC-iso>/` directories were created by the reconciler
  (bd-6ac6b5 criterion 1) but had no rotation, no pin mechanism, and no CLI
  surface. The bd-542d54 bead was open and unassigned.
- `caco bd snapshot ...` did not exist as a subcommand tree.
- The destructive-shrink abort (bd-53f5a7) preserved the *current* on-disk
  jsonl but did nothing to mark the most-recent snapshot as forensically
  important — a scheduled cron could legitimately delete it 7 days later.

## After state

- Failing tests: none.
- New module `caco-beads::snapshots` with `list`, `pin`, `unpin`, `rotate`,
  and `auto_pin_most_recent_for_shrink_abort`. Pin sidecar lives at
  `.beads/snapshots/<stamp>/PINNED` and stores the operator-supplied
  `--reason` text. Default retention 7 days.
- Reconciler's destructive-shrink abort path now calls
  `auto_pin_most_recent_for_shrink_abort` best-effort before returning the
  error, so the most-recent snapshot is protected without operator action.
- New CLI subtree `caco bd snapshot {list,pin,unpin,rotate}` with MCP
  enablement — tools advertised as `caco_bd_snapshot_list` etc.
- 11 new tests across `caco-beads` (9 unit tests on the snapshots module +
  1 integration test asserting the auto-pin fires on shrink abort) and
  `caco-cli` (3 spec / MCP-name tests).
- `cargo test-small` green; `cargo clippy -p caco-beads -p caco-cli` green;
  `cargo test -p caco-beads --lib` 230 passed.

## Diff summary

- Commit: `9f5ff722` (rebased onto current main as `d96cdd83`)
- Files touched:
  - `crates/caco-beads/src/snapshots.rs` (new, ~430 lines incl. tests)
  - `crates/caco-beads/src/lib.rs` (export the new module)
  - `crates/caco-beads/src/store.rs` (auto-pin call in shrink-abort path,
    +1 integration test)
  - `crates/caco-cli/src/lib.rs` (new BD_SNAPSHOT_SUBCOMMANDS branch +
    4 dispatch functions + 3 spec tests)
- Tests: +12 / -0 / flipped 0
- Behavioural delta: any reconciler abort under bd-53f5a7 now leaves a
  pinned snapshot for forensics; operators can pin any snapshot manually
  via `caco bd snapshot pin --stamp <iso> --reason "..."` and rotate
  unpinned old snapshots via `caco bd snapshot rotate [--retention-days N]`.

## Operator-takeaway

The forensic-snapshot pin mechanism is now load-bearing for the bd-cf99b7
postmortem chain: when the bd-53f5a7 shrink-cap aborts a future destructive
write, the prior snapshot is auto-pinned and survives the bd-6ac6b5
retention sweep until an operator runs `caco bd snapshot unpin`. The
follow-up wiring is to schedule `caco bd snapshot rotate` from the daemon
cron (one line in `caco-daemon`'s scheduler) — left as a small follow-up
bead because it is a separate concern (cron plumbing) from this bead's
contract (the pin/rotation primitives themselves). Doctor sensor for
"pinned snapshots count" (criterion 5) is similarly a small follow-up.
