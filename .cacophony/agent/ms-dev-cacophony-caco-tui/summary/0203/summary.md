# Session summary — caco-tui push_toast hardcoded "Shell" title fix

## Goal

During a caco-tui idle observation sweep, turn a visual finding into a landed
fix. While reanalyzing a suspected multi-instance "border fragment" artifact, I
found that every simple TUI toast renders a misleading "⚠ Shell" header because
the generic toast helper hardcodes its title. Fix the mislabel so notices read
neutrally, and accurately re-scope the original border-artifact draft.

## Bead(s)

- `bd-83c144` — caco-tui push_toast hardcodes toast title 'Shell' for all ~131 simple toasts (implemented here)
- `bd-2834a1` — multi-instance warning border fragments: reanalyzed and downgraded to draft (likely not a --no-gfx bug; toast Clears its rect, the trailing border is the intentional right margin)

## Before state

- Failing tests: none.
- `crates/caco-tui/src/state/mod.rs` push_toast() built every toast as `ToastPopup { level: "warning", title: "Shell", body: message }`. All ~131 call sites (voice, tab-layout, discard, view-saved, multi-instance read-only warning, ...) showed the title bar "⚠ Shell" regardless of content.
- The richer notification toast path already sets a real `display_title`; the generic path's "Shell" was a leftover placeholder. No test asserted title == "Shell".

## After state

- Failing tests: none. New unit test `push_toast_uses_notice_title_not_shell_bd_83c144` (in caco-tui state::tests) pins neutral title + verbatim body + level. (Focused queued run was interrupted by an operator daemon restart — daemon_restart_recovered, retryable infra; the merge-gate `cargo test-small` re-validates the lib test on the merge commit.)
- push_toast now sets `title: "Notice"` (level kept "warning" to avoid changing color coding across all toasts).

## Diff summary

- Code/content commit: `26002ffbca` (final landed squash SHA from the reintegration receipt).
- Files touched: `crates/caco-tui/src/state/mod.rs` (push_toast title + comment), `crates/caco-tui/src/state/tests.rs` (+1 test).
- Tests: +1; 0 removed; 0 flipped.
- Behavioural delta: simple toasts now show "⚠ Notice" instead of "⚠ Shell"; no other behavior change.

## Operator-takeaway

Every simple TUI toast was mislabeled "Shell" due to a hardcoded placeholder title in push_toast; it now reads "Notice". Bonus: the suspected multi-instance "border bleed-through" (bd-2834a1) is most likely not a real bug in --no-gfx — the toast Clears its rect and the trailing border is the window frame in the toast's intended right margin — so that draft was downgraded rather than "fixed". A graphics-mode (kitty) capture is still worth doing to confirm there's no real artifact where Clear is intentionally skipped.
