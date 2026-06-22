# Session summary — bd-eda8f5: notifications hero-header div → <header>

## Goal
Pattern (m) semantic `<header>` coverage parity: notifications.html used `<div>` for page header instead of `<header>`.

## Bead
- `bd-eda8f5`

## Audit
- `<header>` per entry HTML inventoried.
- workspace.html has `<header class="workspace-topbar">` ✓.
- notifications.html: hero-header was `<div>`.

## Fix
notifications.html:145 `<div class="hero-header">` → `<header class="hero-header">`.

## Why
- Native `<header>` provides sectioning semantics.
- Inside `<main>`, `<header>` = section header (not banner).
- Landmark rotor navigation in screen-readers.

## Regression test (~30 lines)
- Assert `<header class="hero-header">` present.
- Assert `<div class="hero-header">` NOT present (regression defense).

## Operator-visible effect
- Landmark navigation includes Notifications section header.

## Diff summary
- `crates/caco-web/static/notifications.html` -- div→header swap.
- `crates/caco-web/src/tests.rs` -- new bd-eda8f5 forward-guard (~30 lines).
- Net pass: 577 -> 578; 0 failures.

## Operator-takeaway
69 cycles, 112 wins. Pattern (m) semantic header coverage. Pattern catalog: 22 entries.
