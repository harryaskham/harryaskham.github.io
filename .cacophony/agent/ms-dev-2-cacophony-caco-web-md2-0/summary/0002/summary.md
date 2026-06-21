# Session summary — caco-web: Services/Projects snapshot-failure empty states (bd-47b2fa)

## Goal

Follow-up to bd-0557e9. A fresh caco-web duty-cycle backend-unavailable probe of
the remaining inventory surfaces found that the Services and Projects views still
showed misleading healthy-empty states ("No services" / "No projects configured")
during a backend outage — contradicting the "Dashboard backend unavailable" toast
and, for Projects, implying a config problem rather than connectivity. This
session extends the bd-0557e9 fix so those snapshot-fed views also reflect the
outage.

## Bead(s)

- `bd-47b2fa` — caco-web: Services/Projects show misleading "No services"/"No projects configured" during a failed snapshot (filed + claimed + fixed this session)
- (parent: `bd-0557e9` — nodes/agents/beads snapshot-failure empty states; landed earlier today)
- (related: `bd-a01431` — reflect draft: no central renderActiveView helper)

## Before state

- Failing tests: none.
- During a never-loaded snapshot failure: Services → "No services"; Projects →
  "No projects configured" (Evidence: web/screenshots/before-{services,projects}.png).
- Root: renderServices (~8124) and renderProjects (~13874) ALREADY have
  snapshot-aware empty states, but bd-0557e9's failure-path re-render only covered
  nodes/agents/beads, so Services/Projects kept their stale boot-time empty state
  (rendered before snapshotUnavailable flipped).

## After state

- Failing tests: none (`cargo test -p caco-web --lib`, incl. new contract test
  `app_js_snapshot_empty_rerenders_services_projects_bd_47b2fa`).
- Services → "Services delayed / Waiting for the initial daemon snapshot…";
  Projects → "Projects delayed / Waiting for the initial daemon snapshot before
  showing project data." (Evidence: web/screenshots/after-{services,projects}.png).

## Diff summary

- Code/content commit: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `crates/caco-web/static/app.js`: added `rerenderSnapshotEmptyForCurrentView()`
    (re-renders the active snapshot-fed inventory view — nodes/agents/beads/
    services/projects) and routed the loadSnapshot-failure re-render through it,
    replacing bd-0557e9's nodes/agents/beads-only inline block. Scoped to
    snapshot-fed views (Choices/Merge Queue load from their own endpoints).
  - `crates/caco-web/src/tests.rs`: +1 contract test (bd-47b2fa).
- Tests: +1 / -0 / flipped 0.
- Behavioural delta: Services/Projects now show the snapshot-unavailable/delayed
  state during a never-loaded outage instead of misleading healthy-empty. No
  change to stale-data-after-load behaviour or the connection-status banners.

## Embedded artefacts

- `web/screenshots/before-services.png` — pre-fix "No services" during outage.
- `web/screenshots/before-projects.png` — pre-fix "No projects configured" during outage.
- `web/screenshots/after-services.png` — post-fix "Services delayed".
- `web/screenshots/after-projects.png` — post-fix "Projects delayed".

## Operator-takeaway

This finishes the bd-0557e9 thread for the snapshot-fed inventory views: the
dashboard no longer tells an operator "No projects configured" / "No services"
during a backend outage on those panes. The underlying gap is still bd-a01431
(no single central renderActiveView helper — the failure path re-renders via a
small per-view switch); a future refactor could unify that. Choices / Merge
Queue were intentionally left out because they load from their own endpoints,
not the UI snapshot.
