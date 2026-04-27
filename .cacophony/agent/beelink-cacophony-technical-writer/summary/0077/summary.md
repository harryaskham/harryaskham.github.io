# Session summary — caco-web status and profile-mode Pages alignment

## Goal

Refresh the GitHub Pages documentation after recent caco-web service-management, web workspace, and persistent-profile reintegration policy changes, while running a full public-docs audit for staleness, correctness, secrets, and visual consistency with the web surface.

## Bead(s)

- `bd-1d2e41` — technical-writer recurring documentation and GitHub Pages maintenance
- Related: `bd-f74047` — caco-web stale dashboard assets/version-drift service-management behavior
- Related: `bd-95cda5` — persistent-profile default reintegration modes adjusted away from flaky PR defaults

## Before state

- Failing tests: none run by this docs-only agent; code-test ownership for `supervisor_config_tests::enterprise_theme_is_registered_and_well_formed` remains with `bd-ddcb2a`.
- Relevant metrics: `docs/validate-pages.sh` passed on the pre-change tree; `docs/cli.html` was 51132 bytes, still below the 51200-byte page budget.
- Context: `README.md`, `AGENTS.md`, and `SPEC.md` described `caco web status` served-version drift, but the published daemon Pages page still described caco-web only as PID/port/health probing. Recent profile frontmatter changed `caco-aks`, `caco-android`, and `caco-tui` default reintegration modes to `direct`, while `docs/profiles.html` still listed their previous PR-backed defaults.

## After state

- Failing tests: none introduced; documentation validation passed.
- Relevant metrics: `./docs/validate-pages.sh` reported 1781 passed, 0 warnings, 0 failed; `bash -n docs/install.sh`, `git diff --check`, public command/secret scans, HTML public-safety scan, and CSS token parity scan passed. The known `caco-aks-master` public-hygiene drift remains tracked under `bd-a0bbb2` and was not edited here because another agent owns that bead.
- Context: `docs/daemon.html` now states that `caco web status` reports served `/health` version versus current package version, flags `version_drift`, and treats responsive stale dashboard assets as respawn-eligible after updates. `docs/profiles.html` now lists `caco-aks`, `caco-android`, and `caco-tui` with `direct` default reintegration mode. `docs/index.html` now mentions agent detail split panes in the web workspace caption.

## Diff summary

- Commits: `903703dad`, `90a2aa572`
- Files touched: `docs/daemon.html`, `docs/profiles.html`, `docs/index.html`
- Tests: +0 / -0 / flipped 0
- Behavioural delta: Published Pages documentation now matches the implemented and in-repo documented caco-web version-drift status behavior, current persistent-profile reintegration defaults, and the web workspace Agent Detail split-pane behavior.

## Operator-takeaway

The Pages site now reflects caco-web stale-asset/version-drift handling, direct-mode defaults for key persistent profiles, and web workspace agent-detail split panes without touching the actively owned AKS/theme/TUI docs-drift bead; the remaining public-doc drift is still tracked separately under `bd-a0bbb2`.
