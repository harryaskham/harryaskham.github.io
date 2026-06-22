# Technical-writer review summary

## Goal

Continue the app-docs catch-up: after the Picophony page, add a public docs page
for the iOS phone app and Apple Watch companion, the biggest remaining app-docs gap.

## Bead(s)

- `bd-c63005` — technical-writer documentation maintenance.
- Documents the iOS/watchOS companion epic `bd-1a082f` (plus `caco-embed-ios` / `bd-5c289f`, mobile bootstrap `bd-a1a34f`).

## Before state

- No public docs page for `companion/ios`; only `wearable.html` covered Android phone + Wear OS.
- `docs/daily-changelog.md` current through `4fdccc627` (2026-06-21); Picophony page already landed.

## After state

- New `docs/ios.html` + `docs/ios.md`: components, the two build paths (agent-verifiable SwiftPM kit vs Xcode/Simulator), daemon connectivity (loopback / cluster mTLS bootstrap / embedded in-process `caco-embed-ios` daemon), feature parity surface, the Apple Watch layout/relay-vs-direct contract, and distribution (SOPS signing, TestFlight, `just ios-kit-smoke`).
- Linked from the sidebar on all pages and staged in `.github/workflows/docs.yml`; `docs/tui.html` trimmed under the 51200-byte budget.
- Validation: `./docs/validate-pages.sh` passed.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `docs/ios.html` (new), `docs/ios.md` (new), `docs/tui.html`, every `docs/*.html` sidebar, `.github/workflows/docs.yml`.
- Behavioural delta: documentation only.

## Operator-takeaway

The iOS + Apple Watch companion now has a first-party docs page sourced from the
companion/ios README/PARITY/WATCH-LAYOUT/MOBILE_REMOTE contracts, including the
no-Xcode kit-smoke lane and the embedded in-process daemon. Remaining: confirm
Android companion doc currency, document the sparse-checkout to git-LFS change,
and add the 2026-06-22 daily-changelog day.
