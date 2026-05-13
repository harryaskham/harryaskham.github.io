# Session summary — Split profiles lifecycle guidance page

## Goal

Resolve `bd-e89fcb`, which tracked `docs/profiles.html` sitting so close to the default 51,200-byte Pages budget that routine profile documentation updates failed validation. The goal was to create durable headroom without adding a blanket budget exception.

## Bead(s)

- `bd-e89fcb` — [docs] Split or budget-relax docs/profiles.html before profile docs edits keep failing

## Before state

- Failing tests: `docs/profiles.html` itself still passed, but it was only 51,184 bytes against the 51,200-byte default budget, leaving 16 bytes of headroom.
- Relevant metrics: adding one normal Pi helper-child diagnostic sentence during the previous technical-writer pass pushed the page to 51,558 bytes and failed `docs/validate-pages.sh`.
- Context: the page mixed schema/reference content, shipped-profile inventory, and long operational lifecycle guidance in one published HTML file.

## After state

- Failing tests: none in the docs validation lane.
- Relevant metrics: `docs/profiles.html` is now 39,184 bytes and the new `docs/profile-lifecycle.html` companion is 16,765 bytes. `./docs/validate-pages.sh` reported 3414 passed, 0 warnings, 0 failed; `git diff --check` was clean.
- Context: the companion page keeps the existing Agent Profiles sidebar entry active rather than adding a new sidebar href, so the shared navigation sequence remains stable.

## Diff summary

- Commits: pending reintegration squash.
- Files touched: `docs/profiles.html`, `docs/profile-lifecycle.html`, `docs/daily-changelog.md`, and this summary.
- Tests: +0 / -0 / flipped 0; validation was Pages QA plus whitespace checking.
- Behavioural delta: no runtime behavior changes. The long persistent/composite/mixin profile guidance moved out of `docs/profiles.html`, and the omitted Pi helper-child diagnostic guidance now lives in the lifecycle companion page.

## Operator-takeaway

The profile reference now has roughly 12 KiB of headroom, so future routine profile docs edits should not require unrelated prose trimming; lifecycle guidance remains published and linked from the original page.
