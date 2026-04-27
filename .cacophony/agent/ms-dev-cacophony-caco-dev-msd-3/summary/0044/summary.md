# Session summary — notification endpoint SPEC alignment

## Goal

Align the normative SPEC notification API contract with the implemented daemon routes and public API documentation for single-notification detail and acknowledgement. This was a small contract-drift fix discovered during a Pages/API audit.

## Bead(s)

- `bd-95d17f` — [spec] notification detail and ack endpoints missing from SPEC

## Before state

- Failing tests: none observed; this was a documentation/spec contract drift.
- Relevant metrics: `git diff --check` passed before commit.
- Context: `SPEC.md` section 15.3.1 listed only `POST /api/v1/notifications` and `GET /api/v1/notifications`, while the daemon implements `GET /api/v1/notifications/<notification-id>` and `POST /api/v1/notifications/<notification-id>/ack`.

## After state

- Failing tests: none observed.
- Relevant metrics: `git diff --check` remained clean after the SPEC edit.
- Context: `SPEC.md` now lists the detail and ack endpoints and records the required `not_found`, idempotent ack, feed replication, UI-stream, and convergence semantics.

## Diff summary

- Commits: `c6b7832d1` (`bd-95d17f: document notification detail and ack endpoints`)
- Files touched: `SPEC.md`
- Tests: +0 / -0 / flipped 0
- Behavioural delta: no runtime behavior changed; the normative API contract now matches the implemented notification detail and acknowledgement routes.

## Operator-takeaway

The implementation was already present; this closes the source-of-truth gap so future notification API/docs work can rely on SPEC 15.3.1 without rediscovering the hidden detail and ack endpoints.
