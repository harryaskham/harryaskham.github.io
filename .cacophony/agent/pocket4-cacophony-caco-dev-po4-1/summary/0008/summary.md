# Session summary — fix marking notifications as read in webapp

## Goal

Fix the bug where marking notifications as read in the webapp
didn't persist the change to the backend. After page refresh,
notifications would reappear as unread.

## Bead(s)

- `bd-7476e5` — Fix marking notifications as read in webapp (P2 bug)

## Before state

- markNotificationRead(), markNotificationGroupRead(), and
  markAllNotificationsRead() only updated local state.notifications[]
  with n.acknowledged = true.
- No API call to persist the acknowledgment to the backend.
- Daemon endpoint POST /api/v1/notifications/{notif_id}/ack existed
  (bd-281351) but wasn't used by the webapp.
- Notifications appeared as read during the session but reappeared
  as unread after page refresh.

## After state

- All three mark-as-read functions now call the daemon API:
  POST /api/v1/notifications/{notif_id}/ack for each notification.
- Changes persist to backend immediately.
- Notifications remain marked as read across page refreshes.
- No visual regression — updates are still immediate.
- All 185 caco-web tests + smoke tests pass.

## Diff summary

- `crates/caco-web/static/app.js` (+15 lines, -2 lines):
  - markNotificationRead(): added fetch POST /ack
  - markNotificationGroupRead(): added fetch POST /ack for each item
  - markAllNotificationsRead(): added fetch POST /ack for each notification

## Operator-takeaway

The fix is minimal: the daemon endpoint already existed, the webapp
just wasn't calling it. Now marking notifications as read actually
persists to the database and survives page refreshes.
