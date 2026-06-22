# caco-web observation audit (2026-06-17T12:25:09Z)
Server: caco 1.2.1261 port 19411, daemon 127.0.0.1:11100 (ms-dev)

## agents
## beads
## feed
## inbox
## chat
## nodes
## services
## files
## links
## projects
## choices
## notifications
## actions
## suggestions
## logs
## timeline
## summaries
## transcription
## merge-queue

## Result
- Broad pass across 19 main views (status, agents, beads, feed, inbox, chat, nodes, services, files, links, projects, choices, notifications, actions, suggestions, logs, timeline, summaries, transcription, merge-queue): console 0 messages, 0 failed network requests.
- Keyboard shortcuts: all 21 advertised single-key shortcuts navigate correctly (see shortcut-test.txt). transcription has no advertised shortcut (intentional).
- Responsive: no document-level horizontal overflow at 390px on status/beads/agents/nodes/merge-queue/chat.
- Defect found + fixed (bd-cdbea4): Status hero copy/meta unreadable over the full-opacity cluster-pulse canvas; restored documented localized scrim + text-shadow. Before/after proof in screenshots/fix/.
