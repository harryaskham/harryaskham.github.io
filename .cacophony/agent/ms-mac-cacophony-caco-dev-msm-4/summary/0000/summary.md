# bd-35dc9e — speaking-clock cron emits greeting instead of announcement

## Goal
Stop the hourly `speaking-clock` cron from broadcasting conversational
greetings (`Got it! It's **Tuesday, April 21 at 9:00 PM**. Is there
something I can help you with?`) and ensure the announcement that
reaches TTS contains no markdown emphasis.

## Bead(s)
- bd-35dc9e (P2 task) — speaking-clock cron is being spawned wrong;
  agent comes back with a greeting instead.

## Before state
- `.cacophony/automation.yaml` placed the "respond with ONLY the
  announcement" directive only in the Anthropic-payload `system` field.
  When the upstream proxy stripped or downgraded the system prompt,
  the user message ("The current time is …") read like a chat opener
  and the model replied with a greeting.
- The model occasionally wrapped the date in `**bold**` markdown,
  which TTS read aloud as the literal word "asterisk asterisk".
- `imported_automation_command_uses_shell_expandable_key_path` did not
  cover either failure mode, so the regression was invisible to CI.

## After state
- The directive is repeated inside the user-message content
  ("Reply with ONLY the announcement itself — no greeting, no
  acknowledgement, no markdown, no follow-up question."). The system
  prompt is also tightened to add "no markdown".
- The cron post-processes the model response with `sed -E` to strip
  `*`, `_`, and backticks, plus a leading `Got it!`, `Sure!`,
  `Here you go:`, `Of course!`, `Certainly!`, `Okay!`, or `OK!`. The
  pre-existing whitespace normalisation through `tr/awk` is preserved.
- The cron-fixture test asserts both the user-message directive and
  the markdown/lead-in stripping are present, so regressions surface
  immediately.

## Diff summary
- `.cacophony/automation.yaml` — speaking-clock cron command body:
  user message now carries the directive; sed pipeline added.
- `crates/caco-config/tests/config.rs` —
  `imported_automation_command_uses_shell_expandable_key_path` now
  asserts the new directive and the sed post-processing.

## Operator-takeaway
The speaking-clock no longer leaks chat-style preamble into
broadcasts, even if a future proxy strips the system prompt, and the
TTS path no longer reads markdown asterisks aloud. If a future model
adds a new conversational lead-in not covered by the regex, extend the
sed alternation list and the matching test assertion together.

## Tests
- `cargo test -p caco-config --test config imported_automation` — 1 passed.
- `cargo test-small` — 4140 passed, 0 failed.
