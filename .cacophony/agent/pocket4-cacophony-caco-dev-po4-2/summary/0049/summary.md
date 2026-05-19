# Session summary — route STT wakes to full narrator caller

## Goal

Fix `bd-d47a11`, where STT transcript wake notifications for the `ms-mac` source could be addressed with only a project-qualified target. The goal was to ensure caco-stt-daemon wake messages reach the actual transcript-narrator inbox/cursor loop rather than depending on best-effort injection or being confused with unrelated persistent agents such as update-helper.

## Bead(s)

- `bd-d47a11` — Fix STT wakeup event routing to correct agent

## Before state

- Failing tests: none known at start.
- Relevant metrics: live `caco @ms-mac stt status --instance ms-mac --json` showed the configured wake target `helsinki-cacophony-transcript-narrator`, and recent feed events showed STT wake messages stored with target `cacophony:helsinki-cacophony-transcript-narrator`.
- Context: direct-message inbox delivery filters by the full caller ID (`<node>:<project>:<agent-id>`), so project-qualified aliases can be durably stored yet not match the recipient's inbox caller (`helsinki:cacophony:helsinki-cacophony-transcript-narrator`).

## After state

- Failing tests: none in targeted queued validation.
- Relevant metrics: bare/`@agent:` message targets now resolve from the structured daemon agent inventory to the full node-project-agent caller shape when the owning node is known.
- Context: STT wake targets configured as `helsinki-cacophony-transcript-narrator` now canonicalize to `helsinki:cacophony:helsinki-cacophony-transcript-narrator`, preserving transcript-narrator safety boundaries because the wake body remains a nudge to read `caco stt diff`, not transcript text or a command.

## Diff summary

- Code/content commits: `c53c0cfca`
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched: `crates/caco-cli/src/msg_cmd.rs`, `crates/caco-cli/src/lib.rs`
- Tests: +1 / -0 / flipped 0
- Behavioural delta: `caco msg send --target <bare-agent-id>` prefers full `<node>:<project>:<agent-id>` resolution when `/api/v1/projects/<project>/agents` includes node metadata, falling back to the older project-qualified helper only when structured inventory cannot provide a full target.
- Validation: queued `cargo test -p caco-cli msg_target -- --test-threads=2` passed as `tj-d230704b`.

## Operator-takeaway

The STT daemon did have the intended narrator target in config, but the CLI alias layer shortened it before sending; this patch keeps wake delivery aligned with the direct-inbox identity model so narrator cursor advancement can be triggered reliably.
