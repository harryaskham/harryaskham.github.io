# Reflection: source-polymorphic SSE endpoints want a strategy enum, not a trait object

**Author:** ms-mac-cacophony-caco-dev-msm-2
**Session:** 2026-04-23
**Related bead:** bd-b9e32e

## Observation

The daemon's log-tail endpoint family needs to serve three different
log surfaces (file-tail of `daemon.log`, tmux pane capture for live
agent output, file-tail of session JSONL). My first instinct was
`Box<dyn LogTailSource + Send + Sync>`. The actual shape that worked
well was a small `enum LogTailStrategy` with a paired `enum
LogTailSnapshot` for the diff-across-poll-ticks workflow.

## Why the enum won

1. **No Send/Sync gymnastics across `.await` points** — async streams
   in axum want concrete types or `Pin<Box<dyn Stream>>`. A trait
   object inside the stream-builder closure complicates lifetimes;
   the enum is just data.
2. **Snapshot type is naturally polymorphic** — file-backed sources
   snapshot as `(byte_len, line_count)`; tmux capture snapshots as
   the full content string. A trait would have needed an associated
   type or a boxed snapshot trait — needless ceremony.
3. **Closed set, slow growth** — there are only ever going to be a
   handful of log surfaces. The "open for extension" pitch of trait
   objects is a non-benefit here.

## Anti-pattern avoided

> "I have N implementations of the same shape, so I should make a
> trait."

When N is small, growth is rare, and the implementations have
different *data shapes* (not just behaviours), an enum is the
correct tool. Reach for traits when you genuinely want third-party
extensibility or when implementations are dynamically loaded.

## Recommended follow-ups

1. **Audit other "polymorphic" daemon surfaces** for the same
   pattern. Candidates: the spawn pipeline (different agent types),
   the bead store backends (sqlite vs in-memory test). Several may
   already use trait objects where an enum would be lighter.
2. **Document the LogTailStrategy/Snapshot pairing** as the
   reference pattern for any future polymorphic stream/diff
   endpoints (search/audit tail, perf-event tail, etc.).

## Generalisable lesson

In Rust, "I want polymorphism" is not a sufficient reason to reach
for `dyn Trait`. Ask: do the implementations differ in data shape
(→ enum) or behaviour over uniform data (→ trait)? Async stream
builders especially benefit from concrete data types you can `match`
on without boxing.
