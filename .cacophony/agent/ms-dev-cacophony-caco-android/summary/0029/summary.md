# Session summary — bd-83c33d Wear complications: widen SHORT_TEXT/LONG_TEXT support

## Operator report (2026-06-02)

> "check the complications, they currently are blank"

## Root cause

Each of the 6 wearable complication data sources declared exactly
one ComplicationType in the manifest:

- 5 numeric sources (Agents / Beads / Choices / Exceptions /
  Inbox) declared only `SHORT_TEXT`.
- The combined Status source declared only `LONG_TEXT`.

Wear OS watch faces let the operator pick a complication for each
slot, but slot pickers are filtered by the slot's *expected*
shape. A SHORT_TEXT-only data source cannot be selected for a slot
that wants LONG_TEXT, and a LONG_TEXT-only source cannot fill a
SHORT_TEXT slot. Most modern faces have at least one slot of
either shape; if the operator's chosen slot expects the *other*
shape from what Cacophony advertised, the slot renders blank by
design and the operator never saw why.

The not-configured / error / zero states already render visible
glyphs ("—" / "?" / "0") via the existing layout helpers, so the
fix is not about placeholder text — it's about the slot picker
not offering Cacophony in the first place.

## Bead(s)

- `bd-83c33d` — Wear complications render blank — diagnose +
  visible placeholder.

## After state

- New shared file
  `companion/android/wearable/src/main/java/com/cacophony/companion/wear/complications/WatchComplicationBuilders.kt`:
  - `internal fun buildSharedLongText(text, title, contentDescription):
    LongTextComplicationData` and `buildSharedShortText(text,
    contentDescription)` so the per-source builders don't have to
    import `Builder` + `PlainComplicationText.Builder` each.
  - `internal val CACOPHONY_NUMERIC_COMPLICATION_TYPES = setOf(
    SHORT_TEXT, LONG_TEXT)` and
    `CACOPHONY_STATUS_COMPLICATION_TYPES = setOf(LONG_TEXT,
    SHORT_TEXT)` constants used by the data sources for type
    gating.
- Every numeric data source (Agents / Beads / Choices /
  Exceptions / Inbox) now:
  - Rejects requests outside `CACOPHONY_NUMERIC_COMPLICATION_TYPES`
    rather than silently returning a SHORT_TEXT object.
  - Dispatches on `request.complicationType` — LONG_TEXT
    requests build via `buildSharedLongText(text = build…Text(
    state), title = "Cacophony <Surface>", contentDescription =
    build…ContentDescription(state))`; SHORT_TEXT stays on the
    existing helper.
  - The same dispatch is mirrored in `getPreviewData` so the slot
    picker preview renders correctly under both type queries.
- The combined Status data source mirrors the same shape:
  - SHORT_TEXT requests render via `buildSharedShortText` using
    the existing compact `buildStatusComplicationTitle(state)`
    helper (e.g. "3·1·5") and the long content description for
    TalkBack.
  - LONG_TEXT requests keep going through the existing
    `buildLongText(state)` path.
- `companion/android/wearable/src/main/AndroidManifest.xml`:
  - 5 numeric `SUPPORTED_TYPES` entries flipped from `SHORT_TEXT`
    to `SHORT_TEXT,LONG_TEXT`.
  - Status entry flipped from `LONG_TEXT` to `LONG_TEXT,SHORT_TEXT`.
- New `WatchComplicationWidenedTypesSourceTest` (4 tests):
  - Shared helper declares both type sets + helper functions.
  - All 5 numeric services dispatch on
    `CACOPHONY_NUMERIC_COMPLICATION_TYPES`, render LONG_TEXT via
    `buildSharedLongText`, and keep the SHORT_TEXT fallback.
  - Status service dispatches on
    `CACOPHONY_STATUS_COMPLICATION_TYPES`, renders SHORT_TEXT via
    `buildSharedShortText`, and keeps the LONG_TEXT fallback.
  - Manifest declares exactly 5 numeric `SHORT_TEXT,LONG_TEXT`
    entries plus the Status `LONG_TEXT,SHORT_TEXT` entry, and no
    stale single-type declarations remain.

## Diff summary

- Files touched (8):
  - 1 new shared helper file.
  - 6 complication data source services (dispatch + getPreviewData).
  - 1 manifest (widened SUPPORTED_TYPES).
- Files added (1):
  - new source-pin test (4 tests).
- Tests: +4 source-pin tests; no existing tests changed.

## Operator-takeaway

After installing the next wear APK, open the watch face complication
picker. Cacophony Agents / Beads / Choices / Exceptions / Inbox /
Status will now appear in **both** SHORT_TEXT and LONG_TEXT slot
categories. Pick whichever shape your face exposes — the slot will
render the same data styled appropriately (compact glyph for
SHORT_TEXT, body line plus 'Cacophony <Surface>' title for
LONG_TEXT). The previously-blank state was a slot-type filter
issue, not a fetch failure; no separate placeholder change was
required because the existing layout helpers already render "—" /
"?" / "0" for not-configured / error / zero states.
