#!/usr/bin/env python3
"""The "how it was made" badge every a.skh.am site carries, from one place.

    python3 _tools/made-badge.py agents          # print the footer pill for one class
    python3 _tools/made-badge.py agents --meta   # print the <meta> tag for the page head
    python3 _tools/made-badge.py --all           # print every variant (for docs and review)

Classes:
    hand         Made by hand    — written by hand, mostly before 2026; since then agents have only cleaned up or fixed bugs
    with-agents  Made with agents — built by a person and AI agents working together
    agents       Made by agents  — built by AI agents from specs and configuration, with light human steering

Every page of a site declares its class in the head (<meta name="a.skh.am:made" content="…">), which
_tools/site-index.py reads for /all, and shows the pill in its footer. The pill styles itself inline
(system font, its own background and border, fixed size) so it looks the same on every site and theme
and needs no stylesheet or JavaScript; it links to https://a.skh.am/made/, which explains the classes.
Copy the printed HTML verbatim; don't restyle it per site.
"""
import sys

META = "a.skh.am:made"
URL = "https://a.skh.am/made/"

CLASSES = {
    "hand": dict(
        label="Made by hand",
        title="Made by hand: written by hand, mostly before 2026; since then agents have only cleaned up or fixed bugs",
        bg="#f7eddc", border="#d8b37c", ink="#5a3810",
        # a pen nib
        glyph='<path d="M2.5 9.5l.7-2.6L8 2.1l1.9 1.9-4.8 4.8z" fill="none" stroke="currentColor" stroke-width="1.3" stroke-linejoin="round"/><path d="M7 3.1l1.9 1.9" stroke="currentColor" stroke-width="1.3"/>',
    ),
    "with-agents": dict(
        label="Made with agents",
        title="Made with agents: built by a person and AI agents working together",
        bg="#eeeafa", border="#b4a6e4", ink="#3b2b7c",
        # two overlapping circles, one solid
        glyph='<circle cx="4.4" cy="6" r="3.1" fill="none" stroke="currentColor" stroke-width="1.3"/><circle cx="7.6" cy="6" r="3.1" fill="currentColor" fill-opacity=".55" stroke="currentColor" stroke-width="1.3"/>',
    ),
    "agents": dict(
        label="Made by agents",
        title="Made by agents: built by AI agents from specs and configuration, with light human steering",
        bg="#e5f2f6", border="#93c1d3", ink="#123f52",
        # a four-point spark
        glyph='<path d="M6 1.2c.4 2.4 1.6 3.6 4 4.8-2.4 1.2-3.6 2.4-4 4.8-.4-2.4-1.6-3.6-4-4.8 2.4-1.2 3.6-2.4 4-4.8z" fill="currentColor"/>',
    ),
}


def meta(kind: str) -> str:
    assert kind in CLASSES, kind
    return f'<meta name="{META}" content="{kind}">'


def badge(kind: str) -> str:
    c = CLASSES[kind]
    a = ("display:inline-flex;align-items:center;gap:6px;box-sizing:border-box;height:24px;margin:0;"
         "padding:0 10px 0 8px;border-radius:999px;border:1px solid {border};background:{bg};color:{ink};"
         "font:600 12px/1 system-ui,-apple-system,'Segoe UI',Roboto,sans-serif;letter-spacing:0;"
         "text-decoration:none;text-transform:none;white-space:nowrap;vertical-align:middle").format(**c)
    s = "display:block;flex:none;width:12px;height:12px;margin:0;color:inherit"
    return (f'<a class="skh-made" data-made="{kind}" href="{URL}" title="{c["title"]}" style="{a}">'
            f'<svg aria-hidden="true" focusable="false" viewBox="0 0 12 12" width="12" height="12" style="{s}">'
            f'{c["glyph"]}</svg>{c["label"]}</a>')


def main(argv):
    if argv == ["--all"]:
        for k in CLASSES:
            print(f"<!-- {k} -->\n{meta(k)}\n{badge(k)}\n")
        return 0
    if not argv or argv[0] not in CLASSES or argv[1:] not in ([], ["--meta"]):
        print(__doc__, file=sys.stderr)
        return 2
    print(meta(argv[0]) if argv[1:] == ["--meta"] else badge(argv[0]))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
