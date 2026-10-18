#!/usr/bin/env python3
"""The "how it was made" badge every a.skh.am site carries, from one place.

    python3 _tools/made-badge.py agents          # print the footer seal for one class
    python3 _tools/made-badge.py agents --meta   # print the <meta> tag for the page head
    python3 _tools/made-badge.py agents --pill   # a compact one-line pill, where a seal will not fit
    python3 _tools/made-badge.py --all           # print every variant (for docs and review)

Classes:
    hand         Made by hand    — written by hand, mostly before 2026; since then agents have only cleaned up or fixed bugs
    with-agents  Made with agents — built by a person and AI agents working together
    agents       Made by agents  — built by AI agents from specs and configuration, with light human steering

Every page of a site declares its class in the head (<meta name="a.skh.am:made" content="…">), which
_tools/site-index.py reads for /all, and shows the seal in its footer: a starburst foil seal (gold, silver,
steel by class), 112px by default. Seal and pill style themselves inline (fonts, fills, sizes) so they look
the same on every site and theme and need no stylesheet or JavaScript; both link to https://a.skh.am/made/,
which explains the classes.
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


SEAL = {  # foil light / mid / dark, ink, and the centre wording (small line, big line)
    "hand": dict(light="#fbe7a1", mid="#d9a640", dark="#8a5a12", ink="#3f2604", small="MADE BY", big="HAND"),
    "with-agents": dict(light="#f1eefc", mid="#b3a6e2", dark="#5d4ca3", ink="#241a55", small="MADE WITH", big="AGENTS"),
    "agents": dict(light="#e4f6fa", mid="#86c2d4", dark="#2a6a80", ink="#0b3240", small="MADE BY", big="AGENTS"),
}


def seal(kind: str, size: int = 112) -> str:
    """The footer seal: a starburst foil seal in the manner of an old console maker's seal of quality.

    One self-contained inline SVG inside a link; every colour and font is set inline so no site
    stylesheet can restyle it. Gradient ids are namespaced by class, so several seals can share a page.
    """
    import math
    c, f = CLASSES[kind], SEAL[kind]
    k = f"skh-seal-{kind}"
    pts = []
    for i in range(72):  # 36 points, alternating radii
        r = 58 if i % 2 == 0 else 53.2
        t = math.pi * 2 * i / 72 - math.pi / 2
        pts.append(f"{60 + r * math.cos(t):.2f},{60 + r * math.sin(t):.2f}")
    star = " ".join(pts)
    font = "font-family:system-ui,-apple-system,'Segoe UI',Roboto,'Helvetica Neue',Arial,sans-serif;font-weight:800"

    def text(x, y, size_, body, extra="", anchor="middle", spacing=0.6):
        style = f"{font};font-size:{size_}px;letter-spacing:{spacing}px"
        hi = (f'<text x="{x}" y="{y + 0.7}" text-anchor="{anchor}" style="{style};fill:#fff;fill-opacity:.55" {extra}>{body}</text>')
        lo = f'<text x="{x}" y="{y}" text-anchor="{anchor}" style="{style};fill:{f["ink"]}" {extra}>{body}</text>'
        return hi + lo

    def arc(id_, d, body, size_=8.6):
        style = f"{font};font-size:{size_}px;letter-spacing:1.4px"
        return (f'<path id="{k}-{id_}" d="{d}" style="fill:none;stroke:none"/>'
                f'<text style="{style};fill:#fff;fill-opacity:.55" transform="translate(0 .7)"><textPath href="#{k}-{id_}" startOffset="50%" text-anchor="middle">{body}</textPath></text>'
                f'<text style="{style};fill:{f["ink"]}"><textPath href="#{k}-{id_}" startOffset="50%" text-anchor="middle">{body}</textPath></text>')

    svg = (
        f'<svg role="img" aria-label="{c["label"]}" viewBox="0 0 120 120" width="{size}" height="{size}" '
        f'style="display:block;width:{size}px;height:{size}px;max-width:none;margin:0;overflow:visible">'
        f'<defs>'
        f'<linearGradient id="{k}-foil" x1="0" y1="0" x2="1" y2="1">'
        f'<stop offset="0" stop-color="{f["light"]}"/><stop offset=".45" stop-color="{f["mid"]}"/>'
        f'<stop offset=".7" stop-color="{f["light"]}"/><stop offset="1" stop-color="{f["dark"]}"/></linearGradient>'
        f'<radialGradient id="{k}-disk" cx=".38" cy=".32" r=".8">'
        f'<stop offset="0" stop-color="{f["light"]}"/><stop offset=".6" stop-color="{f["mid"]}"/>'
        f'<stop offset="1" stop-color="{f["dark"]}"/></radialGradient>'
        f'</defs>'
        f'<polygon points="{star}" transform="translate(1.6 2.4)" style="fill:#000;fill-opacity:.32"/>'
        f'<polygon points="{star}" style="fill:url(#{k}-foil);stroke:{f["dark"]};stroke-width:.8"/>'
        f'<circle cx="60" cy="60" r="49.5" style="fill:url(#{k}-disk);stroke:{f["dark"]};stroke-width:1.4"/>'
        f'<circle cx="60" cy="60" r="48" style="fill:none;stroke:#fff;stroke-opacity:.5;stroke-width:.8"/>'
        f'<circle cx="60" cy="60" r="34.5" style="fill:none;stroke:{f["dark"]};stroke-width:1"/>'
        f'<circle cx="60" cy="60" r="33.4" style="fill:none;stroke:#fff;stroke-opacity:.45;stroke-width:.6"/>'
        + arc("top", "M 19.5 60 A 40.5 40.5 0 0 1 100.5 60", "OFFICIAL · A.SKH.AM")
        + arc("bot", "M 14.6 60 A 45.4 45.4 0 0 0 105.4 60", "SEAL OF ORIGIN")
        + text(17.2, 63.2, 7, "★", spacing=0) + text(102.8, 63.2, 7, "★", spacing=0)
        + text(60, 55, 8.4, f["small"], spacing=1)
        + text(60, 73, 16 if len(f["big"]) <= 4 else 13.6, f["big"], spacing=0.4)
        + f'<ellipse cx="46" cy="40" rx="26" ry="14" transform="rotate(-28 46 40)" style="fill:#fff;fill-opacity:.16"/>'
        f'</svg>'
    )
    return (f'<a class="skh-made" data-made="{kind}" href="{URL}" title="{c["title"]}" '
            f'style="display:inline-block;margin:0;padding:0;border:0;background:none;line-height:0;text-decoration:none;vertical-align:middle">'
            f'{svg}</a>')


def main(argv):
    if argv == ["--all"]:
        for k in CLASSES:
            print(f"<!-- {k} -->\n{meta(k)}\n{seal(k)}\n")
        return 0
    if not argv or argv[0] not in CLASSES or argv[1:] not in ([], ["--meta"], ["--pill"]):
        print(__doc__, file=sys.stderr)
        return 2
    print(meta(argv[0]) if argv[1:] == ["--meta"] else badge(argv[0]) if argv[1:] == ["--pill"] else seal(argv[0]))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
