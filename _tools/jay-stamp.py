#!/usr/bin/env python3
"""Stamp Jay's app shell with one content-derived build id.

Every shell file carries the same `jay-build:<id>` token. The service worker
installs a deploy atomically (and refuses a half-propagated one), and the page
heals itself if its HTML and JavaScript ever come from different builds.

    python3 _tools/jay-stamp.py          # restamp after editing static/jay/
    python3 _tools/jay-stamp.py --check  # CI: fail if the stamp is stale
"""
import argparse
import hashlib
import re
import sys
from pathlib import Path

APP = Path(__file__).resolve().parents[1] / "static" / "jay"
STAMPED = ["index.html", "style.css", "app.js", "db.js", "audio.js", "dsp.js", "worker.js", "capture-worklet.js", "sw.js"]
HASHED = STAMPED + ["manifest.webmanifest", "runtime/ort.js", "runtime/manifest.json", "models/moonshine-tiny/manifest.json", "icon.svg"]
TOKEN = re.compile(r"jay-build:[0-9a-z]+")
HEAD = {".html": None, ".css": "/* {} */\n"}


def build_id():
    h = hashlib.sha256()
    for name in HASHED:
        h.update(name.encode() + b"\0" + TOKEN.sub("jay-build:", (APP / name).read_text()).encode() + b"\0")
    return h.hexdigest()[:10]


def stamped(name, text, bid):
    token = f"jay-build:{bid}"
    if TOKEN.search(text):
        return TOKEN.sub(token, text)
    if name.endswith(".html"):
        return text.replace("<meta charset=\"utf-8\">", f"<meta charset=\"utf-8\">\n  <meta name=\"jay-build\" content=\"{token}\">", 1)
    if name.endswith(".css"):
        return f"/* {token} */\n{text}"
    return f"// {token}\n{text}"


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--check", action="store_true")
    args = ap.parse_args()
    bid = build_id()
    stale = [n for n in STAMPED if stamped(n, (APP / n).read_text(), bid) != (APP / n).read_text()]
    if args.check:
        if stale:
            sys.exit(f"Jay build stamp is stale in {', '.join(stale)}: run python3 _tools/jay-stamp.py")
        print(f"Jay build stamp {bid} consistent across {len(STAMPED)} shell files")
        return
    for n in stale:
        (APP / n).write_text(stamped(n, (APP / n).read_text(), bid))
    print(f"Jay build {bid}" + (f" (restamped {len(stale)} files)" if stale else " (already current)"))


if __name__ == "__main__":
    main()
