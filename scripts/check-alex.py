#!/usr/bin/env python3
"""Validate the self-contained /alex page and its compressed audio before push."""
from pathlib import Path
from html.parser import HTMLParser
import hashlib
import json
import re

ROOT = Path(__file__).resolve().parents[1] / "alex"


class Links(HTMLParser):
    def handle_starttag(self, tag, attrs):
        for key, value in attrs:
            if key not in ("src", "href") or not value or value.startswith("data:"):
                continue
            if tag == "base":
                assert value == "/alex/"
                continue
            assert not value.startswith(("/", "http:", "https:")), value
            assert (ROOT / value).is_file(), value


def main():
    files = list(ROOT.rglob("*"))
    assert not any(f.is_symlink() for f in files), "Symlinks are not publishable assets"
    files = [f for f in files if f.is_file()]
    assert sum(f.stat().st_size for f in files) < 2_000_000, "Page exceeds 2 MB"
    html = (ROOT / "index.html").read_text()
    assert not html.startswith("---"), "Keep this static page outside Jekyll navigation/layouts"
    Links().feed(html)
    tracks = json.loads((ROOT / "tracks.json").read_text())
    assert len(tracks) == len(set(tracks)) == 50
    for track in tracks:
        assert re.fullmatch(r"audio/[0-9a-f]{16}\.mp3", track), track
        data = (ROOT / track).read_bytes()
        assert 1000 < len(data) < 200_000, track
        assert data.startswith(b"ID3") or data[0] == 0xFF, track
        assert hashlib.sha256(data).hexdigest()[:16] == Path(track).stem, track
    assert {str(p.relative_to(ROOT)) for p in files} == set(tracks) | {
        "index.html", "style.css", "player.js", "tracks.json"
    }, "Unexpected public file"
    print(f"Alex: {len(tracks)} compressed clips; {sum(f.stat().st_size for f in files):,} bytes; links and hashes valid")


if __name__ == "__main__":
    main()
