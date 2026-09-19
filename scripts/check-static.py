#!/usr/bin/env python3
"""Check standalone source trees and, optionally, their root-mounted build output."""
import argparse
from html.parser import HTMLParser
from pathlib import Path
import re
import subprocess
import sys
from urllib.parse import unquote, urlsplit

REPO = Path(__file__).resolve().parents[1]
ROOT = REPO / "static"


def local_link(value, source, site):
    url = urlsplit(value)
    if url.scheme or url.netloc or not url.path:
        return
    assert not url.path.startswith("/"), f"Use preview-safe relative links: {source}: {value}"
    target = (source.parent / unquote(url.path)).resolve()
    assert target.is_relative_to(site), f"Link escapes subsite: {source}: {value}"
    assert target.is_file() or (target / "index.html").is_file(), f"Missing: {source}: {value}"


class Links(HTMLParser):
    def __init__(self, source, site):
        super().__init__()
        self.source, self.site = source, site

    def handle_starttag(self, tag, attrs):
        assert tag != "base", f"Use relative links, not <base>: {self.source}"
        for key, value in attrs:
            if not value:
                continue
            if key in ("href", "src", "poster"):
                local_link(value, self.source, self.site)
            elif key == "srcset":
                for candidate in value.split(","):
                    local_link(candidate.strip().split()[0], self.source, self.site)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--built", type=Path, help="Jekyll destination to check byte-for-byte")
    args = parser.parse_args()
    total = count = 0
    for site in sorted(ROOT.iterdir()):
        assert site.is_dir() and not site.is_symlink(), f"Only subsite directories belong in static/: {site}"
        assert (site / "index.html").is_file(), f"Missing index.html: {site}"
        for file in sorted(site.rglob("*")):
            assert not file.is_symlink(), f"Symlinks are not publishable: {file}"
            assert not any(p.startswith((".", "_")) for p in file.relative_to(ROOT).parts), f"Jekyll-hidden path: {file}"
            if not file.is_file():
                continue
            data = file.read_bytes()
            assert len(data) < 10_000_000, f"Asset exceeds 10 MB: {file}"
            total += len(data)
            count += 1
            if file.suffix == ".html":
                assert not data.startswith(b"---"), f"Standalone HTML must not have Jekyll front matter: {file}"
                Links(file, site).feed(data.decode())
            elif file.suffix == ".css":
                for value in re.findall(r"url\(['\"]?([^) '\"]+)", data.decode()):
                    local_link(value, file, site)
            if args.built:
                output = args.built / file.relative_to(ROOT)
                assert output.is_file(), f"Missing root-mounted output: {output}"
                assert data == output.read_bytes(), f"Static asset was changed: {output}"
    assert total < 25_000_000, "Static assets exceed 25 MB"
    if args.built:
        assert not (args.built / "static").exists(), "Unexpected /static/ output"
    subprocess.run([sys.executable, str(REPO / "scripts/check-alex.py")], check=True)
    print(f"Static sites: {count} files; {total:,} bytes; links and budgets valid" +
          ("; all files published unchanged at root" if args.built else ""))


if __name__ == "__main__":
    main()
