#!/usr/bin/env python3
"""Generate /all: an index of every site served under a.skh.am/<name>/.

Two sources, both derived rather than registered:

* Pages — every static/<slug>/index.html in this repository (except /all).
  Title, description and favicon come from that file; no network is needed.
* Repos — every harryaskham/* repository whose GitHub Pages site is served at
  exactly https://a.skh.am/<repo>/ (the user site at / is excluded by the rule).
  Discovery needs the GitHub API, so the last result is kept in
  _tools/site-index.json and only replaced by an explicit --refresh.

    python3 _tools/site-index.py            # render static/all/index.html (offline)
    python3 _tools/site-index.py --refresh  # rediscover repos + live metadata, then render
    python3 _tools/site-index.py --check    # CI: fail if the rendered page is stale

The rendered page depends only on what is indexed (no dates), so an unchanged
refresh is byte-identical: the daily check compares it with gh-pages and
deploys only on a real difference.

--refresh uses `gh api`. With the owner's token it sees private repos and exact
Pages URLs; with any other token (e.g. Actions' GITHUB_TOKEN) it discovers
public repos only and keeps snapshot entries it cannot see. Network failures
never fail a deploy: the snapshot is kept and a warning is printed.
"""
import argparse
import concurrent.futures as cf
import datetime as dt
from html import escape, unescape
from html.parser import HTMLParser
import json
from pathlib import Path
import re
import subprocess
import sys
import time
import urllib.error
import urllib.request
from urllib.parse import urljoin

REPO = Path(__file__).resolve().parents[1]
STATIC = REPO / "static"
OUT = STATIC / "all" / "index.html"
SNAPSHOT = Path(__file__).with_suffix(".json")
OWNER = "harryaskham"
HOST = "https://a.skh.am"
SELF = "all"
SOURCE = f"https://github.com/{OWNER}/{OWNER}.github.io/tree/main/static"


# ── metadata ────────────────────────────────────────────────────────────────

class Meta(HTMLParser):
    """Collect <title>, description and the best favicon from a page head."""

    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.title, self.description, self.icons = "", "", []
        self._in_title = self._seen_title = self._in_svg = False

    def handle_starttag(self, tag, attrs):
        a = {k.lower(): (v or "") for k, v in attrs}
        if tag == "svg":
            self._in_svg = True
        elif tag == "title" and not self._in_svg and not self._seen_title:
            self._in_title = True  # the document title only, never inline-SVG <title>s
        elif tag == "meta" and a.get("name", a.get("property", "")).lower() in ("description", "og:description"):
            self.description = self.description or a.get("content", "")
        elif tag == "link" and a.get("href"):
            rel = a.get("rel", "").lower().split()
            if "icon" in rel:
                self.icons.insert(0, a["href"])
            elif "apple-touch-icon" in rel:
                self.icons.append(a["href"])

    def handle_endtag(self, tag):
        if tag == "svg":
            self._in_svg = False
        elif tag == "title" and self._in_title:
            self._in_title, self._seen_title = False, True

    def handle_data(self, data):
        if self._in_title:
            self.title += data


def tidy(text):
    text = re.sub(r"\uFFFD+", "—", unescape(text or ""))  # upstream mojibake, e.g. a mangled em dash
    return re.sub(r"\s+", " ", text).strip()


def parse(html, base):
    m = Meta()
    m.feed(html)
    icon = next((urljoin(base, i) for i in m.icons if not i.startswith("data:")), "")
    return {"title": tidy(m.title), "description": tidy(m.description), "icon": icon}


def local_pages():
    pages = []
    for site in sorted(p for p in STATIC.iterdir() if p.is_dir() and p.name != SELF):
        index = site / "index.html"
        if not index.is_file():
            continue
        meta = parse(index.read_text(errors="replace"), f"{HOST}/{site.name}/")
        pages.append({"name": site.name, **meta, "live": True, "source": f"{SOURCE}/{site.name}"})
    return pages


# ── discovery (--refresh) ───────────────────────────────────────────────────

def gh(*args):
    out = subprocess.run(["gh", "api", *args], capture_output=True, text=True, timeout=60)
    if out.returncode:
        raise RuntimeError(out.stderr.strip() or f"gh api {' '.join(args)} failed")
    return out.stdout


def fetch(url, tries=3):
    """GET a page; return (live, html). Live means 200 at this exact path, no off-path redirect.

    4xx is a definite answer. Network errors and 5xx are retried so a momentary
    blip cannot flip a site offline (and so trigger a pointless deploy)."""
    req = urllib.request.Request(url, headers={"User-Agent": "a.skh.am site-index"})
    for attempt in range(tries):
        try:
            with urllib.request.urlopen(req, timeout=20) as r:
                final = r.geturl().split("?")[0].split("#")[0]
                body = r.read(512_000).decode(r.headers.get_content_charset() or "utf-8", errors="replace")
                return final.rstrip("/") == url.rstrip("/"), body
        except urllib.error.HTTPError as e:
            if e.code < 500:
                return False, ""
        except (urllib.error.URLError, TimeoutError, ConnectionError, OSError):
            pass
        time.sleep(2 * (attempt + 1))
    return False, ""


def discover(snapshot):
    """Return [{name, description, exact}] for repos whose Pages site is a.skh.am/<name>/."""
    try:
        me = gh("user", "--jq", ".login").strip()
    except RuntimeError:
        me = ""
    owner = me.lower() == OWNER.lower()
    listing = f"user/repos?per_page=100&affiliation=owner&visibility=all" if owner else f"users/{OWNER}/repos?per_page=100&type=owner"
    repos = [json.loads(line) for line in gh("--paginate", listing, "--jq", ".[] | select(.has_pages) | {name, description}").splitlines() if line]
    repos = [r for r in repos if not r["name"].lower().endswith(".github.io")]

    def exact(r):
        try:
            url = gh(f"repos/{OWNER}/{r['name']}/pages", "--jq", ".html_url").strip()
        except RuntimeError:
            return None  # Pages metadata not visible to this token; decide by HTTP below
        return re.sub(r"^http:", "https:", url).rstrip("/").lower() == f"{HOST}/{r['name']}".lower()

    with cf.ThreadPoolExecutor(8) as pool:
        for r, ok in zip(repos, pool.map(exact, repos)):
            r["exact"] = ok
    found = [r for r in repos if r["exact"] is not False]
    if not owner:  # cannot see private repos: keep what the snapshot already knew
        seen = {r["name"] for r in found}
        found += [{"name": r["name"], "description": "", "exact": None} for r in snapshot.get("repos", []) if r["name"] not in seen]
    return found, owner


def refresh(snapshot):
    found, owner = discover(snapshot)
    old = {r["name"]: r for r in snapshot.get("repos", [])}

    def describe(r):
        url = f"{HOST}/{r['name']}/"
        live, html = fetch(url)
        if not live and r["exact"] is None and r["name"] not in old:
            return None  # unverified public repo that isn't serving here (e.g. its own domain)
        meta = parse(html, url) if live else {"title": "", "description": "", "icon": ""}
        if meta["icon"] and not fetch(meta["icon"])[0]:
            meta["icon"] = ""
        prev = old.get(r["name"], {})
        return {
            "name": r["name"],
            "title": meta["title"] or prev.get("title") or r["name"],
            "description": meta["description"] or tidy(r.get("description")) or prev.get("description", ""),
            "icon": meta["icon"] or (prev.get("icon", "") if not live else ""),
            "live": live,
            "source": f"https://github.com/{OWNER}/{r['name']}",
        }

    with cf.ThreadPoolExecutor(8) as pool:
        repos = sorted((r for r in pool.map(describe, found) if r), key=lambda r: r["name"].lower())
    same = repos == snapshot.get("repos")
    return {
        "refreshed": snapshot.get("refreshed") if same else dt.date.today().isoformat(),
        "complete": owner,
        "repos": repos,
    }


# ── rendering ───────────────────────────────────────────────────────────────

ICONS = """<svg xmlns="http://www.w3.org/2000/svg" hidden>
  <symbol id="i-out" viewBox="0 0 24 24"><path d="M14 5h5v5M19 5l-8 8M17 14v4a1 1 0 0 1-1 1H6a1 1 0 0 1-1-1V8a1 1 0 0 1 1-1h4"/></symbol>
  <symbol id="i-find" viewBox="0 0 24 24"><circle cx="10.5" cy="10.5" r="5.5"/><path d="M15 15l4.5 4.5"/></symbol>
  <symbol id="i-reload" viewBox="0 0 24 24"><path d="M19 12a7 7 0 1 1-2.05-4.95M19 5v4h-4"/></symbol>
  <symbol id="i-code" viewBox="0 0 24 24"><path d="M9 7l-5 5 5 5M15 7l5 5-5 5"/></symbol>
  <symbol id="i-back" viewBox="0 0 24 24"><path d="M15 5l-7 7 7 7"/></symbol>
  <symbol id="i-rail" viewBox="0 0 24 24"><rect x="4" y="5" width="16" height="14" rx="1.5"/><path d="M10 5v14"/></symbol>
</svg>"""


def item(site):
    name, url = site["name"], f"{HOST}/{site['name']}/"
    title = site["title"] or name
    if title.lower() == name.lower() and site["description"]:
        title = site["description"]  # a title that only repeats the path says nothing
    icon = (f'<img src="{escape(site["icon"])}" alt="" width="16" height="16" loading="lazy" decoding="async">'
            if site["icon"] else "")
    state = "" if site["live"] else ' data-offline=""'
    note = "" if site["live"] else '<span class="off" title="Not serving">offline</span>'
    return (
        f'      <li data-name="{escape(name)}" data-source="{escape(site["source"])}"{state}'
        f' data-find="{escape((name + " " + title + " " + site["description"]).lower())}">'
        f'<a class="go" href="{escape(url)}" target="view" title="{escape(site["description"] or title)}">'
        f'<span class="ico" data-letter="{escape(name[:1].upper())}">{icon}</span>'
        f'<span class="path">/{escape(name)}</span>{note}<span class="title">{escape(title)}</span></a>'
        f'<a class="out" href="{escape(url)}" target="_blank" rel="noopener" aria-label="Open /{escape(name)} in a new tab" title="New tab">'
        f'<svg aria-hidden="true"><use href="#i-out"/></svg></a></li>'
    )


def group(key, label, sites):
    rows = "\n".join(item(s) for s in sites)
    return (f'    <section class="group" aria-labelledby="g-{key}"><h2 id="g-{key}">{label}<span>{len(sites)}</span></h2>\n'
            f'      <ul>\n{rows}\n      </ul>\n    </section>')


def render(snapshot):
    pages, repos = local_pages(), snapshot.get("repos", [])
    total = len(pages) + len(repos)
    return f"""<!doctype html>
<!-- Generated by _tools/site-index.py — edit the generator, style.css or app.js, not this file. -->
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1,viewport-fit=cover">
<meta name="color-scheme" content="light dark">
<meta name="theme-color" content="#f4f1ea" media="(prefers-color-scheme: light)">
<meta name="theme-color" content="#16171a" media="(prefers-color-scheme: dark)">
<title>a.skh.am/all</title>
<link rel="icon" href="icon.svg" type="image/svg+xml">
<link rel="stylesheet" href="style.css">
</head>
<body>
{ICONS}
<div class="shell">
  <nav class="index" aria-label="Sites">
    <header class="head">
      <span class="brand">a.skh.am<b>/all</b></span>
      <label class="find"><svg aria-hidden="true"><use href="#i-find"/></svg><input id="q" type="search" placeholder="Filter" aria-label="Filter sites" autocomplete="off" spellcheck="false"><kbd>/</kbd></label>
    </header>
    <div class="lists" id="lists">
{group("pages", "Pages", pages)}
{group("repos", "Repos", repos)}
      <p class="none" id="none" hidden>No match</p>
    </div>
    <footer class="foot"><span>{total} sites</span></footer>
  </nav>
  <div class="split" id="split" role="separator" aria-orientation="vertical" aria-label="Resize index" tabindex="0"></div>
  <main class="viewer" id="viewer">
    <header class="bar">
      <button class="icon back" id="back" type="button" aria-label="Back to index" title="Index"><svg aria-hidden="true"><use href="#i-back"/></svg></button>
      <button class="icon rail" id="rail" type="button" aria-label="Toggle index" title="Toggle index ([)"><svg aria-hidden="true"><use href="#i-rail"/></svg></button>
      <a class="where" id="where" href="#" target="view"><span class="ico" id="where-ico"></span><span class="path" id="where-path"></span><span class="title" id="where-title"></span></a>
      <span class="tools">
        <button class="icon" id="reload" type="button" aria-label="Reload" title="Reload"><svg aria-hidden="true"><use href="#i-reload"/></svg></button>
        <a class="icon" id="source" href="#" target="_blank" rel="noopener" aria-label="Source" title="Source"><svg aria-hidden="true"><use href="#i-code"/></svg></a>
        <a class="icon" id="open" href="#" target="_blank" rel="noopener" aria-label="Open in a new tab" title="New tab"><svg aria-hidden="true"><use href="#i-out"/></svg></a>
      </span>
      <i class="progress" id="progress" aria-hidden="true"></i>
    </header>
    <div class="stage">
      <div class="empty" id="empty"><p class="mark">a.skh.am/<span>…</span></p><p class="keys"><span><kbd>/</kbd> filter</span><span><kbd>↑</kbd><kbd>↓</kbd> move</span><span><kbd>↵</kbd> open</span><span><kbd>[</kbd> index</span></p></div>
      <iframe name="view" id="view" title="Selected site" allow="microphone; camera; clipboard-write; fullscreen; autoplay; web-share"></iframe>
    </div>
  </main>
</div>
<script src="app.js"></script>
</body>
</html>
"""


# ── entry point ─────────────────────────────────────────────────────────────

def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    mode = ap.add_mutually_exclusive_group()
    mode.add_argument("--refresh", action="store_true", help="rediscover repo sites and live metadata (network)")
    mode.add_argument("--check", action="store_true", help="fail if static/all/index.html is stale")
    ap.add_argument("--strict", action="store_true", help="with --refresh: fail unless discovery was complete")
    args = ap.parse_args()
    snapshot = json.loads(SNAPSHOT.read_text()) if SNAPSHOT.is_file() else {"repos": []}

    if args.check:
        if not OUT.is_file() or OUT.read_text() != render(snapshot):
            sys.exit("static/all/index.html is stale: run python3 _tools/site-index.py")
        print("site-index: /all is current")
        return

    if args.refresh:
        try:
            fresh = refresh(snapshot)
        except Exception as exc:  # never block a deploy on discovery
            if args.strict:
                sys.exit(f"site-index: refresh failed: {exc}")
            print(f"site-index: refresh failed, keeping snapshot: {exc}", file=sys.stderr)
        else:
            if not fresh.pop("complete"):
                if args.strict:
                    sys.exit("site-index: token cannot see private repos (is SITE_INDEX_TOKEN valid?)")
                print("site-index: token cannot see private repos; kept snapshot entries it could not verify", file=sys.stderr)
            snapshot = fresh
            SNAPSHOT.write_text(json.dumps(snapshot, indent=2, ensure_ascii=False) + "\n")

    OUT.parent.mkdir(exist_ok=True)
    OUT.write_text(render(snapshot))
    live = sum(r["live"] for r in snapshot["repos"])
    print(f"site-index: {len(local_pages())} pages + {len(snapshot['repos'])} repos ({live} live) → {OUT.relative_to(REPO)}")


if __name__ == "__main__":
    main()
