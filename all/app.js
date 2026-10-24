// a.skh.am/all — hash-routed viewer for the generated site index.
(() => {
  "use strict";
  const $ = (s, el = document) => el.querySelector(s);
  const $$ = (s, el = document) => [...el.querySelectorAll(s)];
  const body = document.body;
  const view = $("#view");
  const q = $("#q");
  const items = $$(".group li");
  const byName = new Map(items.map((li) => [li.dataset.name, li]));
  const store = { get: (k) => { try { return localStorage.getItem(k); } catch { return null; } },
                  set: (k, v) => { try { localStorage.setItem(k, v); } catch {} } };
  const narrow = matchMedia("(max-width: 720px)");
  let current = null;
  let navigated = false; // whether this page pushed a hash entry (so Back can pop it)

  // ── Favicons: a broken icon falls back to the monogram ──
  const dropIcon = (img) => img.remove();
  $$(".ico img").forEach((img) => {
    if (img.complete && !img.naturalWidth) dropIcon(img);
    else img.addEventListener("error", () => dropIcon(img), { once: true });
  });

  // ── Selection ──
  const nameFromHash = () => decodeURIComponent(location.hash.replace(/^#\/?/, "")).replace(/\/+$/, "");

  function select(name) {
    const li = byName.get(name) || null;
    if (current) current.classList.remove("on");
    current = li;
    if (!li) {
      body.classList.remove("viewing", "loading");
      if (view.dataset.url) show("about:blank");
      delete view.dataset.url;
      document.title = "a.skh.am/all";
      return;
    }
    li.classList.add("on");
    const go = $(".go", li);
    const url = go.href;
    body.classList.add("viewing");
    if (view.dataset.url !== url) {
      body.classList.add("loading");
      show(url);
    }
    $("#where").href = url;
    $("#where-path").textContent = `/${name}`;
    $("#where-title").textContent = $(".title", li).textContent;
    $("#where-ico").replaceChildren(...[...$(".ico", li).childNodes].map((n) => n.cloneNode(true)));
    $("#where-ico").dataset.letter = $(".ico", li).dataset.letter;
    $("#open").href = url;
    const source = $("#source");
    source.href = li.dataset.source || "#";
    source.hidden = !li.dataset.source;
    document.title = `/${name} · a.skh.am/all`;
    if (!narrow.matches) li.scrollIntoView({ block: "nearest" });
  }

  // Swap the frame's document without adding a joint-history entry, so browser
  // Back walks the selection hash rather than stepping through frame loads.
  function show(url) {
    view.dataset.url = url;
    try { view.contentWindow.location.replace(url); }
    catch { view.src = url; } // cross-origin current document (e.g. local dev)
  }

  function open(name) {
    if (nameFromHash() === name) return select(name);
    navigated = true;
    location.hash = `#/${name}`;
  }

  addEventListener("hashchange", () => select(nameFromHash()));

  // Plain clicks open in the viewer; modified/middle clicks keep browser behaviour.
  $("#lists").addEventListener("click", (e) => {
    const go = e.target.closest(".go");
    if (!go || e.button !== 0 || e.metaKey || e.ctrlKey || e.shiftKey || e.altKey) return;
    e.preventDefault();
    open(go.closest("li").dataset.name);
  });

  // Same-origin pages: follow in-frame navigation in the bar and the new-tab link.
  view.addEventListener("load", () => {
    body.classList.remove("loading");
    if (!current) return;
    try {
      const loc = view.contentWindow.location;
      if (loc.href === "about:blank") return;
      $("#open").href = loc.href;
      $("#where").href = loc.href;
      const sub = loc.pathname.replace(/^\/[^/]+\/?/, "");
      $("#where-path").textContent = `/${current.dataset.name}${sub ? "/" + sub : ""}`;
      const t = view.contentDocument.title.trim();
      if (t) $("#where-title").textContent = t;
    } catch { /* cross-origin (e.g. local dev): keep the indexed URL */ }
  });

  $("#reload").addEventListener("click", () => {
    if (!current) return;
    body.classList.add("loading");
    try { view.contentWindow.location.reload(); } catch { view.src = view.dataset.url; }
  });

  $("#back").addEventListener("click", () => {
    if (navigated) history.back();
    else { history.pushState(null, "", location.pathname + location.search); select(""); }
  });

  // ── Filter ──
  function visible() { return items.filter((li) => !li.hidden); }

  function filter() {
    const terms = q.value.toLowerCase().trim().split(/\s+/).filter(Boolean);
    items.forEach((li) => { li.hidden = !terms.every((t) => li.dataset.find.includes(t)); });
    $$(".group").forEach((g) => { g.hidden = !$$("li", g).some((li) => !li.hidden); });
    $("#none").hidden = visible().length > 0;
  }
  q.addEventListener("input", filter);

  q.addEventListener("keydown", (e) => {
    if (e.key === "ArrowDown") { e.preventDefault(); const first = visible()[0]; if (first) $(".go", first).focus(); }
    else if (e.key === "Enter") {
      const first = visible()[0];
      if (!first) return;
      e.preventDefault();
      if (e.metaKey || e.ctrlKey) window.open($(".go", first).href, "_blank", "noopener");
      else open(first.dataset.name);
    } else if (e.key === "Escape") {
      if (q.value) { q.value = ""; filter(); } else q.blur();
    }
  });

  // ── List keyboard: arrows move between visible rows; typing returns to the filter ──
  $("#lists").addEventListener("keydown", (e) => {
    const li = e.target.closest("li");
    if (!li) return;
    const rows = visible();
    const i = rows.indexOf(li);
    if (e.key === "ArrowDown" || e.key === "ArrowUp") {
      e.preventDefault();
      const next = rows[i + (e.key === "ArrowDown" ? 1 : -1)];
      if (next) $(".go", next).focus(); else if (e.key === "ArrowUp") q.focus();
    } else if (e.key.length === 1 && /\S/.test(e.key) && !e.metaKey && !e.ctrlKey && !e.altKey && e.key !== "[") {
      q.focus();
    }
  });

  // ── Global keys ──
  addEventListener("keydown", (e) => {
    const typing = e.target.matches("input, textarea, [contenteditable]");
    if (e.key === "/" && !typing) { e.preventDefault(); q.focus(); q.select(); }
    else if (e.key === "[" && !typing && !e.metaKey && !e.ctrlKey) { e.preventDefault(); toggleRail(); }
  });

  // ── Index rail: collapse and resize ──
  function toggleRail(force) {
    const off = body.classList.toggle("norail", force);
    store.set("all.rail.hidden", off ? "1" : "");
    $("#rail").setAttribute("aria-pressed", String(!off));
  }
  $("#rail").addEventListener("click", () => toggleRail());

  const root = document.documentElement;
  const clamp = (w) => Math.round(Math.max(220, Math.min(w, 560, innerWidth - 320)));
  const setWidth = (w) => { root.style.setProperty("--rail", `${clamp(w)}px`); };
  const saved = Number(store.get("all.rail.width"));
  if (saved) setWidth(saved);
  if (store.get("all.rail.hidden")) toggleRail(true);

  const split = $("#split");
  split.addEventListener("pointerdown", (e) => {
    e.preventDefault();
    split.setPointerCapture(e.pointerId);
    body.classList.add("dragging");
  });
  split.addEventListener("pointermove", (e) => { if (body.classList.contains("dragging")) setWidth(e.clientX); });
  const endDrag = () => {
    if (!body.classList.contains("dragging")) return;
    body.classList.remove("dragging");
    store.set("all.rail.width", parseInt(getComputedStyle(root).getPropertyValue("--rail"), 10));
  };
  split.addEventListener("pointerup", endDrag);
  split.addEventListener("pointercancel", endDrag);
  split.addEventListener("dblclick", () => { root.style.removeProperty("--rail"); store.set("all.rail.width", ""); });
  split.addEventListener("keydown", (e) => {
    if (e.key !== "ArrowLeft" && e.key !== "ArrowRight") return;
    e.preventDefault();
    const now = $(".index").getBoundingClientRect().width;
    setWidth(now + (e.key === "ArrowRight" ? 24 : -24));
    store.set("all.rail.width", parseInt(getComputedStyle(root).getPropertyValue("--rail"), 10));
  });

  select(nameFromHash());
  if (!current && !narrow.matches && !("ontouchstart" in window)) q.focus({ preventScroll: true });
})();
