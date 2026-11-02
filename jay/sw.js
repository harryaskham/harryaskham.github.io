// jay-build:7e8cfe1543
// Jay · service worker. Serves one complete, consistent build of the app shell
// from a per-build cache (never a mix of old and new files), keeps working
// offline, and adds COOP/COEP so ONNX Runtime can use WebAssembly threads.
const BUILD = "jay-build:7e8cfe1543";
const SHELL = "jay-shell-" + BUILD.split(":")[1];
const SCOPE = new URL("./", self.location.href).pathname;
const STAMPED = ["index.html", "style.css", "app.js", "db.js", "audio.js", "dsp.js", "worker.js", "capture-worklet.js"];
const FILES = [...STAMPED, "runtime/ort.js", "runtime/manifest.json", "models/moonshine-tiny/manifest.json",
  "manifest.webmanifest", "icon.svg", "icon-192.png"];
const abs = (f) => new URL(f, self.location.href).href;

self.addEventListener("install", (e) => {
  e.waitUntil((async () => {
    const cache = await caches.open(SHELL);
    try {
      for (const f of FILES) {
        const res = await fetch(new Request(abs(f), { cache: "no-cache" }));
        if (!res.ok) throw new Error(`${f}: HTTP ${res.status}`);
        // A deploy still propagating through the CDN can mix builds; refuse it and retry later.
        if (STAMPED.includes(f) && !(await res.clone().text()).includes(BUILD)) throw new Error(`${f} is from another build`);
        await cache.put(abs(f), res);
      }
    } catch (err) {
      await caches.delete(SHELL);
      throw err;
    }
    await self.skipWaiting();
  })());
});

self.addEventListener("activate", (e) => {
  e.waitUntil((async () => {
    const keys = await caches.keys();
    // Builds before stamping (jay-shell-v1/v2) could serve stale HTML with new scripts; move their pages over now.
    const legacy = keys.some((k) => /^jay-shell-v\d+$/.test(k));
    for (const k of keys) if (k.startsWith("jay-shell-") && k !== SHELL) await caches.delete(k);
    await self.clients.claim();
    if (legacy) for (const c of await self.clients.matchAll({ type: "window" })) c.navigate(c.url).catch(() => {});
  })());
});

function isolate(res) {
  if (!res || res.status === 0 || res.type === "opaque") return res;
  const h = new Headers(res.headers);
  // fetch() already decoded the body: drop headers that describe the compressed transfer.
  h.delete("content-encoding"); h.delete("content-length");
  h.set("Cross-Origin-Embedder-Policy", "require-corp");
  h.set("Cross-Origin-Opener-Policy", "same-origin");
  h.set("Cross-Origin-Resource-Policy", "same-origin");
  return new Response(res.body, { status: res.status, statusText: res.statusText, headers: h });
}

async function serve(req, url) {
  const rel = url.pathname.slice(SCOPE.length);
  const key = req.mode === "navigate" && (rel === "" || rel === "index.html") ? abs("index.html") : url.origin + url.pathname;
  const cache = await caches.open(SHELL);
  const hit = await cache.match(key);
  if (hit) return hit;
  const res = await fetch(req);
  if (res.ok && !res.redirected && req.mode !== "navigate") cache.put(key, res.clone());
  return res;
}

// Android share sheet → "Jay": stash the shared audio, then open the app to transcribe it.
async function share(req) {
  try {
    const form = await req.formData();
    const cache = await caches.open("jay-share");
    let i = 0;
    for (const f of form.getAll("audio")) {
      if (!(f instanceof Blob) || !f.size) continue;
      await cache.put(abs(`share/${Date.now()}-${i++}`), new Response(f, { headers: { "content-type": f.type || "application/octet-stream", "x-name": encodeURIComponent(f.name || "shared audio") } }));
    }
  } catch {}
  return Response.redirect(abs("./?shared=1"), 303);
}

self.addEventListener("fetch", (e) => {
  const req = e.request;
  if (req.method === "POST" && new URL(req.url).pathname === SCOPE + "share") { e.respondWith(share(req)); return; }
  if (req.method !== "GET") return;
  const url = new URL(req.url);
  if (url.origin !== location.origin || !url.pathname.startsWith(SCOPE)) return;
  if (url.pathname.startsWith(SCOPE + "worklog")) return; // plain static page, not part of the app
  // Model shards are cached by the ASR worker itself; just add isolation headers.
  if (/\.bin$/.test(url.pathname) || url.pathname.includes("/local-models/")) {
    e.respondWith(fetch(req).then(isolate));
    return;
  }
  e.respondWith(serve(req, url).then(isolate).catch(() => new Response("Offline", { status: 503 })));
});

self.addEventListener("message", (e) => { if (e.data === "build") e.source?.postMessage({ build: BUILD }); });
