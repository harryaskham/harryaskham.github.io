// Jay · service worker. Offline app shell + cross-origin isolation headers
// (COOP/COEP) so ONNX Runtime can use WebAssembly threads on static hosting.
const SHELL = "jay-shell-v2";
const SCOPE = new URL("./", self.location.href).pathname;
const FILES = ["./", "index.html", "style.css", "app.js", "db.js", "audio.js", "dsp.js", "worker.js", "capture-worklet.js",
  "runtime/ort.js", "runtime/manifest.json", "models/moonshine-tiny/manifest.json", "icon.svg", "icon-192.png", "manifest.webmanifest"];

self.addEventListener("install", (e) => {
  e.waitUntil(caches.open(SHELL).then((c) => c.addAll(FILES.map((f) => new Request(f, { cache: "reload" })))).catch(() => {}).then(() => self.skipWaiting()));
});
self.addEventListener("activate", (e) => {
  e.waitUntil((async () => {
    for (const k of await caches.keys()) if (k.startsWith("jay-shell-") && k !== SHELL) await caches.delete(k);
    await self.clients.claim();
  })());
});

function isolate(res) {
  if (!res || res.status === 0 || res.type === "opaque") return res;
  const h = new Headers(res.headers);
  h.set("Cross-Origin-Embedder-Policy", "require-corp");
  h.set("Cross-Origin-Opener-Policy", "same-origin");
  h.set("Cross-Origin-Resource-Policy", "same-origin");
  return new Response(res.body, { status: res.status, statusText: res.statusText, headers: h });
}

async function shell(req) {
  const cache = await caches.open(SHELL);
  const url = new URL(req.url);
  const root = url.pathname === SCOPE || url.pathname === SCOPE + "index.html";
  const key = req.mode === "navigate" ? (root ? "./" : url.origin + url.pathname) : req;
  const net = fetch(req, { cache: "no-cache" }).then((res) => {
    if (res.ok && !res.redirected) cache.put(key, res.clone());
    return res;
  });
  const hit = await cache.match(key, { ignoreSearch: true });
  if (!hit) return net;
  // Network first, but never make the user wait long on a flaky connection.
  return Promise.race([net.catch(() => hit), new Promise((r) => setTimeout(() => r(hit), 2500))]).then((r) => r || hit);
}

self.addEventListener("fetch", (e) => {
  const req = e.request;
  if (req.method !== "GET") return;
  const url = new URL(req.url);
  if (url.origin !== location.origin || !url.pathname.startsWith(SCOPE)) return;
  if (url.pathname.startsWith(SCOPE + "worklog")) return; // plain static page, not part of the app
  // Model shards are cached by the ASR worker itself; just add isolation headers.
  if (/\.bin$/.test(url.pathname) || url.pathname.includes("/local-models/")) {
    e.respondWith(fetch(req).then(isolate));
    return;
  }
  e.respondWith(shell(req).then(isolate).catch(() => new Response("Offline", { status: 503 })));
});
