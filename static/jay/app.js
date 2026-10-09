// Jay · on-device medical transcription. UI, sessions, recording, uploads,
// search, history and export. All audio and transcripts stay in this browser.
import * as db from "./db.js";
import { Recorder, FakeRecorder, decodeFile, f32ToI16, concatI16, wavBlob, SR } from "./audio.js";

const BUILD = "jay-build:0237969105";
const $ = (s, el = document) => el.querySelector(s);
const $$ = (s, el = document) => [...el.querySelectorAll(s)];
const esc = (s) => String(s ?? "").replace(/[&<>"']/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" })[c]);
const uid = () => Date.now().toString(36) + Math.random().toString(36).slice(2, 8);
const icon = (n) => `<svg aria-hidden="true"><use href="#i-${n}"/></svg>`;
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const fmtDur = (s) => {
  s = Math.max(0, Math.round(s || 0));
  const h = Math.floor(s / 3600), m = Math.floor((s % 3600) / 60), x = s % 60;
  return h ? `${h}:${String(m).padStart(2, "0")}:${String(x).padStart(2, "0")}` : `${m}:${String(x).padStart(2, "0")}`;
};
const fmtBytes = (b) => (b < 1e6 ? `${Math.max(0, Math.round(b / 1e3))} KB` : b < 1e9 ? `${(b / 1e6).toFixed(b < 1e7 ? 1 : 0)} MB` : `${(b / 1e9).toFixed(2)} GB`);
const fmtTime = (t) => new Date(t).toLocaleTimeString([], { hour: "2-digit", minute: "2-digit" });
const fmtDay = (t) => new Date(t).toLocaleDateString([], { weekday: "short", day: "numeric", month: "short" });
const fmtWhen = (t) => {
  const d = new Date(t), now = new Date();
  if (d.toDateString() === now.toDateString()) return fmtTime(t);
  return `${fmtDay(t)} · ${fmtTime(t)}`;
};

// ── Models ───────────────────────────────────────────────────────────────
const HF = (repo, rev) => `https://huggingface.co/${repo}/resolve/${rev}/`;
const MOONSHINE_BASE = HF("onnx-community/moonshine-base-ONNX", "b1e9b6aae3c3c7298f10c3798393fdf38e8fbbad");
const MEDASR = HF("ysdede/medasr-onnx", "2baa5ece746ece2e7e5eb129797ca53ca0f5050d");
const BUILTIN = [
  {
    id: "moonshine-tiny", name: "Moonshine Tiny", short: "Tiny", tags: ["in"], kind: "moonshine", builtin: true, size: 24.3e6,
    blurb: "General English · instant, offline, bundled with Jay",
    manifest: "./models/moonshine-tiny/manifest.json", config: { decoder_num_hidden_layers: 6, decoder_num_key_value_heads: 8, headDim: 36 },
    license: "MIT · Useful Sensors", link: "https://huggingface.co/UsefulSensors/moonshine-tiny",
  },
  {
    id: "medasr", name: "MedASR", short: "MedASR", tags: ["med"], kind: "ctc", format: "medasr", size: 108.1e6,
    blurb: "Google's clinical speech model · radiology & medical dictation, spoken punctuation",
    files: {
      model: { url: MEDASR + "model_int8.onnx", sha256: "6672e7bf25ff6c7fa2f6b620dcae6127431614390665f08dd3ffbd9e72e23309", size: 108083856 },
      tokens: { url: MEDASR + "tokens.txt", size: 6516 },
    },
    license: "Health AI Developer Foundations terms · int8 ONNX", link: "https://huggingface.co/google/medasr",
  },
  {
    id: "moonshine-base", name: "Moonshine Base", short: "Base", tags: [], kind: "moonshine", size: 66.8e6,
    blurb: "General English · more accurate, ~3× slower than Tiny",
    files: {
      encoder: { url: MOONSHINE_BASE + "onnx/encoder_model_quantized.onnx", sha256: "1dd9ab0a7f987113d30affcba5a068d11c8f90fa0223caa3e491ade431ad9751", size: 20513063 },
      decoder: { url: MOONSHINE_BASE + "onnx/decoder_model_merged_quantized.onnx", sha256: "cc9f3cd6698a369c6008b41aa60aa3fb3322e7f03c9bdf19d8e6b7200afca4f3", size: 42498870 },
      vocab: { url: MOONSHINE_BASE + "tokenizer.json", size: 3761754 },
    },
    config: { decoder_num_hidden_layers: 8, decoder_num_key_value_heads: 8, headDim: 52 },
    license: "MIT · Useful Sensors", link: "https://huggingface.co/UsefulSensors/moonshine-base",
  },
];

// ── Preferences ──────────────────────────────────────────────────────────
const PREF_KEY = "jay.prefs";
const prefs = Object.assign(
  { model: "moonshine-tiny", theme: "system", mic: "", noise: true, pause: 0.6, commands: false, timestamps: true, preload: true, custom: [], rate: 1, corrections: [] },
  JSON.parse(localStorage.getItem(PREF_KEY) || "{}"),
);
const savePrefs = () => localStorage.setItem(PREF_KEY, JSON.stringify(prefs));

// ── Corrections: "heard → write" rules applied to every new transcript ──────
let corrCache = { key: "", rules: [] };
function correctionRules() {
  const key = JSON.stringify(prefs.corrections);
  if (key === corrCache.key) return corrCache.rules;
  const rules = prefs.corrections.filter((r) => r.from?.trim() && r.to != null).map((r) => ({
    re: new RegExp(`(?<![\\p{L}\\p{N}])${r.from.trim().replace(/[.*+?^${}()|[\]\\]/g, "\\$&").replace(/\s+/g, "\\s+")}(?![\\p{L}\\p{N}])`, "giu"),
    to: r.to,
  }));
  corrCache = { key, rules };
  return rules;
}
function applyCorrections(text) {
  for (const { re, to } of correctionRules()) {
    // Capitalise only where a sentence starts (or the heard word was Title-case), never for mid-sentence acronyms.
    text = text.replace(re, (m, off, all) => {
      const start = off === 0 || /[.?!:]\s*$|\n\s*$/.test(all.slice(Math.max(0, off - 3), off));
      const title = /^\p{Lu}\p{Ll}/u.test(m);
      return (start || title) && /^\p{Ll}/u.test(to) ? to[0].toUpperCase() + to.slice(1) : to;
    });
  }
  return text;
}
const correctSegs = (segs) => (prefs.corrections.length ? segs.map((x) => ({ ...x, text: applyCorrections(x.text) })) : segs);
const allModels = () => [...BUILTIN, ...prefs.custom];
const modelById = (id) => allModels().find((m) => m.id === id) || BUILTIN[0];
const current = () => modelById(prefs.model);
const specOf = (m) => ({ id: m.id, kind: m.kind, manifest: m.manifest, files: m.files, format: m.format, config: m.config, rev: m.rev });

// ── State ────────────────────────────────────────────────────────────────
const S = {
  sessions: new Map(), turns: new Map(), sid: null, query: "",
  model: {}, cache: {}, runtime: null,
  rec: null, queue: [], busy: false, playing: null, editing: null,
};
const turnsOf = (sid) => [...S.turns.values()].filter((t) => t.sessionId === sid).sort((a, b) => a.createdAt - b.createdAt);
const strip = (t) => Object.fromEntries(Object.entries(t).filter(([k]) => !k.startsWith("_")));
const saveTurn = (t) => (S.turns.get(t.id) === t ? db.put("turns", strip(t)) : Promise.resolve());
const saveSession = (s) => db.put("sessions", s);

// ── Worker ───────────────────────────────────────────────────────────────
let W;
const jobs = new Map();
const jobSeen = new Map(); // job → last time the engine said anything about it
function worker() {
  if (W) return W;
  W = new Worker(new URL("./worker.js", import.meta.url), { type: "module" });
  const debug = localStorage.getItem("jay.debug");
  if (debug) self.jayWorker = () => W;
  W.onmessage = ({ data: m }) => {
    if (m.type === "hello" && m.build && m.build !== BUILD) heal("worker " + m.build);
    if (debug) console.debug("[jay]", m.type, m.job || "", m.text ?? m.segments?.map((x) => x.text).join(" | ") ?? m.error ?? "");
    if (m.type === "model") onModel(m);
    else if (m.type === "runtime") { S.runtime = m.info; renderSettingsIfOpen(); }
    else if (m.type === "fatal") restartWorker(m.error);
    else if (m.type === "debug") console.debug("[jay] worker", JSON.stringify(m));
    else if (m.type === "cache-status") { Object.assign(S.cache, m.status); renderSettingsIfOpen(); renderEmptyIfShown(); }
    else if (m.job && jobs.has(m.job)) { jobSeen.set(m.job, Date.now()); jobs.get(m.job)(m); }
  };
  if (localStorage.getItem("jay.threads") || localStorage.getItem("jay.spin")) W.postMessage({ type: "config", threads: +localStorage.getItem("jay.threads") || 0, spin: !!localStorage.getItem("jay.spin") });
  W.onerror = (e) => { e.preventDefault?.(); restartWorker(e.message || "engine error"); };
  return W;
}
/** Tear down a crashed engine; in-flight jobs fail cleanly and recordings keep their audio. */
function restartWorker(reason) {
  console.warn("Jay engine restart:", reason);
  try { W?.terminate(); } catch {}
  W = null;
  for (const id of Object.keys(S.model)) if (S.model[id].state !== "error") delete S.model[id];
  const live = S.rec && !S.rec.stopping ? S.rec.job : null;
  for (const [job, fn] of [...jobs]) {
    if (job === live) { S.rec.crashed = true; continue; }
    if (S.recStopping?.job === job) { S.recStopping.crashed = true; fn({ type: "live-done", job }); continue; }
    fn({ type: "job-error", job, error: "The transcription engine restarted — try again" });
    jobs.delete(job);
  }
  toast("Transcription engine restarted");
  renderChip();
  if (live) { worker().postMessage({ type: "live-start", job: live, spec: specOf(modelById(S.rec.turn.model)), opts: S.rec.opts }); }
}
function onModel(m) {
  m.at = Date.now();
  S.model[m.id] = m;
  if (m.state === "ready") { S.cache[m.id] = { state: "cached" }; if (S.runtime == null && m.runtime) S.runtime = m.runtime; }
  if (m.state === "error" && m.id === prefs.model) toast(`${modelById(m.id).name}: ${m.error}`);
  renderChip(); renderSettingsIfOpen(); renderEmptyIfShown();
  for (const t of S.turns.values()) if (t.sessionId === S.sid && t.model === m.id && /transcribing|recording|queued/.test(t.status)) updateTurnStatus(t);
}
function ensureModel(m = current()) {
  const st = S.model[m.id]?.state;
  if (st === "ready" || st === "loading") return;
  if (!W) worker();
  S.model[m.id] = { id: m.id, state: "loading", stage: "start", at: Date.now() };
  worker().postMessage({ type: "load", spec: specOf(m) });
  renderChip();
}
// A load that goes silent (no progress for 3 min) becomes a retryable error, never an endless spinner.
// Likewise a transcription the engine stops talking about for 2 min restarts the engine; the clip
// fails with "Try again" (or, for a stopped dictation, re-transcribes from its saved audio).
setInterval(() => {
  const loadingAny = Object.values(S.model).some((m) => m.state === "loading");
  if (!W || loadingAny) { for (const j of jobs.keys()) jobSeen.set(j, Date.now()); return; }
  for (const j of jobs.keys()) {
    if (S.rec?.job === j && !S.rec.stopping) { jobSeen.set(j, Date.now()); continue; }
    if (!jobSeen.has(j)) jobSeen.set(j, Date.now());
    if (Date.now() - jobSeen.get(j) > (+localStorage.getItem("jay.stall") || 120000)) { jobSeen.clear(); restartWorker("transcription stalled"); return; }
  }
}, localStorage.getItem("jay.stall") ? 1000 : 10000);
setInterval(() => {
  for (const st of Object.values(S.model)) {
    if (st.state === "loading" && Date.now() - (st.at || 0) > 180000) {
      onModel({ id: st.id, state: "error", error: "Model load stalled" });
      restartWorker("model load stalled");
    }
  }
}, 10000);
function refreshCache() { worker().postMessage({ type: "cache-status", specs: allModels().map(specOf) }); }

// ── Toasts & menus ───────────────────────────────────────────────────────
function toast(msg, action, ms = 4200) {
  const el = document.createElement("div");
  el.className = "toast";
  el.innerHTML = `<span>${esc(msg)}</span>`;
  let done;
  if (action) {
    const b = document.createElement("button");
    b.textContent = action.label;
    b.onclick = () => { action.run(); kill(); };
    el.append(b);
  }
  $("#toasts").append(el);
  const kill = () => { if (done) return; done = 1; el.classList.add("out"); setTimeout(() => el.remove(), 220); action?.expire?.(); };
  setTimeout(kill, ms);
  return kill;
}
function menu(anchor, items, align = "end") {
  const el = $("#menu");
  el.innerHTML = "";
  for (const it of items) {
    if (it === "-") { el.append(document.createElement("hr")); continue; }
    if (it.head) { const h = document.createElement("div"); h.className = "hd"; h.textContent = it.head; el.append(h); continue; }
    const b = document.createElement("button");
    b.setAttribute("role", "menuitem");
    if (it.danger) b.className = "danger";
    if (it.on) b.classList.add("on");
    b.innerHTML = `${it.icon ? icon(it.icon).replace("<svg", `<svg class="${it.on ? "ck" : ""}"`) : ""}<span>${esc(it.label)}</span>${it.sub ? `<span class="sub">${esc(it.sub)}</span>` : ""}`;
    b.onclick = () => { closeMenu(); it.run(); };
    el.append(b);
  }
  el.hidden = false;
  const r = anchor.getBoundingClientRect(), mr = el.getBoundingClientRect();
  let x = align === "end" ? r.right - mr.width : r.left;
  let y = r.bottom + 6;
  if (y + mr.height > innerHeight - 8) y = r.top - mr.height - 6;
  el.style.left = Math.max(8, Math.min(x, innerWidth - mr.width - 8)) + "px";
  el.style.top = Math.max(8, y) + "px";
  el.querySelector("button")?.focus({ preventScroll: true });
}
const closeMenu = () => { $("#menu").hidden = true; };
addEventListener("pointerdown", (e) => { if (!$("#menu").hidden && !e.target.closest("#menu")) closeMenu(); }, true);

// ── Search ───────────────────────────────────────────────────────────────
const terms = () => S.query.toLowerCase().split(/\s+/).filter(Boolean);
const turnText = (t) => (t.segments || []).map((s) => s.text).join(" ");
function highlight(text, ts = terms()) {
  let h = esc(text);
  if (!ts.length) return h;
  const re = new RegExp(`(${ts.map((x) => esc(x).replace(/[.*+?^${}()|[\]\\]/g, "\\$&")).join("|")})`, "gi");
  return h.replace(re, "<mark>$1</mark>");
}
function snippet(text, ts) {
  const low = text.toLowerCase();
  const i = Math.max(0, ...ts.map((t) => low.indexOf(t)).filter((i) => i >= 0).slice(0, 1));
  const a = Math.max(0, i - 40);
  return (a ? "…" : "") + text.slice(a, a + 160);
}
function search() {
  const ts = terms();
  const hits = [];
  for (const s of S.sessions.values()) {
    const turns = turnsOf(s.id);
    const hay = (s.title + " " + turns.map((t) => t.name + " " + turnText(t)).join(" ")).toLowerCase();
    if (!ts.every((t) => hay.includes(t))) continue;
    let best = null, score = 0;
    for (const t of turns) {
      const txt = turnText(t);
      const low = (t.name + " " + txt).toLowerCase();
      const sc = ts.reduce((n, x) => n + (low.split(x).length - 1), 0);
      if (sc > score) { score = sc; best = t; }
    }
    hits.push({ s, t: best, score: score + (ts.every((t) => s.title.toLowerCase().includes(t)) ? 5 : 0) });
  }
  return hits.sort((a, b) => b.score - a.score || b.s.updatedAt - a.s.updatedAt);
}

// ── Sidebar ──────────────────────────────────────────────────────────────
function sessionStats(sid) {
  const ts = turnsOf(sid);
  return { n: ts.length, dur: ts.reduce((s, t) => s + (t.duration || 0), 0), last: ts.at(-1) };
}
function groupOf(t) {
  const d = new Date(t), now = new Date();
  const day = 864e5, start = new Date(now.getFullYear(), now.getMonth(), now.getDate()).getTime();
  if (t >= start) return "Today";
  if (t >= start - day) return "Yesterday";
  if (t >= start - 6 * day) return "This week";
  return d.toLocaleDateString([], { month: "long", year: d.getFullYear() === now.getFullYear() ? undefined : "numeric" });
}
function renderSidebar() {
  const nav = $("#sessions");
  const ts = terms();
  let html = "";
  if (ts.length) {
    const hits = search();
    html += `<div class="group">${hits.length} match${hits.length === 1 ? "" : "es"}</div>`;
    for (const { s, t } of hits) {
      const st = sessionStats(s.id);
      html += `<a class="sess${s.id === S.sid ? " on" : ""}" href="#/s/${s.id}${t ? "/" + t.id : ""}">
        <span class="t">${highlight(s.title, ts)}</span>
        <span class="m"><span>${fmtWhen(s.updatedAt)}</span><span>${st.n} clip${st.n === 1 ? "" : "s"}</span></span>
        ${t ? `<span class="s">${highlight(snippet(turnText(t) || t.name, ts), ts)}</span>` : ""}</a>`;
    }
    if (!hits.length) html += `<div class="ledger-empty">Nothing matches “${esc(S.query)}”.</div>`;
  } else {
    const list = [...S.sessions.values()].sort((a, b) => b.updatedAt - a.updatedAt);
    let g = "";
    for (const s of list) {
      const grp = groupOf(s.updatedAt);
      if (grp !== g) { html += `<div class="group">${grp}</div>`; g = grp; }
      const st = sessionStats(s.id);
      const live = S.rec && S.rec.turn.sessionId === s.id;
      const preview = st.last ? turnText(st.last) : "";
      html += `<a class="sess${s.id === S.sid ? " on" : ""}" href="#/s/${s.id}">
        <span class="t">${esc(s.title)}</span>
        <span class="m">${live ? `<span class="live">● recording</span>` : `<span>${fmtTime(s.updatedAt)}</span>`}<span>${st.n} clip${st.n === 1 ? "" : "s"}</span><span>${fmtDur(st.dur)}</span></span>
        ${preview ? `<span class="s">${esc(preview.slice(0, 180))}</span>` : ""}</a>`;
    }
    if (!list.length) html = `<div class="ledger-empty">Sessions you record or upload will gather here.</div>`;
  }
  nav.innerHTML = html;
}

// ── Page ─────────────────────────────────────────────────────────────────
const SONG = `<svg class="song" viewBox="0 0 280 56" aria-hidden="true"><path d="M8 40c6-14 10-14 14 0M30 30c3-18 6-18 9 0 3 18 6 18 9 0M58 36c4-24 9-24 13 0M84 22l6 18 6-26 6 22M120 40c5-6 9-26 14-20s7 18 12 4M160 30c2-10 4-10 6 0s4 10 6 0 4-10 6 0M196 38c8-30 12-30 18 0M222 26c3 0 4 12 8 12s5-20 9-20 5 16 9 16 4-8 8-8M262 34h10" /></svg>`;
function renderPage() {
  const s = S.sessions.get(S.sid);
  const page = $("#page");
  page.classList.toggle("draft", !s);
  $("#title").textContent = s ? s.title : "New session";
  const turns = s ? turnsOf(s.id) : [];
  const dur = turns.reduce((a, t) => a + (t.duration || 0), 0);
  $("#meta").innerHTML = s ? `<span>${fmtDay(s.createdAt)} · ${fmtTime(s.createdAt)}</span><span>${turns.length} clip${turns.length === 1 ? "" : "s"}</span><span>${fmtDur(dur)}</span>` : `<span>${fmtDay(Date.now())}</span>`;
  const box = $("#turns");
  box.classList.toggle("notime", !prefs.timestamps);
  if (!turns.length) { box.innerHTML = emptyHtml(); return; }
  box.innerHTML = turns.map(turnHtml).join("");
  requestAnimationFrame(() => turns.forEach(drawSono));
  turns.forEach(ensureSono);
}
const sonoPending = new Set();
/** Backups omit sonograms; rebuild them lazily from stored audio. */
async function ensureSono(t) {
  if (t.sono || !t.hasAudio || t.status !== "done" || sonoPending.has(t.id) || jobs.has(t.id)) return;
  sonoPending.add(t.id);
  try {
    const blob = await db.get("audio", t.id);
    if (!blob) return;
    const pcm = await decodeFile(blob);
    const job = "sono-" + t.id;
    jobs.set(job, (m) => {
      if (m.type !== "sonogram" && m.type !== "job-error") return;
      jobs.delete(job); sonoPending.delete(t.id);
      if (m.sono) { t.sono = m.sono; saveTurn(t); updateTurnStatus(t); }
    });
    worker().postMessage({ type: "sonogram", job, pcm: pcm.buffer }, [pcm.buffer]);
  } catch { sonoPending.delete(t.id); }
}
function emptyHtml() {
  const m = current();
  const med = BUILTIN[1];
  const showMed = prefs.model !== "medasr" && S.cache.medasr?.state !== "cached";
  return `<div class="empty" id="empty"><div class="inner">
    <svg class="mark" aria-hidden="true"><use href="#jay"/></svg>
    <h2>Ready when you are</h2>
    ${SONG}
    <div class="acts">
      <button class="btn primary" data-act="record">${icon("mic")}Dictate<kbd>R</kbd></button>
      <button class="btn" data-act="upload">${icon("upload")}Upload audio<kbd>U</kbd></button>
    </div>
    ${showMed ? `<div><button class="suggest" data-act="use-medasr">${icon("feather")}<span>Clinical vocabulary? <b>Use MedASR</b></span><span class="sz">${fmtBytes(med.size)} once</span></button></div>` : ""}
    <div class="loadline" id="loadline">${loadLine(m)}</div>
  </div></div>`;
}
const loadText = (m) => loadLine(m).replace(/<[^>]+>/g, "").replace(/ · Retry$/, "");
function loadLine(m) {
  const st = S.model[m.id];
  if (!st) return "";
  if (st.state === "loading") {
    if (st.stage === "download" && st.total) return `Fetching ${m.name} · ${Math.round((st.got / st.total) * 100)}% of ${fmtBytes(st.total)}`;
    if (st.stage === "compile") return `Warming up ${m.name}…`;
    return `Loading ${m.name}…`;
  }
  if (st.state === "ready") return `${m.name} ready · runs on this device${S.runtime ? ` · ${S.runtime.threads} thread${S.runtime.threads > 1 ? "s" : ""}` : ""}`;
  if (st.state === "error") return `Couldn’t load ${m.name}: ${esc(st.error)} · <button class="link" data-act="retry-model">Retry</button>`;
  return "";
}
function renderEmptyIfShown() {
  const l = $("#loadline");
  if (l) l.innerHTML = loadLine(current());
  const sug = $("#empty .suggest");
  if (sug && (prefs.model === "medasr" || S.cache.medasr?.state === "cached")) sug.remove();
}

function paragraphs(t, list = t.segments || []) {
  const out = [];
  let cur = null, prevEnd = 0, prevText = "";
  list.forEach((s, i) => {
    const gap = s.start - prevEnd;
    const long = cur && s.start - cur.start > 40 && /[.?!]$/.test(prevText);
    if (!cur || (s.brk ?? 0) >= 1 || gap > 1.0 || long) { cur = { start: s.start, segs: [] }; out.push(cur); }
    cur.segs.push([s, i]);
    prevEnd = s.end; prevText = s.text;
  });
  return out;
}
const secLabel = (h) => h.replace(/^([^:<]{2,40}:)/, '<b class="sec">$1</b>');
function textHtml(t) {
  const ts = terms();
  const live = Array.isArray(t._partial) ? t._partial.filter((x) => x.text) : [];
  const final = t.segments || [];
  const paras = paragraphs(t, live.length ? final.concat(live.map((x) => ({ ...x, _p: 1 }))) : final);
  const editing = S.editing === t.id;
  const lastP = final.length + live.length - 1;
  let html = paras.map((p) => `<p><button class="ts" data-seek="${p.start}" aria-label="Play from ${fmtDur(p.start)}" tabindex="${editing ? -1 : 0}">${fmtDur(p.start)}</button>${p.segs
    .map(([s, i]) => s._p
      ? `<span class="partial${i === lastP ? " tail" : ""}">${s.sec ? secLabel(esc(s.text)) : esc(s.text)}</span>`
      : `<span class="seg" data-i="${i}" data-t="${s.start}"${editing ? ' contenteditable="true" spellcheck="true"' : ""}>${editing ? esc(s.text) : s.sec ? secLabel(highlight(s.text, ts)) : highlight(s.text, ts)}</span>`)
    .join(" ")}</p>`).join("");
  if (!html) {
    if (t.status === "recording") html = `<p class="muted"><span class="caret">Listening</span></p>`;
    else if (t.status === "done") html = `<p class="muted">No speech detected.</p>`;
    else if (t.status === "error") html = "";
    else html = `<p class="muted">Transcribing…</p>`;
  }
  return html;
}
function statusHtml(t) {
  if (t.status === "recording") return "";
  if (t.status === "queued") return `<div class="progress indeterminate"><i></i></div>`;
  if (t.status === "transcribing") {
    const st = S.model[t.model];
    if (st?.state === "loading" && st.total) return `<div class="progress"><i style="width:${(st.got / st.total) * 100}%"></i></div>`;
    if (t._progress) return `<div class="progress"><i style="width:${Math.min(100, t._progress * 100)}%"></i></div>`;
    return `<div class="progress indeterminate"><i></i></div>`;
  }
  return "";
}
function footHtml(t) {
  if (t.status === "error") return `<div class="foot"><span>${esc(t.error || "Transcription failed")}</span>${t.hasAudio ? `<button data-act="retry">Try again</button>${t.model !== "moonshine-tiny" ? `<button data-act="retry-tiny">Use Moonshine Tiny</button>` : ""}` : ""}</div>`;
  if (/transcribing|recording|queued/.test(t.status) && S.model[t.model]?.state === "loading") return `<div class="foot"><span>${esc(loadText(modelById(t.model)))}</span></div>`;
  if (t.prev && t.status === "done") return `<div class="foot"><span>${t.prev.live ? "Transcribed from the full recording" : `Re-transcribed with ${esc(modelById(t.model).name)}`}</span><button data-act="restore">${t.prev.live ? "Show live draft" : "Restore previous"}</button></div>`;
  return "";
}
function chipHtml(t) {
  if (t.status === "recording") return `<span class="chip live">${S.rec?.paused ? "paused" : "live"}</span>`;
  if (t.status === "queued") return `<span class="chip">queued</span>`;
  if (t.status === "transcribing") return `<span class="chip">transcribing${t._progress ? " " + Math.round(t._progress * 100) + "%" : ""}</span>`;
  if (t.status === "error") return `<span class="chip err">failed</span>`;
  const m = modelById(t.model);
  const speed = t.rtf ? ` · ${t.rtf >= 10 ? Math.round(t.rtf) : t.rtf.toFixed(1)}× realtime` : "";
  return `<span class="chip" title="${esc(m.name + speed)}">${esc(m.name)}${t.edited ? " · edited" : ""}</span>`;
}
function turnHtml(t) {
  const playing = S.playing?.id === t.id;
  return `<article class="turn${t.status === "recording" ? " live" : ""}${playing && S.playing.on ? " playing" : ""}${S.editing === t.id ? " editing" : ""}" id="t-${t.id}" data-id="${t.id}">
    <button class="play" data-act="play" aria-label="Play" ${t.hasAudio ? "" : "disabled"}>${icon(playing && S.playing.on ? "pause" : "play")}</button>
    <div class="turn-head">
      <span class="kind">${icon(t.kind === "upload" ? "file" : "mic")}<span>${esc(t.name)}</span></span>
      <span class="when">${fmtTime(t.createdAt)}</span>
      <span class="chipbox">${chipHtml(t)}</span>
      <span class="turn-tools">
        ${/transcribing|queued/.test(t.status) && t.kind === "upload" ? `<button class="icon-btn stop" data-act="stop-turn" title="Stop transcribing" aria-label="Stop transcribing">${icon("stop")}</button>` : ""}
        ${S.editing === t.id
          ? `<button class="icon-btn" data-act="edit-done" title="Done editing" aria-label="Done editing">${icon("check")}</button>`
          : `<button class="icon-btn" data-act="copy" title="Copy" aria-label="Copy">${icon("copy")}</button>
             <button class="icon-btn" data-act="edit" title="Edit" aria-label="Edit">${icon("edit")}</button>`}
        <button class="icon-btn" data-act="turn-more" title="More" aria-label="More">${icon("more")}</button>
      </span>
    </div>
    ${t.sono || t.hasAudio ? `<div class="scrub" data-act="scrub"><canvas></canvas><div class="played"></div><div class="cursor"></div><span class="dur">${fmtDur(t.duration)}</span></div>` : ""}
    <div class="statusbox">${statusHtml(t)}</div>
    <div class="text">${textHtml(t)}</div>
    <div class="footbox">${footHtml(t)}</div>
  </article>`;
}
function updateTurn(t, { text = true } = {}) {
  const el = $(`#t-${t.id}`);
  if (!el) return;
  if (text && S.editing !== t.id) $(".text", el).innerHTML = textHtml(t);
  updateTurnStatus(t);
}
function updateTurnStatus(t) {
  const el = $(`#t-${t.id}`);
  if (!el) return;
  const busy = /transcribing|queued/.test(t.status) && t.kind === "upload";
  if (!!$(".stop", el) !== busy && S.editing !== t.id) { el.outerHTML = turnHtml(t); drawSono(t); return; }
  $(".chipbox", el).innerHTML = chipHtml(t);
  $(".statusbox", el).innerHTML = statusHtml(t);
  $(".footbox", el).innerHTML = footHtml(t);
  el.classList.toggle("live", t.status === "recording");
  const play = $(".play", el);
  play.disabled = !t.hasAudio;
  if (!$(".scrub", el) && (t.sono || t.hasAudio)) {
    $(".turn-head", el).insertAdjacentHTML("afterend", `<div class="scrub" data-act="scrub"><canvas></canvas><div class="played"></div><div class="cursor"></div><span class="dur">${fmtDur(t.duration)}</span></div>`);
  }
  const d = $(".scrub .dur", el);
  if (d) d.textContent = fmtDur(t.duration);
  if (t.sono) drawSono(t);
}
function appendTurn(t) {
  const box = $("#turns");
  if ($("#empty")) box.innerHTML = "";
  box.insertAdjacentHTML("beforeend", turnHtml(t));
  requestAnimationFrame(() => { drawSono(t); $(`#t-${t.id}`)?.scrollIntoView({ block: "end", behavior: "smooth" }); });
}
function renderHeaderMeta() {
  const s = S.sessions.get(S.sid);
  if (!s) return;
  const turns = turnsOf(s.id);
  const dur = turns.reduce((a, t) => a + (t.duration || 0), 0);
  $("#title").textContent = s.title;
  $("#meta").innerHTML = `<span>${fmtDay(s.createdAt)} · ${fmtTime(s.createdAt)}</span><span>${turns.length} clip${turns.length === 1 ? "" : "s"}</span><span>${fmtDur(dur)}</span>`;
}

// ── Sonogram drawing ─────────────────────────────────────────────────────
function inkRGB() {
  const c = getComputedStyle(document.documentElement).getPropertyValue("--sono").trim();
  const m = /^#?([0-9a-f]{6})$/i.exec(c);
  const n = m ? parseInt(m[1], 16) : 0x1d3f86;
  return [(n >> 16) & 255, (n >> 8) & 255, n & 255];
}
function drawSono(t) {
  const el = $(`#t-${t.id} .scrub canvas`);
  if (!el || !t.sono) return;
  const { w, h, data } = t.sono;
  const src = document.createElement("canvas");
  src.width = w; src.height = h;
  const off = src.getContext("2d");
  const img = off.createImageData(w, h);
  const [r, g, b] = inkRGB();
  for (let i = 0; i < w * h; i++) { img.data[i * 4] = r; img.data[i * 4 + 1] = g; img.data[i * 4 + 2] = b; img.data[i * 4 + 3] = data[i] * 0.82; }
  off.putImageData(img, 0, 0);
  const box = el.getBoundingClientRect();
  if (!box.width) return;
  const dpr = devicePixelRatio || 1;
  el.width = Math.max(1, Math.round(box.width * dpr)); el.height = Math.max(1, Math.round(box.height * dpr));
  const ctx = el.getContext("2d");
  ctx.imageSmoothingEnabled = true;
  ctx.drawImage(src, 0, 0, el.width, el.height);
}
new ResizeObserver(() => { clearTimeout(drawSono.t); drawSono.t = setTimeout(() => turnsOf(S.sid).forEach(drawSono), 120); }).observe($("#turns"));

// ── Sessions ─────────────────────────────────────────────────────────────
function newTitle() { return `${fmtDay(Date.now())}, ${fmtTime(Date.now())}`; }
async function ensureSession() {
  let s = S.sessions.get(S.sid);
  if (s) return s;
  s = { id: uid(), title: newTitle(), createdAt: Date.now(), updatedAt: Date.now(), autoTitle: true };
  S.sessions.set(s.id, s);
  await saveSession(s);
  S.sid = s.id;
  history.replaceState(null, "", `#/s/${s.id}`);
  $("#page").classList.remove("draft");
  renderHeaderMeta(); renderSidebar();
  return s;
}
async function touch(sid) {
  const s = S.sessions.get(sid);
  if (!s) return;
  s.updatedAt = Date.now();
  if (s.autoTitle) {
    const first = turnsOf(sid).map(turnText).find((x) => x.trim());
    if (first) {
      // Report headings ("Exam Type: …") make poor titles; use what follows them.
      const sentence = first.replace(/[\n\r]+/g, " ").split(/(?<=[.?!])\s/)[0].replace(/^[A-Z][\w ]{1,30}:\s+/, "");
      const words = sentence.split(/\s+/).slice(0, 8).join(" ").replace(/[,:;.]+$/, "");
      if (words.length > 3) s.title = words.length > 52 ? words.slice(0, 50) + "…" : words;
    }
  }
  await saveSession(s);
  if (sid === S.sid) renderHeaderMeta();
  renderSidebar();
}
function openSession(sid, focusTurn) {
  if (S.editing) finishEdit();
  S.sid = sid && S.sessions.has(sid) ? sid : null;
  renderPage(); renderSidebar();
  $("#app").classList.remove("drawer");
  if (focusTurn) requestAnimationFrame(() => {
    const el = $(`#t-${focusTurn}`);
    if (!el) return;
    el.scrollIntoView({ block: "center" });
    const m = $(".seg mark", el)?.closest(".seg");
    m?.classList.add("flash");
  });
  else $("#turns").scrollTop = $("#turns").scrollHeight;
}
function newSession() {
  if (S.rec) return toast("Stop recording first");
  history.pushState(null, "", "#/");
  openSession(null);
}
function route() {
  const m = /^#\/s\/([^/]+)(?:\/([^/]+))?/.exec(location.hash);
  openSession(m?.[1], m?.[2]);
}
addEventListener("hashchange", route);

// ── Recording ────────────────────────────────────────────────────────────
let wakeLock = null;
/** Keep the screen on while dictating so phones don't sleep mid-sentence. */
async function wake(on) {
  try {
    if (!on) { await wakeLock?.release(); wakeLock = null; return; }
    if ("wakeLock" in navigator && !wakeLock && document.visibilityState === "visible") {
      wakeLock = await navigator.wakeLock.request("screen");
      wakeLock.addEventListener("release", () => { wakeLock = null; });
    }
  } catch {}
}
document.addEventListener("visibilitychange", () => { if (S.rec && document.visibilityState === "visible") wake(true); });
async function startRec() {
  if (S.rec || S.starting) return;
  if (!navigator.mediaDevices?.getUserMedia) return toast("Microphone access isn’t available in this browser");
  S.starting = true;
  let rec;
  try {
    rec = window.jayFakeMic && localStorage.getItem("jay.debug") ? new FakeRecorder() : new Recorder();
    const job = uid();
    const ctx = { job, chunks: [], pending: [], samples: 0, seq: 0, errors: [], paused: false, t0: performance.now() };
    await rec.start({
      deviceId: prefs.mic, noise: prefs.noise,
      onPcm: (f32) => {
        if (S.rec !== ctx) return;
        const i16 = f32ToI16(f32);
        ctx.chunks.push(i16); ctx.pending.push(i16); ctx.samples += f32.length;
        worker().postMessage({ type: "live-audio", job, pcm: f32.buffer }, [f32.buffer]);
      },
    });
    const s = await ensureSession();
    const m = current();
    ensureModel(m);
    const t = { id: job, sessionId: s.id, createdAt: Date.now(), kind: "dictation", name: "Dictation", model: m.id, status: "recording", segments: [], duration: 0, hasAudio: false };
    S.turns.set(t.id, t);
    await saveTurn(t);
    ctx.rec = rec; ctx.turn = t;
    S.rec = ctx;
    jobs.set(job, (msg) => onLive(ctx, msg));
    ctx.opts = { pause: prefs.pause, commands: prefs.commands, maxSeg: m.kind === "moonshine" ? 15 : 24 };
    worker().postMessage({ type: "live-start", job, spec: specOf(m), opts: ctx.opts });
    ctx.flushTimer = setInterval(() => flushPcm(ctx), 4000);
    wake(true);
    appendTurn(t);
    setRecUI(true);
    navigator.vibrate?.(15);
    renderSidebar(); renderHeaderMeta();
    liveSono();
  } catch (e) {
    console.error(e);
    rec?.stop();
    const ctx = S.rec;
    if (ctx?.rec === rec) {
      clearInterval(ctx.flushTimer); wake(false);
      jobs.delete(ctx.job); W?.postMessage({ type: "live-stop", job: ctx.job });
      S.rec = null;
      if (!ctx.turn.segments.length) removeTurn(ctx.turn, false); else { ctx.turn.status = "done"; saveTurn(ctx.turn); }
    }
    setRecUI(false); cancelAnimationFrame(sonoRaf);
    const denied = e?.name === "NotAllowedError" || e?.name === "SecurityError";
    toast(denied ? "Microphone permission was denied" : e?.name === "NotFoundError" ? "No microphone found" : `Couldn’t start recording: ${e?.message || e}`);
  } finally { S.starting = false; }
}
async function flushPcm(ctx) {
  if (!ctx.pending.length) return;
  const data = concatI16(ctx.pending.splice(0));
  await db.put("pcm", { turnId: ctx.turn.id, seq: ctx.seq++, data });
  ctx.turn.duration = ctx.samples / SR;
  if (ctx.turn.sessionId === S.sid) renderHeaderMeta();
}
function onLive(ctx, m) {
  const t = ctx.turn;
  if (m.type === "partial") { t._partial = correctSegs(m.segments || []); updateTurn(t); keepBottom(); }
  else if (m.type === "segments") {
    t.segments.push(...correctSegs(m.segments));
    if (m.ms) { ctx.ms = (ctx.ms || 0) + m.ms; ctx.audio = (ctx.audio || 0) + m.audioSec; }
    t._partial = m.partial ? correctSegs(m.partial) : null; updateTurn(t); saveTurn(t); keepBottom();
  } else if (m.type === "job-error") ctx.errors.push(m.error);
  else if (m.type === "sonogram") { t.sono = m.sono; updateTurnStatus(t); saveTurn(t); }
  else if (m.type === "live-done") finishLive(ctx);
}
function keepBottom() {
  const b = $("#turns");
  if (b.scrollHeight - b.scrollTop - b.clientHeight < 160) b.scrollTop = b.scrollHeight;
}
async function stopRec() {
  const ctx = S.rec;
  if (!ctx || ctx.stopping) return;
  ctx.stopping = true;
  clearInterval(ctx.flushTimer);
  wake(false);
  navigator.vibrate?.([10, 60, 10]);
  if (ctx.paused) ctx.rec.resume();
  ctx.paused = false;
  await ctx.rec.stop(); // flushes the worklet's tail into onPcm before closing
  S.rec = null;
  S.recStopping = ctx;
  const st = ctx.rec.stats;
  if (localStorage.getItem("jay.debug")) console.debug("[jay] capture", JSON.stringify({ ...st, samples16k: ctx.samples, wall: (performance.now() - ctx.t0) / 1000 }));
  setRecUI(false);
  const t = ctx.turn;
  let pcm = concatI16(ctx.chunks);
  t.duration = pcm.length / SR;
  if (t.duration < 0.4 && !t.segments.length) {
    worker().postMessage({ type: "live-stop", job: t.id });
    jobs.delete(t.id);
    await removeTurn(t, false);
    return;
  }
  t.status = "transcribing"; t._partial = null;
  updateTurn(t);
  // If the live graph lost audio (starved device), swap in the MediaRecorder copy and re-transcribe it.
  let sonoPcm = pcm, isI16 = true;
  const bk = ctx.rec.backupBlob;
  if (bk && bk.size && t.duration < 90 * 60) {
    try {
      const full = await decodeFile(bk);
      if (localStorage.getItem("jay.debug")) console.debug("[jay] backup", bk.type, bk.size, "full", (full.length / SR).toFixed(2), "live", (pcm.length / SR).toFixed(2));
      if (full.length - pcm.length > Math.max(SR * 0.3, pcm.length * 0.015)) {
        ctx.recovered = true;
        t.duration = full.length / SR;
        pcm = f32ToI16(full); sonoPcm = full; isI16 = false;
      }
    } catch (e) { if (localStorage.getItem("jay.debug")) console.debug("[jay] backup failed", e?.message || e); }
  }
  try { await db.put("audio", wavBlob(pcm), t.id); t.hasAudio = true; }
  catch (e) { t.hasAudio = false; toast(e?.name === "QuotaExceededError" ? "Storage is full — the transcript is kept but this audio couldn’t be saved" : "Couldn’t save this audio"); }
  if (t.hasAudio) await db.delByIndex("pcm", "turn", t.id);
  updateTurn(t); await saveTurn(t);
  worker().postMessage({ type: "sonogram", job: t.id, pcm: sonoPcm.buffer, i16: isI16 }, [sonoPcm.buffer]);
  worker().postMessage({ type: "live-stop", job: t.id });
  touch(t.sessionId);
  persistQuietly();
}
async function finishLive(ctx) {
  const t = ctx.turn;
  if (S.recStopping === ctx) S.recStopping = null;
  jobs.delete(t.id); jobSeen.delete(t.id);
  if ((ctx.crashed || ctx.recovered) && t.hasAudio) {
    if (t.segments.length) t.prev = { model: t.model, segments: t.segments, edited: false, live: true };
    t.segments = []; t.status = "queued"; S.queue.push(t.id);
    if (ctx.recovered) toast("The live stream missed some audio — re-transcribing the full recording");
  }
  else if (!t.segments.length && ctx.errors.length) { t.status = "error"; t.error = ctx.errors[0]; }
  else t.status = "done";
  if (ctx.ms) t.rtf = ctx.audio / (ctx.ms / 1000);
  t._partial = null;
  await saveTurn(t);
  updateTurn(t);
  touch(t.sessionId);
  pumpQueue();
}
function togglePause() {
  const ctx = S.rec;
  if (!ctx) return;
  ctx.paused = !ctx.paused;
  if (ctx.paused) { ctx.rec.pause(); worker().postMessage({ type: "live-pause", job: ctx.job }); }
  else ctx.rec.resume();
  $("#dock").classList.toggle("paused", ctx.paused);
  $("#pause").innerHTML = icon(ctx.paused ? "mic" : "pause");
  $("#pause").title = ctx.paused ? "Resume (P)" : "Pause (P)";
  updateTurnStatus(ctx.turn);
}
function setRecUI(on) {
  $("#dock").classList.toggle("recording", on);
  $("#dock").classList.remove("paused");
  $("#pause").innerHTML = icon("pause");
  $("#rec").setAttribute("aria-label", on ? "Stop (R)" : "Record (R)");
  $("#rec").title = on ? "Stop (R)" : "Record (R)";
  if (!on) { $("#rec").style.setProperty("--lvl", 0); $("#clock").textContent = "0:00"; const w = $("#strip-warn"); if (w) w.hidden = true; idleStrip(); }
}

let sonoRaf;
function liveSono() {
  const cv = $("#live-sono"), ctx2 = cv.getContext("2d");
  const fit = () => { const r = cv.getBoundingClientRect(); cv.width = Math.round(r.width * devicePixelRatio); cv.height = Math.round(r.height * devicePixelRatio); };
  fit();
  const [r, g, b] = inkRGB();
  let fl, time, last = 0, env = null, quiet = null;
  const step = () => {
    const ctx = S.rec;
    if (!ctx) return;
    sonoRaf = requestAnimationFrame(step);
    try { draw(ctx); } catch (e) { if (!step.warned) { step.warned = true; console.warn("live view", e); } }
  };
  const draw = (ctx) => {
    $("#clock").textContent = fmtDur(ctx.samples / SR);
    const an = ctx.rec.analyser;
    if (!an) return;
    fl ||= new Float32Array(an.frequencyBinCount);
    time ||= new Float32Array(an.fftSize);
    const now = performance.now();
    if (now - last < 28) return;
    last = now;
    an.getFloatTimeDomainData(time);
    let s = 0; for (const v of time) s += v * v;
    const db = 10 * Math.log10(s / time.length + 1e-12);
    const lvl = ctx.paused ? 0 : Math.min(1, Math.max(0, (db + 55) / 40));
    $("#rec").style.setProperty("--lvl", lvl.toFixed(2));
    // A silent input for the first few seconds usually means a muted or wrong microphone.
    if (!ctx.paused) { ctx.peakDb = Math.max(ctx.peakDb ?? -200, db); ctx.liveMs = (ctx.liveMs || 0) + 28; }
    const mute = !ctx.paused && ctx.liveMs > 3500 && ctx.peakDb < -72;
    if (mute !== ctx.muteShown) { ctx.muteShown = mute; const w = $("#strip-warn"); if (w) w.hidden = !mute; }
    if (cv.width !== Math.round(cv.getBoundingClientRect().width * devicePixelRatio)) fit();
    const W = cv.width, H = cv.height, dx = Math.max(2, Math.round(2 * devicePixelRatio));
    ctx2.globalCompositeOperation = "copy";   // shift without re-blending old ink
    ctx2.drawImage(cv, -dx, 0);
    ctx2.globalCompositeOperation = "source-over";
    ctx2.clearRect(W - dx, 0, dx, H);
    if (ctx.paused) return;
    an.getFloatFrequencyData(fl);
    const nyq = ctx.rec.ctx.sampleRate / 2, bins = fl.length;
    const rows = 32, rh = H / rows, col = new Float32Array(rows);
    for (let i = 0; i < rows; i++) {
      const f0 = 110 * Math.pow(6500 / 110, i / rows), f1 = 110 * Math.pow(6500 / 110, (i + 1) / rows);
      const a = Math.floor((f0 / nyq) * bins), c = Math.max(a + 1, Math.ceil((f1 / nyq) * bins));
      let m = -160; for (let k = a; k < c; k++) if (fl[k] > m) m = fl[k];
      col[i] = m;
    }
    // Field-guide look: show only the strongest ~24 dB of each column, gated by a slow
    // loudness envelope so room tone stays blank and speech shows its formant bands.
    let peak = -200; for (const v of col) peak = Math.max(peak, v);
    env = env == null ? peak : peak > env ? env * 0.6 + peak * 0.4 : env * 0.997 + peak * 0.003;
    quiet = quiet == null ? peak : peak < quiet ? quiet * 0.7 + peak * 0.3 : quiet * 0.999 + peak * 0.001;
    const gate = Math.max(0, Math.min(1, (peak - quiet - 8) / 14));
    if (gate > 0) for (let i = 0; i < rows; i++) {
      const v = Math.max(0, 1 - (peak - col[i]) / 24) * gate;
      if (v <= 0.04) continue;
      ctx2.fillStyle = `rgba(${r},${g},${b},${(Math.pow(v, 1.6) * 0.9).toFixed(3)})`;
      ctx2.fillRect(W - dx, H - (i + 1) * rh, dx, rh + 0.5);
    }
  };
  cancelAnimationFrame(sonoRaf);
  ctx2.clearRect(0, 0, cv.width, cv.height);
  step();
}
function idleStrip() {
  const cv = $("#live-sono");
  cv.getContext("2d").clearRect(0, 0, cv.width, cv.height);
  const st = S.model[prefs.model];
  const busy = [...S.turns.values()].some((t) => t.status === "transcribing" || t.status === "queued");
  let msg = `Drop audio anywhere`;
  if (st?.state === "loading") msg = esc(loadText(current()));
  else if (busy) msg = `Transcribing…`;
  else if (st?.state === "error") msg = `Model unavailable — choose another`;
  $("#strip-status").innerHTML = msg;
  $("#strip-status").classList.toggle("hint", msg === "Drop audio anywhere");
}

// ── Uploads ──────────────────────────────────────────────────────────────
const AUDIO_RE = /\.(m4a|mp3|wav|ogg|oga|opus|webm|flac|aac|mp4|mov|caf|aiff?|amr|3gp|wma)$/i;
async function addFiles(files, trusted = false) {
  // Shared voice notes often arrive as application/octet-stream; let the decoder decide for those.
  files = [...files].filter((f) => trusted || /^(audio|video)\//.test(f.type) || AUDIO_RE.test(f.name));
  if (!files.length) return toast("Those files don’t look like audio");
  if (S.rec) return toast("Stop recording first");
  const s = await ensureSession();
  for (const f of files) {
    const t = { id: uid(), sessionId: s.id, createdAt: Date.now(), kind: "upload", name: f.name, mime: f.type, size: f.size, model: prefs.model, status: "queued", segments: [], duration: 0, hasAudio: true };
    S.turns.set(t.id, t);
    try { await db.put("audio", f, t.id); }
    catch (e) { S.turns.delete(t.id); toast(e?.name === "QuotaExceededError" ? `Storage is full — couldn’t add ${f.name}` : `Couldn’t store ${f.name}`); continue; }
    await saveTurn(t);
    S.queue.push(t.id);
    if (S.sid === s.id) appendTurn(t);
  }
  touch(s.id);
  persistQuietly();
  pumpQueue();
}
async function pumpQueue() {
  if (S.busy || S.rec) return;
  const id = S.queue.shift();
  if (!id) { idleStrip(); return; }
  const t = S.turns.get(id);
  if (!t) return pumpQueue();
  S.busy = true;
  try { await transcribeTurn(t); }
  catch (e) { t.status = "error"; t.error = String(e?.message || e); await saveTurn(t); updateTurn(t); }
  S.busy = false;
  touch(t.sessionId);
  pumpQueue();
}
async function transcribeTurn(t) {
  t.status = "transcribing"; t._progress = 0; t.segments = []; t.error = null;
  updateTurn(t); idleStrip();
  const blob = await db.get("audio", t.id);
  if (!blob) throw new Error("Audio is no longer stored for this clip");
  let pcm;
  try { pcm = await decodeFile(blob); }
  catch { throw new Error("Couldn’t decode this file’s audio"); }
  t.duration = pcm.length / SR;
  const m = modelById(t.model);
  t._busy = true;
  await new Promise((resolve, reject) => {
    jobs.set(t.id, (msg) => {
      if (msg.type === "sonogram") { t.sono = msg.sono; updateTurnStatus(t); }
      else if (msg.type === "progress") { t._progress = msg.total ? msg.done / msg.total : 0; updateTurnStatus(t); }
      else if (msg.type === "segments") { t.segments.push(...correctSegs(msg.segments)); updateTurn(t); }
      else if (msg.type === "file-done") { if (msg.ms) t.rtf = msg.audioSec / (msg.ms / 1000); resolve(); }
      else if (msg.type === "job-error") reject(new Error(msg.error));
    });
    worker().postMessage({ type: "file", job: t.id, spec: specOf(m), pcm: pcm.buffer, opts: { commands: prefs.commands } }, [pcm.buffer]);
  }).finally(() => { jobs.delete(t.id); jobSeen.delete(t.id); t._busy = false; });
  if (stopping.delete(t.id)) {
    t.status = "error";
    t.error = `Stopped at ${fmtDur(t.segments.at(-1)?.end || 0)} of ${fmtDur(t.duration)}`;
  } else t.status = "done";
  t._progress = 0;
  await saveTurn(t);
  updateTurn(t);
}
const stopping = new Set();
async function retranscribe(t, modelId = prefs.model) {
  if (!t.hasAudio) return;
  if (t.segments?.length) t.prev = { model: t.model, segments: t.segments, edited: t.edited };
  t.model = modelId; t.edited = false; t.status = "queued";
  await saveTurn(t); updateTurn(t);
  S.queue.push(t.id); pumpQueue();
}

// ── Playback ─────────────────────────────────────────────────────────────
const player = $("#player");
let urlFor = { id: null, url: null };
async function play(t, at) {
  if (!t.hasAudio) return;
  if (urlFor.id !== t.id) {
    const blob = await db.get("audio", t.id);
    if (!blob) return toast("Audio was cleared for this clip");
    if (urlFor.url) URL.revokeObjectURL(urlFor.url);
    urlFor = { id: t.id, url: URL.createObjectURL(blob) };
    player.src = urlFor.url;
    player.playbackRate = prefs.rate;
    $$(".turn.playing, .turn.seeked").forEach((e) => e.classList.remove("playing", "seeked"));
    $$(".seg.now").forEach((e) => e.classList.remove("now"));
  }
  S.playing = { id: t.id, on: true };
  if (at != null) player.currentTime = at;
  try { await player.play(); } catch {}
}
function togglePlay(t) {
  if (S.playing?.id === t.id && !player.paused) player.pause();
  else play(t);
}
function syncPlayer() {
  const t = S.turns.get(S.playing?.id);
  if (!t) return;
  const el = $(`#t-${t.id}`);
  if (!el) return;
  const dur = player.duration && isFinite(player.duration) ? player.duration : t.duration;
  const p = dur ? player.currentTime / dur : 0;
  const scrub = $(".scrub", el);
  if (scrub) { $(".cursor", scrub).style.left = p * 100 + "%"; $(".played", scrub).style.width = p * 100 + "%"; }
  const now = player.currentTime;
  let idx = -1;
  (t.segments || []).forEach((s, i) => { if (now >= s.start - 0.05 && now < s.end + 0.3) idx = i; });
  $$(".seg.now", el).forEach((e) => e.classList.remove("now"));
  if (idx >= 0) $(`.seg[data-i="${idx}"]`, el)?.classList.add("now");
}
function onPlayState() {
  const on = !player.paused;
  if (S.playing) S.playing.on = on;
  $$(".turn").forEach((el) => {
    const me = el.dataset.id === S.playing?.id;
    el.classList.toggle("playing", me && on);
    el.classList.toggle("seeked", me);
    const b = $(".play", el);
    b.innerHTML = icon(me && on ? "pause" : "play");
    b.setAttribute("aria-label", me && on ? "Pause" : "Play");
  });
  cancelAnimationFrame(onPlayState.raf);
  const loop = () => { syncPlayer(); if (!player.paused) onPlayState.raf = requestAnimationFrame(loop); };
  loop();
  if (!on) $$(".seg.now").forEach((e) => e.classList.remove("now"));
}
player.addEventListener("play", onPlayState);
player.addEventListener("pause", onPlayState);
player.addEventListener("ended", onPlayState);
player.addEventListener("seeked", syncPlayer);

// ── Editing ──────────────────────────────────────────────────────────────
function startEdit(t) {
  if (S.editing) finishEdit();
  S.editing = t.id;
  const el = $(`#t-${t.id}`);
  el.outerHTML = turnHtml(t);
  drawSono(t);
  $(`#t-${t.id} .seg`)?.focus();
}
async function finishEdit() {
  const t = S.turns.get(S.editing);
  S.editing = null;
  if (!t) return;
  const el = $(`#t-${t.id}`);
  if (el) {
    let changed = false;
    let learn = null;
    $$(".seg", el).forEach((s) => {
      const seg = t.segments[+s.dataset.i];
      const v = s.textContent.replace(/\s+/g, " ").trim();
      if (seg && seg.text !== v) { learn ||= suggestRule(seg.text, v); seg.text = v; changed = true; }
    });
    if (learn && !prefs.corrections.some((r) => r.from.toLowerCase() === learn.from.toLowerCase())) {
      toast(`Always write “${learn.from}” as “${learn.to}”?`, { label: "Remember", run: () => { prefs.corrections.push(learn); savePrefs(); toast("Added to corrections"); } }, 8000);
    }
    t.segments = t.segments.filter((s) => s.text);
    if (changed) { t.edited = true; await saveTurn(t); touch(t.sessionId); }
    el.outerHTML = turnHtml(t);
    drawSono(t);
  }
}

/** A small word-level edit (1–3 words) is a candidate correction rule. */
function suggestRule(before, after) {
  const a = before.split(/\s+/), b = after.split(/\s+/);
  let i = 0; while (i < a.length && i < b.length && a[i] === b[i]) i++;
  let j = 0; while (j < a.length - i && j < b.length - i && a[a.length - 1 - j] === b[b.length - 1 - j]) j++;
  const strip = (w) => w.join(" ").replace(/^[^\p{L}\p{N}]+|[^\p{L}\p{N}]+$/gu, "");
  const from = strip(a.slice(i, a.length - j)), to = strip(b.slice(i, b.length - j));
  const n = (x) => x.split(/\s+/).length;
  if (!from || !to || from.toLowerCase() === to.toLowerCase() || n(from) > 3 || n(to) > 4) return null;
  return { from, to };
}

// ── Deleting with undo ───────────────────────────────────────────────────
async function removeTurn(t, undoable = true) {
  if (S.playing?.id === t.id) { player.pause(); S.playing = null; }
  const blob = undoable ? await db.get("audio", t.id) : null;
  S.turns.delete(t.id);
  S.queue = S.queue.filter((x) => x !== t.id);
  if (jobs.has(t.id) && t.status !== "recording") { W?.postMessage({ type: "cancel", job: t.id }); }
  await db.del("turns", t.id); await db.del("audio", t.id); await db.delByIndex("pcm", "turn", t.id);
  $(`#t-${t.id}`)?.remove();
  if (S.sid === t.sessionId && !turnsOf(t.sessionId).length) renderPage();
  renderHeaderMeta(); renderSidebar();
  if (undoable) toast("Clip deleted", { label: "Undo", run: async () => {
    S.turns.set(t.id, t); await saveTurn(t); if (blob) await db.put("audio", blob, t.id);
    if (S.sid === t.sessionId) renderPage(); renderSidebar();
  } }, 6000);
}
async function removeSession(sid) {
  const s = S.sessions.get(sid);
  if (!s) return;
  if (S.rec?.turn.sessionId === sid) return toast("Stop recording first");
  const turns = turnsOf(sid);
  const blobs = await Promise.all(turns.map((t) => db.get("audio", t.id)));
  for (const t of turns) { S.turns.delete(t.id); await db.del("turns", t.id); await db.del("audio", t.id); }
  S.sessions.delete(sid); await db.del("sessions", sid);
  if (S.playing && turns.some((t) => t.id === S.playing.id)) { player.pause(); S.playing = null; }
  if (S.sid === sid) { history.replaceState(null, "", "#/"); openSession(null); } else renderSidebar();
  toast(`Deleted “${s.title}”`, { label: "Undo", run: async () => {
    S.sessions.set(sid, s); await saveSession(s);
    for (const [i, t] of turns.entries()) { S.turns.set(t.id, t); await saveTurn(t); if (blobs[i]) await db.put("audio", blobs[i], t.id); }
    location.hash = `#/s/${sid}`; renderSidebar();
  } }, 7000);
  updateStorage();
}

// ── Export / import ──────────────────────────────────────────────────────
function plainText(t) {
  return paragraphs(t).map((p) => p.segs.map(([s]) => s.text).join(" ")).join("\n\n");
}
function sessionMarkdown(s, stamps = true) {
  const turns = turnsOf(s.id);
  let md = `# ${s.title}\n\n_${new Date(s.createdAt).toLocaleString()} · ${turns.length} clip${turns.length === 1 ? "" : "s"} · ${fmtDur(turns.reduce((a, t) => a + (t.duration || 0), 0))}_\n`;
  for (const t of turns) {
    md += `\n## ${t.kind === "upload" ? t.name : "Dictation"} — ${fmtTime(t.createdAt)} (${fmtDur(t.duration)})\n\n`;
    md += paragraphs(t).map((p) => (stamps ? `**[${fmtDur(p.start)}]** ` : "") + p.segs.map(([x]) => x.text).join(" ")).join("\n\n") + "\n";
  }
  return md;
}
const sessionText = (s) => turnsOf(s.id).map(plainText).filter(Boolean).join("\n\n");
const slug = (s) => s.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "").slice(0, 60) || "jay";
function download(name, blob) {
  const a = document.createElement("a");
  a.href = URL.createObjectURL(blob); a.download = name;
  document.body.append(a); a.click(); a.remove();
  setTimeout(() => URL.revokeObjectURL(a.href), 4000);
}
async function copy(text, what = "Copied", html) {
  try {
    if (html && window.ClipboardItem && navigator.clipboard.write) {
      await navigator.clipboard.write([new ClipboardItem({ "text/plain": new Blob([text], { type: "text/plain" }), "text/html": new Blob([html], { type: "text/html" }) })]);
    } else await navigator.clipboard.writeText(text);
    toast(what);
  } catch { try { await navigator.clipboard.writeText(text); toast(what); } catch { toast("Clipboard unavailable"); } }
}
/** Paste-friendly HTML (paragraphs, bold report section labels) for notes/EHR fields. */
function turnHtmlCopy(t) {
  return paragraphs(t).map((p) => `<p>${p.segs.map(([x]) => (x.sec ? esc(x.text).replace(/^([^:]{2,40}:)/, "<b>$1</b>") : esc(x.text))).join(" ")}</p>`).join("");
}
const sessionHtmlCopy = (s) => turnsOf(s.id).map(turnHtmlCopy).join("");
function exportSession(s, kind) {
  if (kind === "md") download(`${slug(s.title)}.md`, new Blob([sessionMarkdown(s)], { type: "text/markdown" }));
  if (kind === "txt") download(`${slug(s.title)}.txt`, new Blob([sessionText(s)], { type: "text/plain" }));
  if (kind === "vtt") {
    const ts = (x) => { const h = Math.floor(x / 3600), m = Math.floor((x % 3600) / 60), sec = (x % 60).toFixed(3).padStart(6, "0"); return `${String(h).padStart(2, "0")}:${String(m).padStart(2, "0")}:${sec}`; };
    let n = 0;
    const body = turnsOf(s.id).map((t) => `NOTE ${t.kind === "upload" ? t.name : "Dictation"} · ${new Date(t.createdAt).toLocaleString()}\n\n` +
      (t.segments || []).map((x) => `${++n}\n${ts(x.start)} --> ${ts(Math.max(x.end, x.start + 0.5))}\n${x.text}\n`).join("\n")).join("\n");
    download(`${slug(s.title)}.vtt`, new Blob([`WEBVTT\n\n${body}`], { type: "text/vtt" }));
  }
  if (kind === "json") download(`${slug(s.title)}.json`, new Blob([JSON.stringify({ session: s, turns: turnsOf(s.id).map(cleanTurn) }, null, 2)], { type: "application/json" }));
}
const cleanTurn = (t) => { const o = strip(t); delete o.sono; return o; };
async function b64(blob) {
  return new Promise((r) => { const f = new FileReader(); f.onload = () => r(String(f.result).split(",")[1]); f.readAsDataURL(blob); });
}
async function exportBackup(withAudio) {
  const k = toast("Preparing backup…", null, 60000);
  const parts = [`{"jay":1,"exported":${Date.now()},"sessions":${JSON.stringify([...S.sessions.values()])},"turns":${JSON.stringify([...S.turns.values()].map(cleanTurn))},"audio":{`];
  if (withAudio) {
    let first = true;
    for (const t of S.turns.values()) {
      const blob = t.hasAudio && (await db.get("audio", t.id));
      if (!blob) continue;
      parts.push(`${first ? "" : ","}${JSON.stringify(t.id)}:${JSON.stringify({ type: blob.type, name: blob.name || null, data: await b64(blob) })}`);
      first = false;
    }
  }
  parts.push("}}");
  k();
  download(`jay-backup-${new Date().toISOString().slice(0, 10)}.json`, new Blob(parts, { type: "application/json" }));
}
async function exportAllMarkdown() {
  const list = [...S.sessions.values()].sort((a, b) => a.createdAt - b.createdAt);
  download(`jay-notes-${new Date().toISOString().slice(0, 10)}.md`, new Blob([list.map((s) => sessionMarkdown(s)).join("\n\n---\n\n")], { type: "text/markdown" }));
}
async function importBackup(file) {
  let data;
  try { data = JSON.parse(await file.text()); } catch { return toast("That isn’t a Jay backup"); }
  if (!data?.jay || !Array.isArray(data.sessions)) return toast("That isn’t a Jay backup");
  let n = 0;
  for (const s of data.sessions) {
    const have = S.sessions.get(s.id);
    if (have && have.updatedAt >= s.updatedAt) continue;
    S.sessions.set(s.id, s); await saveSession(s); n++;
  }
  for (const t of data.turns || []) {
    const have = S.turns.get(t.id);
    if (have && (have.updatedAt || have.createdAt) >= (t.updatedAt || t.createdAt) && have.segments?.length) continue;
    const a = data.audio?.[t.id];
    if (a) {
      const bin = Uint8Array.from(atob(a.data), (c) => c.charCodeAt(0));
      await db.put("audio", new Blob([bin], { type: a.type }), t.id);
      t.hasAudio = true;
    } else if (!(await db.get("audio", t.id))) t.hasAudio = false;
    if (t.status !== "done" && t.status !== "error") t.status = "done";
    S.turns.set(t.id, t); await saveTurn(t);
  }
  renderSidebar(); renderPage(); updateStorage();
  toast(`Imported ${n} session${n === 1 ? "" : "s"}`);
}

// ── Storage ──────────────────────────────────────────────────────────────
async function storageInfo() {
  const est = (await navigator.storage?.estimate?.()) || {};
  let audio = 0;
  const blobs = await db.all("audio");
  for (const b of blobs) audio += b?.size || 0;
  let models = 0;
  for (const m of allModels()) if (S.cache[m.id]?.state === "cached") models += m.size || 0;
  const usage = Math.max(est.usage || 0, audio + models);
  return { usage, quota: est.quota || 0, audio, models, other: Math.max(0, usage - audio - models), persisted: await navigator.storage?.persisted?.() };
}
async function updateStorage() {
  const i = await storageInfo();
  $("#store-label").textContent = fmtBytes(i.usage);
  $("#store-meter").title = `Storage · audio ${fmtBytes(i.audio)} · models ${fmtBytes(i.models)}`;
  return i;
}
let persistAsked = false;
async function persistQuietly() {
  if (persistAsked || !navigator.storage?.persist) return;
  persistAsked = true;
  try { if (!(await navigator.storage.persisted())) await navigator.storage.persist(); } catch {}
}
function armed(btn, run) {
  if (btn.classList.contains("armed")) { btn.classList.remove("armed"); run(); return; }
  const label = btn.innerHTML;
  btn.classList.add("armed"); btn.innerHTML = `${icon("trash")}Tap again to confirm`;
  setTimeout(() => { if (btn.isConnected) { btn.classList.remove("armed"); btn.innerHTML = label; } }, 3500);
}

// ── Settings sheet ───────────────────────────────────────────────────────
const sheet = $("#settings");
let tab = "models";
function openSettings(t = tab) {
  tab = t;
  if (!sheet.open) sheet.showModal();
  renderSettings();
  refreshCache();
  if (!S.runtime) worker().postMessage({ type: "runtime" });
}
function renderSettingsIfOpen() {
  if (!sheet.open) return;
  const a = document.activeElement;
  if (a && sheet.contains(a) && a.matches("input, select, textarea")) return;
  renderSettings();
}
async function renderSettings() {
  $$(".tabs button", sheet).forEach((b) => b.setAttribute("aria-selected", b.dataset.tab === tab));
  const body = $("#sheet-body");
  if (tab === "models") body.innerHTML = modelsHtml();
  else if (tab === "storage") { const i = await storageInfo(); if (tab === "storage") body.innerHTML = storageHtml(i); }
  else body.innerHTML = prefsHtml();
  if (tab === "prefs") fillMics();
}
function modelRow(m) {
  const st = S.model[m.id], c = S.cache[m.id];
  const sel = prefs.model === m.id;
  let state = "", action = "";
  if (st?.state === "loading") {
    const p = st.total ? Math.round((st.got / st.total) * 100) : 0;
    state = st.stage === "download" ? `Downloading · ${p}%` : st.stage === "compile" ? "Preparing…" : "Loading…";
    action = `<div class="mbar"><i style="width:${p}%"></i></div>`;
  } else if (st?.state === "ready") state = "Loaded";
  else if (st?.state === "error") state = `Error · ${st.error}`;
  else if (c?.state === "cached" || m.builtin) state = m.builtin ? "Bundled" : "Downloaded";
  else state = `${fmtBytes(m.size || 0)} download`;
  const removable = !m.builtin && (c?.state === "cached" || c?.state === "partial" || m.custom);
  return `<div class="row" data-model="${esc(m.id)}">
    <button class="radio${sel ? " on" : ""}" data-act="pick-model" aria-label="Use ${esc(m.name)}" aria-pressed="${sel}"></button>
    <div class="grow">
      <div class="name">${esc(m.name)}${(m.tags || []).map((t) => `<span class="tag ${t}">${t === "med" ? "Medical" : t === "in" ? "Built-in" : esc(t)}</span>`).join("")}${m.custom ? `<span class="tag">Custom</span>` : ""}</div>
      <div class="desc">${esc(m.blurb || "")}</div>
      <div class="fine">${esc(state)}${m.license ? ` · ${m.link ? `<a href="${esc(m.link)}" target="_blank" rel="noopener">${esc(m.license)}</a>` : esc(m.license)}` : ""}</div>
      ${action}
    </div>
    ${!sel && c?.state !== "cached" && !m.builtin && st?.state !== "loading" && !m.custom ? `<button class="btn" data-act="get-model">${icon("download")}Get</button>` : ""}
    ${removable ? `<button class="icon-btn" data-act="del-model" title="Remove from this device" aria-label="Remove">${icon("trash")}</button>` : ""}
  </div>`;
}
function modelsHtml() {
  const r = S.runtime;
  return `<h3>Speech models</h3>
  ${allModels().map(modelRow).join("")}
  <details class="custom"><summary>${icon("plus")}Add your own model</summary>
    <form id="custom-form">
      <label class="field"><span>Name</span><input type="text" name="name" required placeholder="My fine-tuned model"></label>
      <label class="field"><span>Architecture</span><select name="kind">
        <option value="moonshine">Moonshine-style encoder + merged decoder</option>
        <option value="ctc">MedASR / LASR-style CTC (128 log-mel)</option></select></label>
      <label class="field"><span>Hugging Face repo or folder URL</span><input type="url" name="url" placeholder="https://huggingface.co/onnx-community/moonshine-base-ONNX"></label>
      <label class="field"><span>…or local files</span><input type="file" name="local" multiple accept=".onnx,.json,.txt"></label>
      <div class="fine" style="font:11.5px var(--mono);color:var(--ink-3)">Moonshine: encoder_model*.onnx, decoder_model_merged*.onnx, tokenizer.json, config.json · CTC: model*.onnx, tokens.txt</div>
      <div class="btns" style="margin-top:12px"><button class="btn primary" type="submit">${icon("plus")}Add model</button></div>
    </form>
  </details>
  <div class="about">Runtime · ONNX Runtime Web ${r ? `${esc(r.version?.split(" ").pop() || "")} · WASM SIMD · ${r.threads} thread${r.threads > 1 ? "s" : ""}${r.isolated ? "" : " (single-threaded mode)"}` : "· not started"}<br>
  Audio is processed in this tab only; model files are fetched once and cached.<br>
  Transcripts are drafts — review before clinical use.</div>`;
}
function storageHtml(i) {
  const seg = (v, c) => (i.usage ? `<i style="width:${(v / i.usage) * 100}%;background:${c}"></i>` : "");
  return `<h3>On this device</h3>
  <div class="usage">${seg(i.audio, "var(--jay)")}${seg(i.models, "var(--fawn)")}${seg(i.other, "var(--line-2)")}</div>
  <div class="legend"><span><b style="background:var(--jay)"></b>Audio ${fmtBytes(i.audio)}</span><span><b style="background:var(--fawn)"></b>Models ${fmtBytes(i.models)}</span><span><b style="background:var(--line-2)"></b>Other ${fmtBytes(i.other)}</span>${i.quota ? `<span>of ${fmtBytes(i.quota)}</span>` : ""}</div>
  <div class="row"><div class="grow"><div class="name">${S.sessions.size} session${S.sessions.size === 1 ? "" : "s"} · ${S.turns.size} clip${S.turns.size === 1 ? "" : "s"}</div><div class="fine">${i.persisted ? "Protected from automatic browser cleanup" : "Browser may evict data under storage pressure"}</div></div>
  ${i.persisted ? "" : `<button class="btn" data-act="persist">Keep</button>`}</div>
  <h3>Backup</h3>
  <div class="btns">
    <button class="btn" data-act="backup-audio">${icon("download")}Full backup</button>
    <button class="btn" data-act="backup-text">${icon("download")}Transcripts only</button>
    <button class="btn" data-act="export-all-md">${icon("file")}All notes as Markdown</button>
    <label class="btn">${icon("upload")}Restore…<input type="file" accept=".json,application/json" data-act="restore-file" hidden></label>
  </div>
  <h3>Clean up</h3>
  <div class="btns">
    <button class="btn danger" data-act="clear-audio">${icon("trash")}Delete audio, keep text</button>
    <button class="btn danger" data-act="clear-models">${icon("trash")}Remove downloaded models</button>
    <button class="btn danger" data-act="clear-all">${icon("trash")}Erase everything</button>
  </div>`;
}
function prefsHtml() {
  const sw = (k, label, fine) => `<div class="row"><div class="grow"><div class="name">${label}</div>${fine ? `<div class="fine">${fine}</div>` : ""}</div><button class="switch" role="switch" aria-checked="${!!prefs[k]}" data-pref="${k}" aria-label="${label}"></button></div>`;
  const segc = (k, opts) => `<div class="seg-ctl">${opts.map(([v, l, ic]) => `<button data-prefv="${k}" data-v="${v}" aria-pressed="${String(prefs[k]) === String(v)}">${ic ? icon(ic) : ""}${l}</button>`).join("")}</div>`;
  return `<h3>Appearance</h3>
  <div class="row"><div class="grow"><div class="name">Theme</div></div>${segc("theme", [["system", "Auto", "auto"], ["light", "Day", "sun"], ["dark", "Dusk", "moon"]])}</div>
  ${sw("timestamps", "Timestamps in transcripts")}
  <h3>Dictation</h3>
  <div class="row"><div class="grow"><div class="name">Microphone</div></div><select id="mic-select" class="field" style="margin:0;height:36px;border-radius:8px;border:1px solid var(--line);background:var(--paper);padding:0 8px;max-width:240px"><option value="">System default</option></select></div>
  <div class="row"><div class="grow"><div class="name">Phrase pause</div><div class="fine">Silence before a phrase is finalised</div></div>${segc("pause", [[0.45, "Quick"], [0.6, "Normal"], [0.9, "Relaxed"]])}</div>
  ${sw("noise", "Noise suppression", "Browser echo/noise filtering on the mic")}
  ${sw("commands", "Spoken punctuation", "“comma”, “full stop”, “new paragraph” → , . ¶ (always on for MedASR)")}
  <div class="row"><div class="grow"><div class="name">Playback speed</div></div>${segc("rate", [[1, "1×"], [1.25, "1.25×"], [1.5, "1.5×"], [2, "2×"]])}</div>
  <h3>Corrections</h3>
  <div id="corrections">${prefs.corrections.map((r, i) => corrRow(r, i)).join("")}</div>
  <div class="btns"><button class="btn" data-act="corr-add">${icon("plus")}Add</button>${prefs.corrections.length ? `<button class="btn" data-act="corr-apply">${icon("redo")}Apply to existing notes</button>` : ""}</div>
  <h3>Startup</h3>
  ${sw("preload", "Warm up model on open")}`;
}
const corrRow = (r, i) => `<div class="corr" data-i="${i}"><input type="text" value="${esc(r.from)}" placeholder="heard" data-corr="from" aria-label="Heard"><span>→</span><input type="text" value="${esc(r.to)}" placeholder="write" data-corr="to" aria-label="Write"><button class="icon-btn" data-act="corr-del" aria-label="Remove">${icon("x")}</button></div>`;
async function fillMics() {
  const sel = $("#mic-select");
  if (!sel || !navigator.mediaDevices?.enumerateDevices) return;
  const devs = (await navigator.mediaDevices.enumerateDevices()).filter((d) => d.kind === "audioinput" && d.deviceId && d.deviceId !== "default");
  for (const d of devs) sel.insertAdjacentHTML("beforeend", `<option value="${esc(d.deviceId)}">${esc(d.label || "Microphone")}</option>`);
  sel.value = prefs.mic;
  sel.onchange = () => { prefs.mic = sel.value; savePrefs(); };
}
async function addCustom(form) {
  const f = new FormData(form);
  const name = String(f.get("name") || "").trim();
  const kind = f.get("kind");
  const url = String(f.get("url") || "").trim().replace(/\/(tree|blob)\/([^/]+)\/?.*$/, "/resolve/$2/").replace(/\/?$/, "/");
  const local = [...form.local.files];
  const id = "custom-" + uid();
  let files = {};
  const pick = (re) => local.find((x) => re.test(x.name));
  if (local.length) {
    const map = kind === "moonshine"
      ? { encoder: pick(/encoder.*\.onnx$/i), decoder: pick(/decoder.*merged.*\.onnx$/i) || pick(/decoder.*\.onnx$/i), vocab: pick(/tokenizer\.json$|vocab\.json$/i), config: pick(/^config\.json$/i) }
      : { model: pick(/\.onnx$/i), tokens: pick(/tokens\.txt$/i) };
    const cache = await caches.open("jay-models-v1");
    for (const [k, file] of Object.entries(map)) {
      if (!file) { if (k === "config") continue; return toast(`Missing ${k} file`); }
      const u = new URL(`./local-models/${id}/${file.name}`, location.href).href;
      await cache.put(u, new Response(file));
      files[k] = { url: u, local: true, size: file.size };
    }
  } else if (/^https?:/.test(url)) {
    const base = /huggingface\.co\/[^/]+\/[^/]+\/$/.test(url) ? url + "resolve/main/" : url;
    files = kind === "moonshine"
      ? { encoder: { url: base + "onnx/encoder_model_quantized.onnx" }, decoder: { url: base + "onnx/decoder_model_merged_quantized.onnx" }, vocab: { url: base + "tokenizer.json" }, config: { url: base + "config.json", optional: true } }
      : { model: { url: base + "model_int8.onnx" }, tokens: { url: base + "tokens.txt" } };
  } else return toast("Add a URL or choose files");
  const size = Object.values(files).reduce((s, x) => s + (x.size || 0), 0);
  prefs.custom.push({ id, name: name || "Custom model", kind, files, custom: true, size, format: kind === "ctc" ? "medasr" : undefined, blurb: local.length ? "Local files" : url, tags: [] });
  savePrefs();
  selectModel(id);
  toast(`Added ${name || "custom model"}`);
}
function selectModel(id) {
  if (S.rec) return toast("Stop recording to switch models");
  prefs.model = id; savePrefs();
  ensureModel(current());
  renderChip(); renderSettingsIfOpen();
  if (!S.sessions.get(S.sid) || !turnsOf(S.sid).length) renderPage();
}
async function deleteModel(id) {
  const m = modelById(id);
  worker().postMessage({ type: "delete-model", spec: specOf(m), specs: allModels().map(specOf) });
  delete S.model[id]; S.cache[id] = { state: "none" };
  if (m.custom) {
    prefs.custom = prefs.custom.filter((x) => x.id !== id);
    const c = await caches.open("jay-models-v1");
    for (const f of Object.values(m.files)) if (f.local) await c.delete(f.url);
  }
  if (prefs.model === id) prefs.model = "moonshine-tiny";
  savePrefs(); renderChip(); renderSettings(); updateStorage();
}

// ── Model chip ───────────────────────────────────────────────────────────
function renderChip() {
  const m = current(), st = S.model[m.id];
  $("#model-name").innerHTML = `<span class="full">${esc(m.name)}</span><span class="short">${esc(m.short || m.name)}</span>`;
  const dot = $("#model-chip .dot");
  dot.className = "dot " + (st?.state || "");
  dot.style.setProperty("--p", st?.total ? Math.round((st.got / st.total) * 100) : 15);
  $("#model-chip").title = st?.state === "loading" ? loadText(m) : st?.state === "error" ? st.error : `${m.name} · ${m.blurb}`;
  if (!S.rec) idleStrip();
}
function chipMenu() {
  menu($("#model-chip"), [
    { head: "Transcribe with" },
    ...allModels().map((m) => ({
      label: m.name, icon: prefs.model === m.id ? "check" : "chip", on: prefs.model === m.id,
      sub: S.model[m.id]?.state === "loading" ? "loading…" : m.builtin || S.cache[m.id]?.state === "cached" ? "" : fmtBytes(m.size || 0),
      run: () => selectModel(m.id),
    })),
    "-",
    { label: "Manage models…", icon: "gear", run: () => openSettings("models") },
  ]);
}

// ── Drawer (phone): the system back gesture closes it instead of leaving the session ──
const drawerIsOverlay = () => matchMedia("(max-width: 760px)").matches;
function openDrawer() {
  if ($("#app").classList.contains("drawer")) return;
  $("#app").classList.add("drawer");
  if (drawerIsOverlay()) history.pushState({ jayDrawer: 1 }, "");
}
function closeDrawer() {
  if (!$("#app").classList.contains("drawer")) return;
  $("#app").classList.remove("drawer");
  if (history.state?.jayDrawer) history.back();
}
addEventListener("popstate", () => $("#app").classList.remove("drawer"));

// ── Theme ────────────────────────────────────────────────────────────────
function applyTheme() {
  const t = prefs.theme;
  if (t === "system") document.documentElement.removeAttribute("data-theme");
  else document.documentElement.dataset.theme = t;
  $("#theme-btn").innerHTML = icon(t === "light" ? "sun" : t === "dark" ? "moon" : "auto");
  $("#theme-btn").title = `Theme: ${t === "system" ? "auto" : t === "light" ? "day" : "dusk"}`;
  requestAnimationFrame(() => turnsOf(S.sid).forEach(drawSono));
}

// ── Events ───────────────────────────────────────────────────────────────
$("#rec").onclick = () => (S.rec ? stopRec() : startRec());
$("#pause").onclick = togglePause;
$("#upload").onclick = () => $("#file").click();
$("#file").onchange = (e) => { addFiles(e.target.files); e.target.value = ""; };
$("#model-chip").onclick = chipMenu;
$("#new-session").onclick = newSession;
$("#settings-btn").onclick = () => openSettings(tab);
$("#store-meter").onclick = () => openSettings("storage");
$("#theme-btn").onclick = () => { prefs.theme = { system: "light", light: "dark", dark: "system" }[prefs.theme]; savePrefs(); applyTheme(); };
$("#menu-btn").onclick = () => openDrawer();
$("#scrim").onclick = () => closeDrawer();
$("#search").addEventListener("input", (e) => { S.query = e.target.value; clearTimeout(renderSidebar.t); renderSidebar.t = setTimeout(() => { renderSidebar(); if (S.sid) $$(".turn").forEach((el) => { const t = S.turns.get(el.dataset.id); if (t && S.editing !== t.id) $(".text", el).innerHTML = textHtml(t); }); }, 90); });
$("#search").addEventListener("keydown", (e) => {
  if (e.key === "Escape") { e.target.value = ""; S.query = ""; renderSidebar(); renderPage(); e.target.blur(); }
  if (e.key === "Enter") $("#sessions a")?.click();
});
$$(".tabs button", sheet).forEach((b) => (b.onclick = () => { tab = b.dataset.tab; renderSettings(); }));
sheet.addEventListener("click", (e) => { if (e.target === sheet) sheet.close(); });

const title = $("#title");
title.addEventListener("click", () => {
  if (!S.sessions.get(S.sid) || title.isContentEditable) return;
  title.contentEditable = "true"; title.focus();
  getSelection().selectAllChildren(title);
});
title.addEventListener("keydown", (e) => { if (e.key === "Enter") { e.preventDefault(); title.blur(); } if (e.key === "Escape") { title.textContent = S.sessions.get(S.sid)?.title; title.blur(); } });
title.addEventListener("blur", async () => {
  title.contentEditable = "false";
  const s = S.sessions.get(S.sid);
  const v = title.textContent.trim();
  if (s && v && v !== s.title) { s.title = v; s.autoTitle = false; await saveSession(s); renderSidebar(); }
  if (s) title.textContent = s.title;
});

document.addEventListener("click", async (e) => {
  const el = e.target.closest("[data-act]");
  if (!el) {
    const seg = e.target.closest(".seg, .ts");
    if (seg?.matches(".ts") && seg.closest(".editing")) return;
    if (seg && !e.target.closest(".editing") && !getSelection().toString()) {
      const t = S.turns.get(seg.closest(".turn").dataset.id);
      play(t, +(seg.dataset.t ?? seg.dataset.seek));
    }
    return;
  }
  const act = el.dataset.act;
  const turnEl = el.closest(".turn");
  const t = turnEl && S.turns.get(turnEl.dataset.id);
  const s = S.sessions.get(S.sid);
  const row = el.closest("[data-model]");
  switch (act) {
    case "record": startRec(); break;
    case "upload": $("#file").click(); break;
    case "use-medasr": selectModel("medasr"); break;
    case "play": togglePlay(t); break;
    case "copy": copy(plainText(t), "Transcript copied", turnHtmlCopy(t)); break;
    case "edit": startEdit(t); break;
    case "edit-done": finishEdit(); break;
    case "retry": retranscribe(t, t.model); break;
    case "retry-tiny": retranscribe(t, "moonshine-tiny"); break;
    case "stop-turn":
      if (S.queue.includes(t.id)) {
        S.queue = S.queue.filter((x) => x !== t.id);
        t.status = "error"; t.error = "Not transcribed"; await saveTurn(t); updateTurn(t); idleStrip();
      } else if (t.status === "transcribing") { stopping.add(t.id); W?.postMessage({ type: "cancel", job: t.id }); el.disabled = true; }
      break;
    case "retry-model": delete S.model[prefs.model]; ensureModel(); break;
    case "corr-add": prefs.corrections.push({ from: "", to: "" }); savePrefs(); renderSettings().then(() => $$("#corrections input[data-corr=from]").at(-1)?.focus()); break;
    case "corr-del": prefs.corrections.splice(+el.closest(".corr").dataset.i, 1); savePrefs(); renderSettings(); break;
    case "corr-apply": {
      let n = 0;
      for (const x of S.turns.values()) {
        if (!x.segments?.length || x.status === "recording") continue;
        let changed = false;
        for (const seg of x.segments) { const v = applyCorrections(seg.text); if (v !== seg.text) { seg.text = v; changed = true; } }
        if (changed) { n++; await saveTurn(x); }
      }
      renderPage(); renderSidebar();
      toast(n ? `Corrected ${n} clip${n === 1 ? "" : "s"}` : "Nothing to correct");
      break;
    }
    case "restore": if (t.prev) { [t.segments, t.model, t.edited] = [t.prev.segments, t.prev.model, t.prev.edited]; t.prev = null; await saveTurn(t); turnEl.outerHTML = turnHtml(t); drawSono(t); } break;
    case "turn-more": menu(el, [
      { label: "Copy text", icon: "copy", run: () => copy(plainText(t), "Transcript copied", turnHtmlCopy(t)) },
      { label: "Edit text", icon: "edit", run: () => startEdit(t) },
      ...(t.hasAudio && !S.rec && t.status !== "recording" ? [{ label: `Re-transcribe with ${current().name}`, icon: "redo", run: () => retranscribe(t) }] : []),
      ...(t.hasAudio ? [{ label: "Download audio", icon: "download", run: async () => { const b = await db.get("audio", t.id); if (b) download(t.kind === "upload" ? t.name : `${slug(s?.title || "dictation")}-${fmtTime(t.createdAt).replace(":", "")}.wav`, b); } }] : []),
      "-",
      ...(t.status === "recording" ? [] : [{ label: "Delete clip", icon: "trash", danger: true, run: () => removeTurn(t) }]),
    ]); break;
    case "copy-session": if (s) copy(sessionText(s), "Session copied", sessionHtmlCopy(s)); break;
    case "export-session": if (s) menu(el, [
      { label: "Markdown", sub: ".md", icon: "file", run: () => exportSession(s, "md") },
      { label: "Plain text", sub: ".txt", icon: "file", run: () => exportSession(s, "txt") },
      { label: "JSON with timings", sub: ".json", icon: "file", run: () => exportSession(s, "json") },
      { label: "Subtitles", sub: ".vtt", icon: "file", run: () => exportSession(s, "vtt") },
    ]); break;
    case "session-more": if (s) menu(el, [
      { label: "Rename", icon: "edit", run: () => title.click() },
      { label: "Copy as Markdown", icon: "copy", run: () => copy(sessionMarkdown(s), "Markdown copied") },
      "-",
      { label: "Delete session", icon: "trash", danger: true, run: () => removeSession(s.id) },
    ]); break;
    case "scrub": {
      if (!t?.hasAudio) break;
      const r = el.getBoundingClientRect();
      const p = Math.max(0, Math.min(1, (e.clientX - r.left) / r.width));
      play(t, p * (t.duration || 0));
      break;
    }
    case "pick-model": selectModel(row.dataset.model); break;
    case "get-model": { const m = modelById(row.dataset.model); ensureModel(m); renderSettings(); break; }
    case "del-model": deleteModel(row.dataset.model); break;
    case "persist": await navigator.storage?.persist?.(); renderSettings(); break;
    case "backup-audio": exportBackup(true); break;
    case "backup-text": exportBackup(false); break;
    case "export-all-md": exportAllMarkdown(); break;
    case "clear-audio": armed(el, async () => {
      player.pause(); S.playing = null;
      await db.clear("audio");
      for (const x of S.turns.values()) if (x.hasAudio) { x.hasAudio = false; await saveTurn(x); }
      renderPage(); renderSettings(); updateStorage(); toast("Audio deleted · transcripts kept");
    }); break;
    case "clear-models": armed(el, async () => {
      for (const m of allModels()) if (!m.builtin) worker().postMessage({ type: "delete-model", spec: specOf(m), specs: allModels().map(specOf) });
      for (const m of allModels()) if (!m.builtin) { delete S.model[m.id]; S.cache[m.id] = { state: "none" }; }
      if (!modelById(prefs.model).builtin) prefs.model = "moonshine-tiny";
      savePrefs(); renderChip(); renderSettings(); updateStorage(); toast("Downloaded models removed");
    }); break;
    case "clear-all": armed(el, async () => {
      if (S.rec) await stopRec();
      player.pause(); S.playing = null;
      for (const j of jobs.keys()) W?.postMessage({ type: "cancel", job: j });
      for (const st of ["turns", "sessions", "audio", "pcm"]) await db.clear(st);
      S.turns.clear(); S.sessions.clear(); S.queue = [];
      history.replaceState(null, "", "#/"); openSession(null);
      renderSettings(); updateStorage(); toast("Everything erased");
    }); break;
  }
});
document.addEventListener("input", (e) => {
  const f = e.target.dataset?.corr;
  if (!f) return;
  const i = +e.target.closest(".corr").dataset.i;
  prefs.corrections[i][f] = e.target.value;
  savePrefs();
});
document.addEventListener("change", (e) => {
  if (e.target.dataset.act === "restore-file" && e.target.files[0]) importBackup(e.target.files[0]);
});
document.addEventListener("paste", (e) => {
  if (!e.target.closest?.(".editing .seg")) return;
  e.preventDefault();
  document.execCommand("insertText", false, e.clipboardData.getData("text/plain").replace(/\s+/g, " "));
});
document.addEventListener("submit", (e) => {
  if (e.target.id === "custom-form") { e.preventDefault(); addCustom(e.target); }
});
sheet.addEventListener("click", (e) => {
  const sw = e.target.closest("[data-pref]");
  if (sw) { const k = sw.dataset.pref; prefs[k] = !prefs[k]; savePrefs(); sw.setAttribute("aria-checked", prefs[k]); if (k === "timestamps") $("#turns").classList.toggle("notime", !prefs.timestamps); }
  const sv = e.target.closest("[data-prefv]");
  if (sv) {
    const k = sv.dataset.prefv, raw = sv.dataset.v;
    prefs[k] = isNaN(+raw) ? raw : +raw; savePrefs();
    $$(`[data-prefv="${k}"]`, sheet).forEach((b) => b.setAttribute("aria-pressed", b === sv));
    if (k === "theme") applyTheme();
    if (k === "rate") player.playbackRate = prefs.rate;
  }
});
document.addEventListener("focusout", (e) => {
  if (S.editing && !e.relatedTarget?.closest?.(`#t-${S.editing}`) && e.target.closest?.(`#t-${S.editing}`)) setTimeout(() => { if (S.editing && !document.activeElement?.closest(`#t-${S.editing}`)) finishEdit(); }, 0);
});

addEventListener("keydown", (e) => {
  const typing = e.target.closest("input, textarea, select, [contenteditable='true']");
  if ((e.metaKey || e.ctrlKey) && e.key.toLowerCase() === "k") { e.preventDefault(); openDrawer(); $("#search").focus(); $("#search").select(); return; }
  if (e.key === "Escape") {
    if (!$("#menu").hidden) return closeMenu();
    if (S.editing) return finishEdit();
    closeDrawer();
    return;
  }
  if (typing || e.metaKey || e.ctrlKey || e.altKey || sheet.open) return;
  const k = e.key.toLowerCase();
  if (k === "r") { e.preventDefault(); S.rec ? stopRec() : startRec(); }
  else if (k === "p" && S.rec) { e.preventDefault(); togglePause(); }
  else if (k === "u") { e.preventDefault(); $("#file").click(); }
  else if (k === "n") { e.preventDefault(); newSession(); }
  else if (k === "/") { e.preventDefault(); openDrawer(); $("#search").focus(); }
  else if (e.key === "?") { e.preventDefault(); $("#keys")?.showModal(); }
  else if (k === " " && S.playing && !e.target.closest("button, a, summary")) { e.preventDefault(); player.paused ? player.play() : player.pause(); }
});

let dragDepth = 0;
addEventListener("dragenter", (e) => { if ([...(e.dataTransfer?.types || [])].includes("Files")) { dragDepth++; document.body.classList.add("dragging"); } });
addEventListener("dragleave", () => { if (--dragDepth <= 0) { dragDepth = 0; document.body.classList.remove("dragging"); } });
addEventListener("dragover", (e) => e.preventDefault());
addEventListener("drop", (e) => { e.preventDefault(); dragDepth = 0; document.body.classList.remove("dragging"); if (e.dataTransfer?.files?.length) addFiles(e.dataTransfer.files); });
addEventListener("beforeunload", (e) => { if (S.rec) { flushPcm(S.rec); e.preventDefault(); } });
document.addEventListener("visibilitychange", () => { if (document.hidden && S.rec) flushPcm(S.rec); });

/** Files shared to Jay from other apps (Android share sheet) wait in a cache until the app opens. */
async function importShared() {
  try {
    const cache = await caches.open("jay-share");
    const keys = await cache.keys();
    if (location.search.includes("shared")) history.replaceState(null, "", location.pathname + location.hash);
    if (!keys.length) return;
    const files = [];
    for (const k of keys) {
      const r = await cache.match(k);
      const b = await r.blob();
      files.push(new File([b], decodeURIComponent(r.headers.get("x-name") || "shared audio"), { type: b.type }));
      await cache.delete(k);
    }
    history.replaceState(null, "", "#/"); openSession(null);
    await addFiles(files, true);
    toast(`Transcribing ${files.length} shared file${files.length === 1 ? "" : "s"}`);
  } catch (e) { console.warn("share import", e); }
}

// ── Boot ─────────────────────────────────────────────────────────────────
async function recover() {
  for (const t of S.turns.values()) {
    if (t.status === "recording") {
      const chunks = (await db.byIndex("pcm", "turn", t.id)).sort((a, b) => a.seq - b.seq).map((c) => c.data);
      if (chunks.length) {
        const pcm = concatI16(chunks);
        await db.put("audio", wavBlob(pcm), t.id);
        t.hasAudio = true; t.duration = pcm.length / SR;
        await db.delByIndex("pcm", "turn", t.id);
        t.status = "queued"; t.segments = []; S.queue.push(t.id);
        toast("Recovered an interrupted dictation");
      } else { t.status = t.segments.length ? "done" : "error"; t.error = "Recording was interrupted"; }
      await saveTurn(t);
    } else if (t.status === "transcribing" || t.status === "queued") {
      t.status = "queued"; S.queue.push(t.id); await saveTurn(t);
    }
  }
}
// ── Updates ──────────────────────────────────────────────────────────────
// The service worker installs each deploy atomically. When a new build takes over,
// reload as soon as nothing is in progress (never mid-recording, -upload or -edit).
let updatePending = false;
const busy = () => !!(S.rec || S.starting || S.busy || S.queue.length || S.editing || sheet.open || $(".toast") ||
  document.activeElement?.matches?.("input, textarea, [contenteditable='true']"));
function applyUpdateWhenIdle() {
  if (!updatePending) return;
  if (busy()) return setTimeout(applyUpdateWhenIdle, 2000);
  location.reload();
}
/** HTML and scripts from different builds: drop cached shells and reload once. */
async function heal(why) {
  console.warn("Jay build mismatch:", why);
  if (sessionStorage.getItem("jay.heal") === BUILD) return; // already tried this session
  sessionStorage.setItem("jay.heal", BUILD);
  try { for (const k of await caches.keys()) if (k.startsWith("jay-shell-")) await caches.delete(k); } catch {}
  try { await (await navigator.serviceWorker?.getRegistration("./"))?.update(); } catch {}
  updatePending = true;
  applyUpdateWhenIdle();
}
async function swReady() {
  const page = document.querySelector('meta[name="jay-build"]')?.content;
  if (page !== BUILD) heal("page " + (page || "unstamped"));
  else sessionStorage.removeItem("jay.heal");
  if (!("serviceWorker" in navigator)) return;
  try {
    const hadController = !!navigator.serviceWorker.controller;
    const reg = await navigator.serviceWorker.register("./sw.js", { scope: "./", updateViaCache: "none" });
    if (hadController) navigator.serviceWorker.addEventListener("controllerchange", () => { updatePending = true; applyUpdateWhenIdle(); });
    // Threads need cross-origin isolation, which the service worker provides on the next load.
    if (!crossOriginIsolated && !sessionStorage.getItem("jay.coi") && localStorage.getItem("jay.coi") !== "unsupported") {
      const ok = hadController || await Promise.race([
        new Promise((r) => navigator.serviceWorker.addEventListener("controllerchange", () => r(true), { once: true })),
        sleep(4000).then(() => false),
      ]);
      if (ok && !busy()) {
        sessionStorage.setItem("jay.coi", "1");
        location.reload();
        return new Promise(() => {});
      }
    } else if (!crossOriginIsolated && sessionStorage.getItem("jay.coi")) localStorage.setItem("jay.coi", "unsupported");
    reg.update?.().catch(() => {});
    // Long-lived tabs and installed apps check for a new build every 30 min and when reopened.
    setInterval(() => reg.update?.().catch(() => {}), 30 * 60 * 1000);
    document.addEventListener("visibilitychange", () => { if (document.visibilityState === "visible") reg.update?.().catch(() => {}); });
  } catch (e) { console.warn("Service worker unavailable", e); }
}
async function boot() {
  window.jayBuild = BUILD;
  applyTheme();
  const [sessions, turns] = await Promise.all([db.all("sessions"), db.all("turns")]);
  sessions.forEach((s) => S.sessions.set(s.id, s));
  turns.forEach((t) => S.turns.set(t.id, t));
  await recover();
  route();
  renderChip();
  updateStorage();
  await swReady();
  worker();
  refreshCache();
  if (prefs.preload || S.queue.length) ensureModel();
  pumpQueue();
  importShared();
  if (location.hash === "#/dictate") { history.replaceState(null, "", "#/"); toast("Tap the mic to start dictating"); }
}
boot();
