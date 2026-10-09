// Jay · ASR worker. Owns ONNX Runtime (WASM), model download/cache, live VAD
// streaming and whole-file chunked transcription. Audio never leaves the device.
import * as ort from "./runtime/ort.js";
import { SR, FRAME, frameDb, lasrFeatures, sonogram, planChunks, quietest, formatMedasr, spokenCommands, plain } from "./dsp.js";

const MODEL_CACHE = "jay-models-v1";
const post = (m, t) => postMessage(m, t || []);
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

// ── Bytes: cache-first fetch with progress ───────────────────────────────
async function cached(url) {
  try { const c = await caches.open(MODEL_CACHE); return await c.match(url); } catch { return null; }
}
async function store(url, buf, type = "application/octet-stream") {
  try { const c = await caches.open(MODEL_CACHE); await c.put(url, new Response(buf, { headers: { "content-type": type } })); } catch {}
}
async function download(url, onBytes) {
  const res = await fetch(url, { cache: "no-store" });
  if (!res.ok) throw new Error(`HTTP ${res.status} for ${url.split("/").slice(-1)[0]}`);
  const total = +res.headers.get("content-length") || 0;
  if (!res.body) { const b = await res.arrayBuffer(); onBytes(b.byteLength, total); return b; }
  const reader = res.body.getReader();
  const chunks = [];
  let got = 0;
  for (;;) {
    const { done, value } = await reader.read();
    if (done) break;
    chunks.push(value); got += value.byteLength;
    onBytes(value.byteLength, total);
  }
  const out = new Uint8Array(got);
  let o = 0;
  for (const c of chunks) { out.set(c, o); o += c.byteLength; }
  return out.buffer;
}
async function gunzip(buf) {
  const u = new Uint8Array(buf, 0, 2);
  if (u[0] !== 0x1f || u[1] !== 0x8b) return buf;
  return new Response(new Blob([buf]).stream().pipeThrough(new DecompressionStream("gzip"))).arrayBuffer();
}
async function sha256(buf) {
  const h = new Uint8Array(await crypto.subtle.digest("SHA-256", buf));
  return Array.from(h, (b) => b.toString(16).padStart(2, "0")).join("");
}
const abs = (u, base = self.location.href) => new URL(u, base).href;

/** A file entry: {url} or {parts:[url], sha256, size, gzip}. Cached whole, keyed by first url + sha. */
async function getFile(entry, progress) {
  const key = entry.parts ? `${entry.parts[0]}?v=${(entry.sha256 || "").slice(0, 12)}` : entry.url;
  const hit = await cached(key);
  if (hit) { const b = await hit.arrayBuffer(); progress.add(entry.gzip || entry.size || b.byteLength, true); return b; }
  if (entry.local) throw new Error("Local model files are missing — add the model again.");
  let buf;
  if (entry.parts) {
    const bufs = [];
    for (const p of entry.parts) bufs.push(await download(p, (n) => progress.add(n)));
    const joined = new Uint8Array(bufs.reduce((s, b) => s + b.byteLength, 0));
    let o = 0;
    for (const b of bufs) { joined.set(new Uint8Array(b), o); o += b.byteLength; }
    buf = await gunzip(joined.buffer);
  } else {
    buf = await download(entry.url, (n) => progress.add(n));
  }
  if (entry.sha256 && (await sha256(buf)) !== entry.sha256) throw new Error("Download was corrupted — please retry.");
  await store(key, buf);
  return buf;
}
async function getJson(url) {
  try {
    const r = await fetch(url, { cache: "no-cache" });
    if (r.ok) { const t = await r.text(); await store(url, t, "application/json"); return JSON.parse(t); }
  } catch {}
  const hit = await cached(url);
  if (hit) return hit.json();
  throw new Error("Offline and not cached yet");
}

// ── Runtime ──────────────────────────────────────────────────────────────
let runtime;
function initRuntime() {
  if (runtime) return runtime;
  runtime = (async () => {
    const base = abs("./runtime/");
    const m = await getJson(base + "manifest.json");
    const w = m.runtime.wasm;
    const entry = { ...w, parts: w.parts.map((p) => base + p) };
    const bin = await getFile(entry, { add() {} });
    const iso = self.crossOriginIsolated === true;
    // Leave headroom for the real-time audio thread and UI: about half the cores, at most 4.
    const threads = iso ? (self.jayThreads || Math.max(1, Math.min(4, Math.floor((navigator.hardwareConcurrency || 4) / 2)))) : 1;
    ort.env.wasm.wasmBinary = bin;
    ort.env.wasm.numThreads = threads;
    ort.env.wasm.proxy = false;
    ort.env.logLevel = "error";
    const info = { threads, isolated: iso, version: m.runtime.version };
    post({ type: "runtime", info });
    return info;
  })().catch((e) => { runtime = null; throw e; });
  return runtime;
}
// No busy-spinning: idle ORT threads must not starve the real-time audio thread or the UI.
const session = (buf) => ort.InferenceSession.create(new Uint8Array(buf), {
  executionProviders: ["wasm"], graphOptimizationLevel: "all",
  extra: { session: { "intra_op.allow_spinning": self.jaySpin ? "1" : "0", "inter_op.allow_spinning": "0" } },
});

// ── Models ───────────────────────────────────────────────────────────────
let model = null;      // {id, kind, ...sessions}
let loading = null;    // {id, promise}

function vocabFrom(json) {
  if (Array.isArray(json)) return json;
  const t = {};
  for (const [k, i] of Object.entries(json.model?.vocab || json)) t[i] = k;
  for (const a of json.added_tokens || []) t[a.id] = a.content;
  const n = Math.max(...Object.keys(t).map(Number)) + 1;
  return Array.from({ length: n }, (_, i) => t[i] ?? "");
}
function tokensTxt(txt) {
  const out = [];
  for (const line of txt.split(/\r?\n/)) {
    const i = line.lastIndexOf(" ");
    if (i < 0) continue;
    out[+line.slice(i + 1)] = line.slice(0, i);
  }
  return out;
}

/** Resolve a spec (from the main thread) into concrete file entries. */
async function resolveFiles(spec) {
  if (spec.manifest) {
    const base = abs(spec.manifest);
    const m = await getJson(base);
    const dir = base.replace(/[^/]+$/, "");
    const shard = (f) => ({ ...f, parts: f.parts.map((p) => dir + p) });
    return {
      encoder: shard(m.files.encoder), decoder: shard(m.files.decoder),
      vocab: { url: dir + m.vocab.file, sha256: m.vocab.sha256, size: m.vocab.size },
    };
  }
  const files = {};
  for (const [k, v] of Object.entries(spec.files)) files[k] = { ...v, url: abs(v.url) };
  return files;
}

async function loadModel(spec) {
  if (model?.id === spec.id && model.rev === spec.rev) return model;
  if (loading?.id === spec.id) return loading.promise;
  const promise = (async () => {
    post({ type: "model", id: spec.id, state: "loading", stage: "runtime" });
    await initRuntime();
    if (model) { try { for (const s of model.sessions) await s.release(); } catch {} model = null; }
    const files = await resolveFiles(spec);
    const total = Object.values(files).reduce((s, f) => s + (f.gzip || f.size || 0), 0);
    let got = 0, cachedBytes = 0, last = 0;
    const progress = {
      add(n, fromCache) {
        got += n; if (fromCache) cachedBytes += n;
        const now = performance.now();
        if (now - last > 120) { last = now; post({ type: "model", id: spec.id, state: "loading", stage: cachedBytes >= got ? "reading" : "download", got, total }); }
      },
    };
    const bytes = {};
    for (const [k, f] of Object.entries(files)) {
      try { bytes[k] = await getFile(f, progress); }
      catch (e) { if (!f.optional) throw e; }
    }
    post({ type: "model", id: spec.id, state: "loading", stage: "compile", got: total, total });
    let m;
    if (spec.kind === "moonshine") {
      const enc = await session(bytes.encoder); bytes.encoder = null;
      const dec = await session(bytes.decoder); bytes.decoder = null;
      const vocabText = new TextDecoder().decode(bytes.vocab);
      const vocab = vocabFrom(JSON.parse(vocabText));
      let cfg = spec.config || {};
      if (bytes.config) cfg = JSON.parse(new TextDecoder().decode(bytes.config));
      const layers = cfg.decoder_num_hidden_layers || dec.inputNames.filter((n) => /past_key_values\.\d+\.decoder\.key/.test(n)).length;
      const heads = cfg.decoder_num_key_value_heads || cfg.decoder_num_attention_heads || cfg.heads || 8;
      const dim = cfg.headDim || Math.round((cfg.hidden_size || 288) / (cfg.decoder_num_attention_heads || heads));
      m = { id: spec.id, rev: spec.rev, kind: "moonshine", enc, dec, vocab, layers, heads, dim, sessions: [enc, dec],
        bos: cfg.decoder_start_token_id ?? 1, eos: cfg.eos_token_id ?? 2 };
    } else {
      const s = await session(bytes.model); bytes.model = null;
      const tokens = tokensTxt(new TextDecoder().decode(bytes.tokens));
      m = { id: spec.id, rev: spec.rev, kind: "ctc", s, tokens, sessions: [s], inputs: s.inputNames, format: spec.format || "medasr" };
    }
    model = m;
    post({ type: "model", id: spec.id, state: "ready", runtime: await runtime });
    return m;
  })();
  loading = { id: spec.id, promise };
  try { return await promise; }
  catch (e) { post({ type: "model", id: spec.id, state: "error", error: String(e.message || e) }); throw e; }
  finally { if (loading?.promise === promise) loading = null; }
}

// ── Decoders ─────────────────────────────────────────────────────────────
function argmax(a, o, n) {
  let bi = 0, bv = -Infinity;
  for (let i = 0; i < n; i++) { const v = a[o + i]; if (v > bv) { bv = v; bi = i; } }
  return bi;
}
function repeating(t) {
  for (let p = 1; p <= 8; p++) {
    const reps = p === 1 ? 6 : 4;
    if (t.length < p * reps) continue;
    let ok = true;
    for (let r = 1; r < reps && ok; r++) for (let k = 0; k < p; k++) if (t[t.length - 1 - k] !== t[t.length - 1 - k - r * p]) { ok = false; break; }
    if (ok) return p * (reps - 1);
  }
  return 0;
}
function detok(vocab, ids) {
  const bytes = [];
  let out = "";
  const flush = () => { if (bytes.length) { out += new TextDecoder().decode(new Uint8Array(bytes)); bytes.length = 0; } };
  for (const id of ids) {
    const t = vocab[id];
    if (id < 3 || !t || /^<<.*>>$/.test(t)) continue;
    const b = /^<0x([0-9A-F]{2})>$/.exec(t);
    if (b) { bytes.push(parseInt(b[1], 16)); continue; }
    flush();
    out += t.replace(/▁/g, " ");
  }
  flush();
  return out.trim();
}

async function moonshine(m, audio) {
  if (audio.length < SR / 2) { const p = new Float32Array(SR / 2); p.set(audio, (p.length - audio.length) >> 1); audio = p; }
  const tE = performance.now();
  const { last_hidden_state: h } = await m.enc.run({ input_values: new ort.Tensor("float32", audio, [1, audio.length]) });
  prof.enc += performance.now() - tE; prof.n++;
  const tD = performance.now();
  const past = {};
  const empty = new Float32Array(0);
  for (let i = 0; i < m.layers; i++) for (const a of ["decoder", "encoder"]) for (const b of ["key", "value"])
    past[`past_key_values.${i}.${a}.${b}`] = new ort.Tensor("float32", empty, [1, m.heads, 0, m.dim]);
  const toks = [];
  let ids = [m.bos];
  const max = Math.ceil((audio.length / SR) * 6.5) + 6;
  for (let step = 0; step < max; step++) {
    const feeds = {
      input_ids: new ort.Tensor("int64", BigInt64Array.from(ids.map(BigInt)), [1, ids.length]),
      encoder_hidden_states: h,
      use_cache_branch: new ort.Tensor("bool", new Uint8Array([step > 0 ? 1 : 0]), [1]),
      ...past,
    };
    const out = await m.dec.run(feeds);
    const lg = out.logits, V = lg.dims[2];
    const next = argmax(lg.data, (lg.dims[1] - 1) * V, V);
    if (next === m.eos) break;
    toks.push(next);
    const cut = repeating(toks);
    if (cut) { toks.length -= cut; break; }
    ids = [next];
    for (const [k, v] of Object.entries(out)) {
      if (!k.startsWith("present")) continue;
      if (step > 0 && k.includes(".encoder.")) continue;
      past[k.replace("present", "past_key_values")] = v;
    }
  }
  prof.dec += performance.now() - tD; prof.steps += toks.length + 1;
  return detok(m.vocab, toks);
}
const prof = { enc: 0, dec: 0, n: 0, steps: 0 };

const PAD = 0.25;
async function ctc(m, audio) {
  const pad = Math.round(SR * PAD);
  const a = new Float32Array(audio.length + 2 * pad);
  a.set(audio, pad);
  const { data, frames } = lasrFeatures(a);
  if (frames < 8) return [];
  const feeds = { input_features: new ort.Tensor("float32", data, [1, frames, 128]) };
  if (m.inputs.includes("attention_mask")) feeds.attention_mask = new ort.Tensor("int32", new Int32Array(frames).fill(1), [1, frames]);
  let out;
  try { out = await m.s.run(feeds); }
  catch (e) {
    if (!feeds.attention_mask) throw e;
    feeds.attention_mask = new ort.Tensor("int64", new BigInt64Array(frames).fill(1n), [1, frames]);
    out = await m.s.run(feeds);
  }
  const lg = out[m.s.outputNames[0]];
  const T = lg.dims[1], V = lg.dims[2];
  const sec = a.length / SR / T;
  const words = [];
  let prev = -1;
  for (let t = 0; t < T; t++) {
    const id = argmax(lg.data, t * V, V);
    if (id !== prev && id !== 0) {
      const tok = m.tokens[id];
      if (tok && !/^<.*>$/.test(tok)) {
        if (tok.startsWith("▁") || !words.length) words.push({ text: tok.replace(/^▁/, ""), t0: t, t1: t });
        else { const w = words[words.length - 1]; w.text += tok; w.t1 = t; }
      }
    }
    prev = id;
  }
  // A window that starts just after a cut can begin with the tail of the previous token ("]", "}.", "paragraph}").
  while (words.length && /^([\]}).,;:]+|(paragraph|line)\}[.,]?)$/i.test(words[0].text)) words.shift();
  return words.map((w) => ({ text: w.text, start: Math.max(0, w.t0 * sec - PAD), end: Math.max(0, (w.t1 + 1) * sec - PAD) }));
}

/** CTC words → sentence-ish segments with timestamps and paragraph breaks. */
function ctcSegments(words, offset, fmt, cont = false) {
  const segs = [];
  let cur = [], brk = 0;
  const flush = (nextBrk) => {
    if (cur.length) {
      const raw = cur.map((w) => w.text).join(" ");
      let text = fmt(raw);
      // A chunk that continues an unfinished sentence keeps its original lower-case start.
      if (cont && !segs.length && /^\p{Ll}/u.test(raw)) text = text.charAt(0).toLowerCase() + text.slice(1);
      const seg = { start: offset + cur[0].start, end: offset + cur[cur.length - 1].end, text, brk };
      if (/^\[/.test(raw) && /^[^:]{2,40}:/.test(text)) seg.sec = 1;
      if (text) segs.push(seg);
      brk = nextBrk;
    } else brk = Math.max(brk, nextBrk);
    cur = [];
  };
  for (const w of words) {
    if (/^\[/.test(w.text)) flush(1);
    cur.push(w);
    const t = w.text.toLowerCase();
    if (/paragraph\}$/.test(t)) { cur.pop(); if (cur.at(-1)?.text === "{new") cur.pop(); flush(2); continue; }
    if (/line\}$/.test(t)) { cur.pop(); if (cur.at(-1)?.text === "{new") cur.pop(); flush(1); continue; }
    const span = cur.at(-1).end - cur[0].start;
    if (/(\{period\}|\{question|[.?!])$/.test(t) || (span > 12 && /(,|\{comma\})$/.test(t)) || span > 20) flush(0);
  }
  flush(0);
  return segs;
}

/** Moonshine occasionally stops early on long phrases; re-decode as two halves if output looks clipped. */
async function moonshineSafe(m, audio, final) {
  const text = await moonshine(m, audio);
  const sec = audio.length / SR;
  const words = text.split(/\s+/).filter(Boolean).length;
  if (!final || sec < 5 || words / sec > 1.6) return text;
  const cut = quietest(audio);
  const [a, b] = await Promise.all([moonshine(m, audio.subarray(0, cut)), moonshine(m, audio.subarray(cut))]);
  const joined = `${a} ${b}`.trim();
  return joined.split(/\s+/).length > words * 1.25 ? joined : text;
}

const continues = (text) => !!text && !/[.?!:]$/.test(text.trim());
const ctcFmt = (m, opts) => (m.format === "medasr" ? formatMedasr : opts.commands ? spokenCommands : plain);
async function transcribe(m, audio, offset, opts, final = true, cont = false, minStart = null) {
  if (m.kind === "moonshine") {
    let text = await moonshineSafe(m, audio, final);
    if (/^[\s.,!?-]*$/.test(text)) return [];
    text = opts.commands ? spokenCommands(text) : plain(text);
    // A phrase that continues an unfinished sentence shouldn't start with a capital ("…includes Colsa…").
    if (cont && /^\p{Lu}\p{Ll}/u.test(text) && !/^I\b/.test(text)) text = text.charAt(0).toLowerCase() + text.slice(1);
    // Sentence-level segments with time spread by character share, so playback can follow along.
    const dur = audio.length / SR, out = [];
    const parts = [];
    text.split(/\n+/).forEach((para, pi) => {
      para.split(/(?<=[.?!]["')\]]*)\s+(?=\S)/).forEach((sent, si) => { sent = sent.trim(); if (sent) parts.push({ sent, brk: pi && !si ? 2 : 0 }); });
    });
    const total = parts.reduce((n, p) => n + p.sent.length, 0) || 1;
    let at = 0;
    for (const p of parts) {
      const len = (p.sent.length / total) * dur;
      out.push({ start: offset + at, end: offset + at + len, text: p.sent, brk: p.brk });
      at += len;
    }
    return out;
  }
  let words = await ctc(m, audio);
  // Leading audio was context only (already committed): keep words that start after the cut.
  if (minStart != null) words = words.filter((w) => offset + w.start >= minStart - 0.05);
  return ctcSegments(words, offset, ctcFmt(m, opts), cont);
}

// ── Serial inference queue (finals before partials) ──────────────────────
const queue = [];
let busy = false, seq = 0;
function enqueue(prio, fn, tag) {
  return new Promise((resolve, reject) => { queue.push({ prio, n: seq++, fn, resolve, reject, tag }); pump(); });
}
async function pump() {
  if (busy || !queue.length) return;
  busy = true;
  queue.sort((a, b) => a.prio - b.prio || a.n - b.n);
  const job = queue.shift();
  try { job.resolve(await job.fn()); } catch (e) { job.reject(e); }
  busy = false;
  pump();
}
const dropPartials = (live) => { for (let i = queue.length - 1; i >= 0; i--) if (queue[i].tag === live) { queue[i].resolve(null); queue.splice(i, 1); } };

// ── Live streaming (energy VAD + rolling partials) ───────────────────────
class Live {
  constructor(id, spec, opts) {
    Object.assign(this, { id, spec, opts });
    this.buf = new Float32Array(SR * 60); this.len = 0; this.base = 0; // absolute sample index of buf[0]
    this.pos = 0;          // absolute samples processed by VAD
    this.db = [];          // recent frame energies [{abs, db}]
    this.floor = -70; this.minWin = [];
    this.speech = false; this.on = 0; this.sil = 0; this.segStart = 0;
    this.lastPartial = 0; this.partialQueued = false; this.finals = []; this.segIndex = 0;
    this.minSil = Math.round(((opts.pause ?? 0.6) * SR) / FRAME);
    this.maxSeg = (opts.maxSeg ?? 18) * SR;
    enqueue(0, () => loadModel(spec)).catch(() => {});
  }
  slice(a, b) { return this.buf.slice(a - this.base, b - this.base); }
  push(pcm) {
    if (this.len + pcm.length > this.buf.length) {
      const anchor = Math.min(this.speech ? this.segStart : this.pos, this.pos) - SR * 2;
      const drop = Math.max(0, Math.min(anchor - this.base, this.len));
      const keep = this.len - drop;
      if (keep + pcm.length > this.buf.length) {
        const nb = new Float32Array(Math.ceil((keep + pcm.length) * 1.5));
        nb.set(this.buf.subarray(drop, this.len)); this.buf = nb;
      } else this.buf.copyWithin(0, drop, this.len);
      this.base += drop; this.len = keep;
    }
    this.buf.set(pcm, this.len); this.len += pcm.length;
    while (this.pos + FRAME <= this.base + this.len) { this.frame(); this.pos += FRAME; }
  }
  frame() {
    const db = frameDb(this.buf, this.pos - this.base);
    // minimum-statistics noise floor over ~3 s
    this.minWin.push(db); if (this.minWin.length > 300) this.minWin.shift();
    if (this.minWin.length < 30) this.floor = Math.min(...this.minWin);
    else if (this.pos % (FRAME * 25) === 0) this.floor = this.floor * 0.7 + Math.min(...this.minWin) * 0.3;
    this.db.push(db); if (this.db.length > 600) this.db.shift();
    const loud = db > Math.max(this.floor + 10, -58);
    const quiet = db < Math.max(this.floor + 6, -62);
    if (!this.speech) {
      this.on = loud ? this.on + 1 : 0;
      if (this.on >= 3) { this.speech = true; this.sil = 0; this.segStart = Math.max(this.base, this.pos - SR * 0.35); this.lastPartial = this.pos; }
    } else {
      this.sil = quiet ? this.sil + 1 : 0;
      const end = this.pos + FRAME;
      if (this.sil >= this.minSil) { this.finalize(end - this.sil * FRAME + SR * 0.2); this.speech = false; this.on = 0; }
      else if (end - this.segStart > this.maxSeg) {
        // cut at the quietest 100 ms within the last 4 s
        const look = Math.min(400, this.db.length - 10);
        let best = this.db.length - 1, bv = Infinity;
        for (let i = this.db.length - look; i < this.db.length - 5; i++) {
          let s = 0; for (let k = -5; k <= 5; k++) s += this.db[Math.max(0, Math.min(this.db.length - 1, i + k))];
          if (s < bv) { bv = s; best = i; }
        }
        const cut = end - (this.db.length - best) * FRAME;
        this.finalize(cut); this.segStart = cut; this.lastPartial = cut;
      } else if (end - this.lastPartial > SR * (this.opts.partialEvery ?? 0.9) && !this.partialQueued && end - this.segStart > SR * 0.6) {
        this.lastPartial = end; this.partialQueued = true;
        const seg = this.segIndex;
        enqueue(2, async () => {
          this.partialQueued = false;
          if (seg !== this.segIndex) return null;
          const m = await loadModel(this.spec);
          const from = this.segStart, to = Math.min(this.pos, this.base + this.len);
          if (m.kind === "ctc") return this.ctcPartial(m, seg, from, to);
          const segs = await transcribe(m, this.slice(from, to), from / SR, this.opts, false);
          if (seg === this.segIndex) post({ type: "partial", job: this.id, segments: segs });
        }, this).catch(() => { this.partialQueued = false; });
      }
    }
  }
  /**
   * CTC streaming: words two consecutive decodes agree on, and that ended >1.2 s ago, are
   * committed as final text (preferably at a sentence end) and decoding moves past them, so
   * long uninterrupted dictation grows as ink instead of one ever-rewritten partial line.
   */
  async ctcPartial(m, seg, from, to) {
    const ctxFrom = this.ctxStart ?? from;
    const words = (await ctc(m, this.slice(ctxFrom, to)))
      .map((w) => ({ ...w, start: w.start + ctxFrom / SR, end: w.end + ctxFrom / SR }))
      .filter((w) => w.start >= from / SR - 0.05);
    if (seg !== this.segIndex || from !== this.segStart) return;
    const fmt = ctcFmt(m, this.opts), end = to / SR;
    const norm = (w) => w.text.toLowerCase().replace(/[^\p{L}\p{N}{}[\]]/gu, "");
    const prev = this.prevWords || [];
    let k = 0;
    while (k < words.length && k < prev.length && norm(words[k]) === norm(prev[k])) k++;
    let c = 0;
    while (c < k && words[c].end < end - 1.2) c++;
    // Never cut inside a {command} or [SECTION] token: check only the words around the boundary.
    const inside = (n) => { const t = words.slice(Math.max(0, n - 3), n).map((w) => w.text).join(" "); return t.lastIndexOf("{") > t.lastIndexOf("}") || t.lastIndexOf("[") > t.lastIndexOf("]") || /^[\]}]/.test(words[n]?.text || ""); };
    // Prefer the latest sentence end whose gap is genuinely silent; a long run with no sentence end
    // may commit mid-sentence, but only at a silent gap. Otherwise wait: the tail stays live.
    const SENT = /([.?!]|(period|mark|paragraph|line)\})$/i;
    const silent = this.floor + 9;
    let cut = null;
    for (let i = c; i >= 4 && !cut; i--) {
      if (!SENT.test(words[i - 1].text) || inside(i)) continue;
      const q = this.quietCut(words[i - 1].end, words[i] ? words[i].start : end);
      if (q.db < silent) { cut = q; c = i; }
    }
    if (!cut && c >= 12 && words[c - 1].end - words[0].start > 8 && !inside(c)) {
      const q = this.quietCut(words[c - 1].end, words[c] ? words[c].start : end);
      if (q.db < silent - 3) cut = q;
    }
    if (!cut) c = 0;
    let committed = [];
    if (c > 0) {
      committed = ctcSegments(words.slice(0, c), 0, fmt, continues(this.lastText));
      if (committed.length) this.lastText = committed.at(-1).text;
      this.segStart = Math.max(this.segStart, cut.at);
      this.ctxStart = Math.max(this.base, this.segStart - Math.round(1.5 * SR));
    }
    this.prevWords = words.slice(c);
    const rest = ctcSegments(words.slice(c), 0, fmt, continues(this.lastText));
    if (committed.length) post({ type: "segments", job: this.id, segments: committed, partial: rest, commit: true });
    else post({ type: "partial", job: this.id, segments: rest });
  }
  /** Quietest 10 ms frame (absolute sample) between two times, so cuts land in the gap between words. */
  quietCut(a, b) {
    let best = Math.round(a * SR), bv = Infinity;
    for (let x = Math.max(this.base, Math.round(a * SR)); x + FRAME <= Math.min(Math.round(b * SR), this.base + this.len); x += FRAME / 2) {
      const v = frameDb(this.buf, x - this.base);
      if (v < bv) { bv = v; best = x + FRAME / 2; }
    }
    return { at: best, db: bv };
  }
  finalize(endAbs) {
    const from = this.segStart, to = Math.min(endAbs, this.base + this.len);
    const ctxFrom = this.ctxStart != null ? Math.max(this.base, this.ctxStart) : from;
    this.prevWords = []; this.ctxStart = null;
    this.segIndex++;
    dropPartials(this); this.partialQueued = false;
    if (to - from < SR * 0.25) { post({ type: "partial", job: this.id, text: "" }); return; }
    const audio = this.slice(ctxFrom, to);
    const p = enqueue(1, async () => {
      const m = await loadModel(this.spec);
      const t0 = performance.now();
      const segs = await transcribe(m, audio, ctxFrom / SR, this.opts, true, continues(this.lastText), ctxFrom < from ? from / SR : null);
      if (segs.length) this.lastText = segs.at(-1).text;
      post({ type: "segments", job: this.id, segments: segs, ms: performance.now() - t0, audioSec: audio.length / SR });
    }).catch((e) => post({ type: "job-error", job: this.id, error: String(e.message || e) }));
    this.finals.push(p);
  }
  flush() {
    if (this.speech) { this.finalize(this.base + this.len); this.speech = false; this.on = 0; this.sil = 0; }
    post({ type: "partial", job: this.id, text: "" });
  }
  async stop() {
    this.flush();
    await Promise.all(this.finals);
    post({ type: "live-done", job: this.id });
  }
}

const lives = new Map();
const cancelled = new Set();

async function fileJob({ job, spec, pcm, opts }) {
  const audio = new Float32Array(pcm);
  const sono = sonogram(audio);
  post({ type: "sonogram", job, sono }, [sono.data.buffer]);
  const chunks = planChunks(audio, spec.kind === "moonshine" ? 15 : 28);
  const total = audio.length / SR;
  post({ type: "progress", job, done: 0, total });
  if (!chunks.length) { post({ type: "file-done", job, empty: true }); return; }
  const m = await enqueue(0, () => loadModel(spec));
  let compute = 0, lastText = "";
  for (const c of chunks) {
    if (cancelled.has(job)) { cancelled.delete(job); post({ type: "file-done", job, cancelled: true }); return; }
    const segs = await enqueue(1, async () => {
      const t0 = performance.now();
      const s = await transcribe(m, audio.subarray(c.start, c.end), c.start / SR, opts, true, continues(lastText));
      if (s.length) lastText = s.at(-1).text;
      compute += performance.now() - t0;
      return s;
    });
    post({ type: "segments", job, segments: segs });
    post({ type: "progress", job, done: c.end / SR, total });
  }
  post({ type: "file-done", job, ms: compute, audioSec: total });
}

// ── Cache inspection ─────────────────────────────────────────────────────
async function cacheStatus(specs) {
  const out = {};
  for (const spec of specs) {
    try {
      const files = await resolveFiles(spec);
      let have = 0, n = 0, bytes = 0;
      for (const f of Object.values(files)) {
        n++;
        const key = f.parts ? `${f.parts[0]}?v=${(f.sha256 || "").slice(0, 12)}` : f.url;
        const hit = await cached(key);
        if (hit) { have++; bytes += f.size || 0; }
      }
      out[spec.id] = { state: have === n ? "cached" : have ? "partial" : "none", bytes };
    } catch { out[spec.id] = { state: "none", bytes: 0 }; }
  }
  return out;
}
async function deleteModel(spec) {
  const c = await caches.open(MODEL_CACHE);
  const files = await resolveFiles(spec).catch(() => ({}));
  for (const f of Object.values(files)) await c.delete(f.parts ? `${f.parts[0]}?v=${(f.sha256 || "").slice(0, 12)}` : f.url);
  if (model?.id === spec.id) { try { for (const s of model.sessions) await s.release(); } catch {} model = null; }
}

// ── Messages ─────────────────────────────────────────────────────────────
onmessage = async ({ data: d }) => {
  if (d.threads) self.jayThreads = d.threads;
  if (d.spin) self.jaySpin = true;
  try {
    switch (d.type) {
      case "load": await enqueue(0, () => loadModel(d.spec)); break;
      case "live-start": lives.set(d.job, new Live(d.job, d.spec, d.opts || {})); break;
      case "live-audio": lives.get(d.job)?.push(new Float32Array(d.pcm)); break;
      case "live-pause": lives.get(d.job)?.flush(); break;
      case "live-stop": { const l = lives.get(d.job); lives.delete(d.job); if (l) await l.stop(); else post({ type: "live-done", job: d.job }); break; }
      case "file": await fileJob(d); break;
      case "cancel": cancelled.add(d.job); break;
      case "sonogram": { const s = sonogram(d.i16 ? new Int16Array(d.pcm) : new Float32Array(d.pcm)); post({ type: "sonogram", job: d.job, sono: s }, [s.data.buffer]); break; }
      case "cache-status": post({ type: "cache-status", status: await cacheStatus(d.specs) }); break;
      case "delete-model": await deleteModel(d.spec); post({ type: "cache-status", status: await cacheStatus(d.specs || []) }); break;
      case "runtime": post({ type: "runtime", info: await initRuntime() }); break;
      case "debug-hang": enqueue(0, () => new Promise(() => {})); break;
      case "debug": post({ type: "debug", prof, busy, queue: queue.map((q) => [q.prio, q.tag ? "partial" : "job"]), model: model?.id || null, loading: loading?.id || null, lives: [...lives.keys()] }); break;
    }
  } catch (e) {
    const msg = String(e?.message || e);
    post({ type: "job-error", job: d.job, error: msg });
    if (/abort|out of memory|RuntimeError|unreachable|memory access/i.test(msg)) post({ type: "fatal", error: msg });
  }
};
const BUILD = "jay-build:0237969105";
post({ type: "hello", build: BUILD });
