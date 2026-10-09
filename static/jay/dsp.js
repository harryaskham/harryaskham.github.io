// Jay · signal processing shared by the ASR worker: FFT, log-mel features,
// sonograms, energy VAD/segmentation and transcript formatting.

export const SR = 16000;

// ── Radix-2 real FFT (power spectrum) ────────────────────────────────────
const fftCache = new Map();
function fftTables(n) {
  let t = fftCache.get(n);
  if (t) return t;
  const rev = new Uint32Array(n);
  const bits = Math.log2(n);
  for (let i = 0; i < n; i++) {
    let r = 0;
    for (let b = 0; b < bits; b++) r |= ((i >> b) & 1) << (bits - 1 - b);
    rev[i] = r;
  }
  const cos = new Float64Array(n / 2), sin = new Float64Array(n / 2);
  for (let i = 0; i < n / 2; i++) {
    cos[i] = Math.cos((2 * Math.PI * i) / n);
    sin[i] = -Math.sin((2 * Math.PI * i) / n);
  }
  t = { rev, cos, sin, re: new Float64Array(n), im: new Float64Array(n) };
  fftCache.set(n, t);
  return t;
}

/** In: frame (length ≤ n, zero padded). Out: power spectrum length n/2+1. */
export function powerSpectrum(frame, n, out) {
  const { rev, cos, sin, re, im } = fftTables(n);
  for (let i = 0; i < n; i++) {
    const j = rev[i];
    re[j] = i < frame.length ? frame[i] : 0;
    im[j] = 0;
  }
  for (let size = 2; size <= n; size <<= 1) {
    const half = size >> 1, step = n / size;
    for (let start = 0; start < n; start += size) {
      for (let k = 0; k < half; k++) {
        const wr = cos[k * step], wi = sin[k * step];
        const a = start + k, b = a + half;
        const tr = re[b] * wr - im[b] * wi;
        const ti = re[b] * wi + im[b] * wr;
        re[b] = re[a] - tr; im[b] = im[a] - ti;
        re[a] += tr; im[a] += ti;
      }
    }
  }
  for (let k = 0; k <= n / 2; k++) out[k] = re[k] * re[k] + im[k] * im[k];
  return out;
}

// ── LASR / MedASR log-mel features (port of LasrFeatureExtractor) ──────────
const kaldiMel = (f) => 1127.0 * Math.log(1.0 + f / 700.0);
let melCache;
function lasrMel(nMels = 128, nFft = 512, lo = 125, hi = 7500) {
  if (melCache) return melCache;
  const bins = nFft / 2 + 1;
  const edges = [];
  const mlo = kaldiMel(lo), mhi = kaldiMel(hi);
  for (let i = 0; i < nMels + 2; i++) edges.push(mlo + ((mhi - mlo) * i) / (nMels + 1));
  // sparse filters: [{start, weights}] per mel band
  const filters = [];
  for (let m = 0; m < nMels; m++) {
    const l = edges[m], c = edges[m + 1], u = edges[m + 2];
    const w = [];
    let start = -1;
    for (let k = 1; k < bins; k++) {
      const bm = kaldiMel((k * (SR / 2)) / (bins - 1));
      const v = Math.max(0, Math.min((bm - l) / (c - l), (u - bm) / (u - c)));
      if (v > 0) { if (start < 0) start = k; w[k - start] = v; }
      else if (start >= 0) break;
    }
    filters.push({ start: Math.max(start, 0), w: Float64Array.from(w.map((x) => x || 0)) });
  }
  return (melCache = filters);
}

/** Float32 16 kHz audio → Float32Array(frames*128) log-mel, frames. */
export function lasrFeatures(audio) {
  const win = 400, hop = 160, nFft = 512, nMels = 128;
  const frames = audio.length < win ? 0 : 1 + Math.floor((audio.length - win) / hop);
  const out = new Float32Array(frames * nMels);
  const hann = new Float64Array(win);
  for (let i = 0; i < win; i++) hann[i] = 0.5 - 0.5 * Math.cos((2 * Math.PI * i) / (win - 1));
  const frame = new Float64Array(win), pow = new Float64Array(nFft / 2 + 1);
  const mel = lasrMel(nMels, nFft);
  for (let f = 0; f < frames; f++) {
    const o = f * hop;
    for (let i = 0; i < win; i++) frame[i] = audio[o + i] * hann[i];
    powerSpectrum(frame, nFft, pow);
    for (let m = 0; m < nMels; m++) {
      const { start, w } = mel[m];
      let s = 0;
      for (let j = 0; j < w.length; j++) s += pow[start + j] * w[j];
      out[f * nMels + m] = Math.log(Math.max(s, 1e-5));
    }
  }
  return { data: out, frames };
}

// ── Sonogram for the turn scrubber ───────────────────────────────────────
/** Field-guide style sonogram. Returns {w, h, data: Uint8Array(w*h)}, row 0 = highest band. */
export function sonogram(audio, maxCols = 480, rows = 40) {
  const n = 512, hop = 160;
  const frames = Math.max(1, Math.floor((audio.length - n) / hop) + 1);
  const cols = Math.max(1, Math.min(maxCols, Math.ceil(frames / 3)));
  const bands = [];
  const lo = kaldiMel(90), hi = kaldiMel(6000);
  for (let r = 0; r <= rows; r++) {
    const m = lo + ((hi - lo) * r) / rows;
    bands.push(Math.round(((700 * (Math.exp(m / 1127) - 1)) / (SR / 2)) * (n / 2)));
  }
  const acc = new Float64Array(cols * rows), cnt = new Uint32Array(cols);
  const frame = new Float64Array(n), pow = new Float64Array(n / 2 + 1);
  const hann = new Float64Array(n);
  for (let i = 0; i < n; i++) hann[i] = 0.5 - 0.5 * Math.cos((2 * Math.PI * i) / (n - 1));
  const stride = Math.max(1, Math.floor(frames / (cols * 6)));
  for (let f = 0; f < frames; f += stride) {
    const o = f * hop;
    let prev = audio[o - 1] || 0;
    for (let i = 0; i < n; i++) { const v = audio[o + i] || 0; frame[i] = (v - 0.9 * prev) * hann[i]; prev = v; }
    powerSpectrum(frame, n, pow);
    const c = Math.min(cols - 1, Math.floor((f / frames) * cols));
    cnt[c]++;
    for (let r = 0; r < rows; r++) {
      let s = 0;
      const a = bands[r], b = Math.max(bands[r + 1], a + 1);
      for (let k = a; k < b; k++) s += pow[k];
      acc[(rows - 1 - r) * cols + c] += s / (b - a);
    }
  }
  const db = new Float32Array(cols * rows);
  for (let i = 0; i < db.length; i++) db[i] = 10 * Math.log10(acc[i] / Math.max(1, cnt[i % cols]) + 1e-12);
  // separable smoothing: 5-tap in time, 3-tap in frequency
  const tmp = new Float32Array(db.length), sm = new Float32Array(db.length);
  const K = [1, 3, 4, 3, 1];
  for (let r = 0; r < rows; r++) for (let c = 0; c < cols; c++) {
    let s = 0, w = 0;
    for (let k = -2; k <= 2; k++) { const cc = c + k; if (cc < 0 || cc >= cols) continue; s += db[r * cols + cc] * K[k + 2]; w += K[k + 2]; }
    tmp[r * cols + c] = s / w;
  }
  for (let r = 0; r < rows; r++) for (let c = 0; c < cols; c++) {
    const up = tmp[Math.max(0, r - 1) * cols + c], dn = tmp[Math.min(rows - 1, r + 1) * cols + c];
    sm[r * cols + c] = 0.25 * up + 0.5 * tmp[r * cols + c] + 0.25 * dn;
  }
  // per-band floor so quiet upper formants still read, plus a global ceiling
  const sorted = Float32Array.from(sm).sort();
  const ceil = sorted[Math.floor(sorted.length * 0.995)];
  const rowFloor = new Float32Array(rows);
  for (let r = 0; r < rows; r++) {
    const row = Float32Array.from(sm.subarray(r * cols, r * cols + cols)).sort();
    rowFloor[r] = row[Math.floor(cols * 0.45)];
  }
  const data = new Uint8Array(cols * rows);
  for (let r = 0; r < rows; r++) {
    const fl = Math.max(rowFloor[r] + 3, ceil - 45);
    const span = Math.max(8, ceil - fl);
    for (let c = 0; c < cols; c++) {
      const i = r * cols + c;
      const v = Math.max(0, Math.min(1, (sm[i] - fl) / span));
      data[i] = Math.round(255 * Math.pow(v, 0.9));
    }
  }
  return { w: cols, h: rows, data };
}

// ── Energy features ──────────────────────────────────────────────────────
export const FRAME = 160; // 10 ms
export function frameDb(audio, offset = 0) {
  let s = 0;
  for (let i = 0; i < FRAME; i++) { const v = audio[offset + i] || 0; s += v * v; }
  return 10 * Math.log10(s / FRAME + 1e-10);
}

/**
 * Split a whole file into speech chunks ≤ maxSec, skipping long silences and
 * cutting at the quietest point when speech runs long. Returns [{start,end}] in samples.
 */
export function planChunks(audio, maxSec = 20) {
  const n = Math.floor(audio.length / FRAME);
  if (!n) return [];
  const e = new Float32Array(n);
  for (let i = 0; i < n; i++) e[i] = frameDb(audio, i * FRAME);
  const sorted = Float32Array.from(e).sort();
  const floor = sorted[Math.floor(n * 0.1)];
  const peak = sorted[Math.floor(n * 0.98)];
  const thr = Math.max(floor + 8, Math.min(peak - 25, floor + 18), -60);
  // speech mask with 300 ms hangover, smoothed
  const speech = new Uint8Array(n);
  let hang = 0;
  for (let i = 0; i < n; i++) {
    if (e[i] > thr) hang = 30;
    speech[i] = hang > 0 ? 1 : 0;
    hang--;
  }
  const regions = [];
  for (let i = 0; i < n; ) {
    if (!speech[i]) { i++; continue; }
    let j = i;
    while (j < n && speech[j]) j++;
    regions.push([Math.max(0, i - 20), Math.min(n, j + 10)]);
    i = j;
  }
  if (!regions.length) return [];
  const maxF = Math.floor(maxSec * 100);
  const quietest = (a, b) => {
    // find min of 200 ms moving average in [a,b)
    let best = b, bestV = Infinity;
    for (let i = a; i < b; i++) {
      let s = 0;
      for (let k = -10; k <= 10; k++) s += e[Math.min(n - 1, Math.max(0, i + k))];
      if (s < bestV) { bestV = s; best = i; }
    }
    return best;
  };
  const chunks = [];
  let cur = null;
  const push = (a, b) => {
    while (b - a > maxF) {
      const cut = quietest(a + Math.floor(maxF * 0.5), a + maxF);
      chunks.push([a, cut]);
      a = cut;
    }
    if (b - a > 15) chunks.push([a, b]);
  };
  for (const [a, b] of regions) {
    if (cur && b - cur[0] <= maxF && a - cur[1] < 120) cur[1] = b;
    else { if (cur) push(cur[0], cur[1]); cur = [a, b]; }
  }
  if (cur) push(cur[0], cur[1]);
  return chunks.map(([a, b]) => ({ start: a * FRAME, end: Math.min(audio.length, b * FRAME) }));
}

// ── Text post-processing ─────────────────────────────────────────────────
const CMD = {
  period: ".", "full stop": ".", comma: ",", colon: ":", semicolon: ";",
  "question mark": "?", "exclamation mark": "!", "exclamation point": "!",
  hyphen: "-", dash: " — ", slash: "/", "open paren": " (", "close paren": ")",
  "open parenthesis": " (", "close parenthesis": ")", "new line": "\n", "next line": "\n",
  "new paragraph": "\n\n", "next paragraph": "\n\n",
};

function tidy(s) {
  return s
    .replace(/[ \t]+([.,:;?!)])/g, "$1")
    .replace(/([.,:;?!])(?=[A-Za-z])/g, "$1 ")
    .replace(/(\d)\. (\d)/g, "$1.$2")
    .replace(/([.,:;])\1+/g, "$1")
    .replace(/,\s*([.:;?!])/g, "$1")
    .replace(/[ \t]*\n[ \t]*/g, "\n")
    .replace(/\n{3,}/g, "\n\n")
    .replace(/[ \t]{2,}/g, " ")
    .replace(/(^|[.?!]\s+|\n)([a-z])/g, (_, a, b) => a + b.toUpperCase())
    .trim();
}

/** MedASR emits {period} {new paragraph} [SECTION] tokens; render them. */
export function formatMedasr(s) {
  s = s.replace(/\{\s*([a-z ]+?)\s*\}/gi, (m, w) => {
    const k = w.toLowerCase();
    return k in CMD ? CMD[k] : ` ${w} `;
  });
  s = s.replace(/\[\s*([A-Z][A-Z /&-]*?)\s*\]\s*:?/g, (m, w) => {
    const t = w.toLowerCase().replace(/(^|\s)\w/g, (c) => c.toUpperCase());
    return `\n${t}: `;
  });
  return tidy(s.replace(/:\s*:/g, ":"));
}

/** Optional spoken punctuation for general models ("comma", "new paragraph"…). */
export function spokenCommands(s) {
  const keys = Object.keys(CMD).sort((a, b) => b.length - a.length).map((k) => k.replace(/ /g, "[\\s-]+"));
  const re = new RegExp(`[,.]?\\s*\\b(${keys.join("|")})\\b[,.]?`, "gi");
  return tidy(s.replace(re, (m, w) => {
    const v = CMD[w.toLowerCase().replace(/[\s-]+/g, " ")];
    if (v == null) return m;
    return v.includes("\n") && /^[.?!]/.test(m) ? m[0] + v : v;
  }));
}

export const plain = (s) => tidy(s);
