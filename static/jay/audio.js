// Jay · main-thread audio: mic recorder, file decode, WAV packing.
export const SR = 16000;

export class Recorder {
  constructor() {
    // Created synchronously inside the click so Safari treats it as user-activated.
    const AC = window.AudioContext || window.webkitAudioContext;
    this.ctx = new AC({ latencyHint: "interactive" });
    this.ctx.resume?.();
  }
  async start({ deviceId, noise = true, onPcm }) {
    const audio = { channelCount: 1, echoCancellation: false, noiseSuppression: noise, autoGainControl: noise };
    if (deviceId) audio.deviceId = { exact: deviceId };
    try {
      this.stream = await navigator.mediaDevices.getUserMedia({ audio });
    } catch (e) {
      if (!deviceId || e.name !== "OverconstrainedError") throw e;
      delete audio.deviceId;
      this.stream = await navigator.mediaDevices.getUserMedia({ audio });
    }
    await this.ctx.audioWorklet.addModule(new URL("./capture-worklet.js", import.meta.url));
    const src = this.ctx.createMediaStreamSource(this.stream);
    this.node = new AudioWorkletNode(this.ctx, "jay-capture", { numberOfInputs: 1, numberOfOutputs: 1, channelCount: 1, channelCountMode: "explicit" });
    this.node.port.onmessage = (e) => {
      if (e.data?.stats) { this.stats = e.data.stats; this.onStats?.(e.data.stats); return; }
      if (!this.paused) onPcm(e.data);
    };
    this.analyser = this.ctx.createAnalyser();
    this.analyser.fftSize = 1024;
    this.analyser.smoothingTimeConstant = 0.2;
    const mute = this.ctx.createGain();
    mute.gain.value = 0;
    src.connect(this.node).connect(mute).connect(this.ctx.destination);
    src.connect(this.analyser);
    await this.ctx.resume();
    this.label = this.stream.getAudioTracks()[0]?.label || "";
    // Independent safety recording: if the real-time graph is starved (busy device),
    // MediaRecorder still captures everything and we can recover on stop.
    try {
      if (window.MediaRecorder && !localStorage.getItem("jay.nomr")) {
        this.mr = new MediaRecorder(this.stream, { audioBitsPerSecond: 64000 });
        this.mrParts = [];
        this.mr.ondataavailable = (e) => e.data.size && this.mrParts.push(e.data);
        this.mrDone = new Promise((r) => (this.mr.onstop = () => r(new Blob(this.mrParts, { type: this.mr.mimeType }))));
        this.mr.start(2000);
      }
    } catch { this.mr = null; }
  }
  pause() { this.paused = true; this.node?.port.postMessage("flush"); try { this.mr?.pause(); } catch {} }
  resume() { this.paused = false; try { this.mr?.resume(); } catch {} }
  /** Full-fidelity backup recording (Blob) or null. */
  async backup() {
    if (!this.mr) return null;
    try { if (this.mr.state !== "inactive") this.mr.stop(); return await this.mrDone; } catch { return null; }
  }
  async stop() {
    this.paused = false;
    this.node?.port.postMessage("flush");
    this.node?.port.postMessage("stats");
    await new Promise((r) => setTimeout(r, 120));
    this.backupBlob = await this.backup();
    this.stream?.getTracks().forEach((t) => t.stop());
    try { await this.ctx.close(); } catch {}
  }
}

/** Minimal RIFF/WAVE reader (PCM 8/16/24/32, float 32/64) → {rate, channels: Float32Array[]}. */
export function parseWav(buf) {
  const dv = new DataView(buf);
  if (buf.byteLength < 44 || dv.getUint32(0) !== 0x52494646 || dv.getUint32(8) !== 0x57415645) return null;
  let o = 12, fmt = null, data = null;
  while (o + 8 <= buf.byteLength) {
    const id = dv.getUint32(o), n = dv.getUint32(o + 4, true);
    if (id === 0x666d7420) {
      fmt = { tag: dv.getUint16(o + 8, true), ch: dv.getUint16(o + 10, true), rate: dv.getUint32(o + 12, true), bits: dv.getUint16(o + 22, true) };
      if (fmt.tag === 0xfffe && n >= 40) fmt.tag = dv.getUint16(o + 32, true); // WAVE_FORMAT_EXTENSIBLE sub-format
    }
    if (id === 0x64617461) { data = [o + 8, Math.min(n, buf.byteLength - o - 8)]; break; }
    o += 8 + n + (n & 1);
  }
  if (!fmt || !data) return null;
  if (fmt.tag !== 1 && fmt.tag !== 3) return null;
  const bps = fmt.bits / 8, frames = Math.floor(data[1] / (bps * fmt.ch));
  const out = Array.from({ length: fmt.ch }, () => new Float32Array(frames));
  let p = data[0];
  for (let i = 0; i < frames; i++) for (let c = 0; c < fmt.ch; c++, p += bps) {
    let v;
    if (fmt.tag === 3) v = bps === 8 ? dv.getFloat64(p, true) : dv.getFloat32(p, true);
    else if (bps === 2) v = dv.getInt16(p, true) / 32768;
    else if (bps === 1) v = (dv.getUint8(p) - 128) / 128;
    else if (bps === 3) v = ((dv.getUint8(p) | (dv.getUint8(p + 1) << 8) | (dv.getInt8(p + 2) << 16))) / 8388608;
    else v = dv.getInt32(p, true) / 2147483648;
    out[c][i] = v;
  }
  return { rate: fmt.rate, channels: out };
}

/** Windowed-sinc resampler (offline). */
export function resample(x, from, to = SR) {
  if (from === to) return x;
  const ratio = from / to, cutoff = Math.min(1, to / from) * 0.92, half = Math.ceil(12 * Math.max(1, ratio)), res = 64;
  const table = new Float32Array(half * res + 2);
  for (let i = 0; i < table.length; i++) {
    const t = i / res, s = t === 0 ? 1 : Math.sin(Math.PI * cutoff * t) / (Math.PI * cutoff * t);
    table[i] = t >= half ? 0 : s * (0.42 + 0.5 * Math.cos((Math.PI * t) / half) + 0.08 * Math.cos((2 * Math.PI * t) / half)) * cutoff;
  }
  const out = new Float32Array(Math.floor(x.length / ratio));
  for (let i = 0; i < out.length; i++) {
    const pos = i * ratio, xi = Math.floor(pos), frac = pos - xi;
    let acc = 0;
    for (let k = -half + 1; k <= half; k++) {
      const j = xi + k;
      if (j < 0 || j >= x.length) continue;
      const d = Math.abs(k - frac) * res, di = d | 0;
      acc += x[j] * (table[di] + (table[di + 1] - table[di]) * (d - di));
    }
    out[i] = acc;
  }
  return out;
}
const mono = (chs) => {
  if (chs.length === 1) return chs[0];
  const out = new Float32Array(chs[0].length);
  for (const c of chs) for (let i = 0; i < out.length; i++) out[i] += c[i] / chs.length;
  return out;
};
const withTimeout = (p, ms, msg) => Promise.race([p, new Promise((_, rej) => setTimeout(() => rej(new Error(msg)), ms))]);

/** Any browser-decodable audio/video → mono Float32 @16 kHz. */
export async function decodeFile(blob) {
  const buf = await blob.arrayBuffer();
  // Fast path for WAV (Jay's own recordings are already 16 kHz mono): no codec, no resampling.
  const wav = parseWav(buf);
  if (wav && wav.rate === SR) return mono(wav.channels);
  let ab;
  try {
    const ctx = new OfflineAudioContext(1, SR, SR);
    ab = await withTimeout(ctx.decodeAudioData(buf.slice(0)), Math.max(30000, blob.size / 2e4), "decode timed out");
  } catch (e) {
    if (wav) return resample(mono(wav.channels), wav.rate);
    throw e;
  }
  const n = ab.length;
  if (ab.numberOfChannels === 1) return ab.getChannelData(0).slice();
  const out = new Float32Array(n);
  for (let c = 0; c < ab.numberOfChannels; c++) {
    const d = ab.getChannelData(c);
    for (let i = 0; i < n; i++) out[i] += d[i];
  }
  for (let i = 0; i < n; i++) out[i] /= ab.numberOfChannels;
  return out;
}

export function f32ToI16(f) {
  const o = new Int16Array(f.length);
  for (let i = 0; i < f.length; i++) { const v = Math.max(-1, Math.min(1, f[i])); o[i] = v < 0 ? v * 0x8000 : v * 0x7fff; }
  return o;
}
export function i16ToF32(i) {
  const o = new Float32Array(i.length);
  for (let k = 0; k < i.length; k++) o[k] = i[k] / 0x8000;
  return o;
}
export function concatI16(chunks) {
  const out = new Int16Array(chunks.reduce((s, c) => s + c.length, 0));
  let o = 0;
  for (const c of chunks) { out.set(c, o); o += c.length; }
  return out;
}
export function wavBlob(i16, rate = SR) {
  const h = new DataView(new ArrayBuffer(44));
  const w = (o, s) => [...s].forEach((c, i) => h.setUint8(o + i, c.charCodeAt(0)));
  w(0, "RIFF"); h.setUint32(4, 36 + i16.byteLength, true); w(8, "WAVE");
  w(12, "fmt "); h.setUint32(16, 16, true); h.setUint16(20, 1, true); h.setUint16(22, 1, true);
  h.setUint32(24, rate, true); h.setUint32(28, rate * 2, true); h.setUint16(32, 2, true); h.setUint16(34, 16, true);
  w(36, "data"); h.setUint32(40, i16.byteLength, true);
  return new Blob([h, i16], { type: "audio/wav" });
}
