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

/** Any browser-decodable audio/video → mono Float32 @16 kHz. */
export async function decodeFile(blob) {
  const buf = await blob.arrayBuffer();
  const ctx = new OfflineAudioContext(1, SR, SR);
  const ab = await ctx.decodeAudioData(buf);
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
