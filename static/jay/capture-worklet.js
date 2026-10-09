// Jay · microphone capture. Runs in the AudioWorklet thread: mixes to mono and
// resamples device audio (44.1/48/… kHz) to 16 kHz with a windowed-sinc filter.
class Capture extends AudioWorkletProcessor {
  constructor() {
    super();
    this.ratio = sampleRate / 16000;
    const cutoff = Math.min(1, 1 / this.ratio) * 0.92; // normalised to input Nyquist
    this.L = 16;                                          // taps either side (in input samples, scaled below)
    this.half = Math.ceil(this.L * Math.max(1, this.ratio));
    this.res = 64;                                        // table resolution per input sample
    const n = this.half * this.res;
    this.table = new Float32Array(n + 2);
    for (let i = 0; i <= n + 1; i++) {
      const x = i / this.res;
      const s = x === 0 ? 1 : Math.sin(Math.PI * cutoff * x) / (Math.PI * cutoff * x);
      const w = x >= this.half ? 0 : 0.42 + 0.5 * Math.cos((Math.PI * x) / this.half) + 0.08 * Math.cos((2 * Math.PI * x) / this.half);
      this.table[i] = s * w * cutoff;
    }
    this.hist = new Float32Array(16384);
    this.hlen = this.half;           // pre-roll of zeros so the first output has full support
    this.x = this.half;              // fractional input position of next output sample
    this.out = new Float32Array(1600);
    this.olen = 0;
    this.port.onmessage = (e) => { if (e.data === "flush") this.flush(); };
  }
  flush() {
    if (!this.olen) return;
    const b = this.out.slice(0, this.olen);
    this.port.postMessage(b, [b.buffer]);
    this.olen = 0;
  }
  process(inputs) {
    const ch = inputs[0];
    if (!ch || !ch.length || !ch[0]) return true;
    const n = ch[0].length;
    if (this.hlen + n > this.hist.length) {
      const nh = new Float32Array((this.hlen + n) * 2);
      nh.set(this.hist.subarray(0, this.hlen));
      this.hist = nh;
    }
    for (let i = 0; i < n; i++) {
      let v = 0;
      for (let c = 0; c < ch.length; c++) v += ch[c][i];
      this.hist[this.hlen + i] = v / ch.length;
    }
    this.hlen += n;
    const { hist, table, res, half, ratio } = this;
    while (this.x + half < this.hlen) {
      const xi = Math.floor(this.x), frac = this.x - xi;
      let acc = 0;
      for (let k = -half + 1; k <= half; k++) {
        const d = Math.abs(k - frac) * res;
        const di = d | 0;
        const w = table[di] + (table[di + 1] - table[di]) * (d - di);
        acc += hist[xi + k] * w;
      }
      this.out[this.olen++] = acc;
      if (this.olen === this.out.length) this.flush();
      this.x += ratio;
    }
    const drop = Math.floor(this.x) - half;
    if (drop > 0) {
      hist.copyWithin(0, drop, this.hlen);
      this.hlen -= drop;
      this.x -= drop;
    }
    return true;
  }
}
registerProcessor("jay-capture", Capture);
