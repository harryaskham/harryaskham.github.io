#!/usr/bin/env python3
"""Reproducibly vendor Jay's on-device ASR runtime and built-in model.

Downloads pinned upstream artefacts, verifies their SHA-256, then writes
deterministic gzip shards (< 10 MB each, the per-file publish limit) plus a
manifest the browser uses to reassemble and re-verify them.

    python3 _tools/jay-assets.py            # (re)build static/jay/{runtime,models}
    python3 _tools/jay-assets.py --check    # verify committed shards only

Sources (all MIT):
  * onnxruntime-web 1.30.0 (CPU/WASM build, SIMD + threads)
  * Moonshine Tiny, int8, onnx-community/moonshine-tiny-ONNX @ a6da1241
"""
import argparse
import gzip
import hashlib
import io
import json
import tarfile
import urllib.request
from pathlib import Path

REPO = Path(__file__).resolve().parents[1]
APP = REPO / "static" / "jay"
CACHE = Path.home() / ".cache" / "jay-assets"
PART = 9_000_000

ORT_TGZ = ("https://registry.npmjs.org/onnxruntime-web/-/onnxruntime-web-1.30.0.tgz", None)
ORT_FILES = {
    "package/dist/ort.wasm.bundle.min.mjs": "11e64bd8ffe11bd1a2a2f0d6275fdfbbba7262f0b76b99b53d228a8a22ef3d90",
    "package/dist/ort-wasm-simd-threaded.wasm": "3398c10d07d229bd91b364548e130e0e51a8e5704b88c7c083ebbeb78842dee2",
}
HF = "https://huggingface.co/onnx-community/moonshine-tiny-ONNX/resolve/a6da1241cd305dcd64eab1edbd615f2bb9aabb95/"
MOON = {
    "encoder": ("onnx/encoder_model_quantized.onnx", "c6fc4b7bc5af75c0591fd157a1f3829b533d18e9769a888fd95a62e470dd4f4a"),
    "decoder": ("onnx/decoder_model_merged_quantized.onnx", "eed87831c3a6103534aae7d47a5d485025c659a1323901513961c39fe8a1a367"),
    "tokenizer": ("tokenizer.json", "7b913404bdd039af4756783218af4440bc07fb7d6d8258d677e34f95b3ec416f"),
}


def sha(data):
    return hashlib.sha256(data).hexdigest()


def fetch(url, digest=None):
    CACHE.mkdir(parents=True, exist_ok=True)
    path = CACHE / hashlib.sha1(url.encode()).hexdigest()
    if not path.exists():
        print("download", url)
        with urllib.request.urlopen(url) as r:
            path.write_bytes(r.read())
    data = path.read_bytes()
    if digest:
        assert sha(data) == digest, f"checksum mismatch: {url}"
    return data


def shard(name, data, outdir):
    gz = gzip.compress(data, compresslevel=9, mtime=0)
    parts = []
    for i in range(0, len(gz), PART):
        part = f"{name}.gz.{len(parts)}.bin"
        (outdir / part).write_bytes(gz[i:i + PART])
        parts.append(part)
    return {"parts": parts, "size": len(data), "gzip": len(gz), "sha256": sha(data)}


def vocab(tokenizer):
    tok = json.loads(tokenizer)
    table = {i: t for t, i in tok["model"]["vocab"].items()}
    for added in tok["added_tokens"]:
        table[added["id"]] = added["content"]
    out = [table.get(i, "") for i in range(max(table) + 1)]
    return json.dumps(out, ensure_ascii=False, separators=(",", ":")).encode()


def build():
    runtime, model = APP / "runtime", APP / "models" / "moonshine-tiny"
    for d in (runtime, model):
        d.mkdir(parents=True, exist_ok=True)
        for old in d.glob("*.bin"):
            old.unlink()
    with tarfile.open(fileobj=io.BytesIO(fetch(ORT_TGZ[0]))) as tar:
        files = {n: tar.extractfile(n).read() for n in ORT_FILES}
    for n, d in ORT_FILES.items():
        assert sha(files[n]) == d, n
    glue = files["package/dist/ort.wasm.bundle.min.mjs"].decode()
    glue = glue.replace("//# sourceMappingURL=ort.wasm.bundle.min.mjs.map", "").rstrip() + "\n"
    (runtime / "ort.js").write_text(glue)
    manifest = {
        "runtime": {"version": "onnxruntime-web 1.30.0", "wasm": shard("ort-wasm-simd-threaded.wasm", files["package/dist/ort-wasm-simd-threaded.wasm"], runtime)},
    }
    (runtime / "manifest.json").write_text(json.dumps(manifest, indent=1) + "\n")
    m = {"id": "moonshine-tiny", "source": HF, "files": {}}
    for key, (path, digest) in MOON.items():
        data = fetch(HF + path, digest)
        if key == "tokenizer":
            v = vocab(data)
            (model / "vocab.json").write_bytes(v)
            m["vocab"] = {"file": "vocab.json", "size": len(v), "sha256": sha(v)}
        else:
            m["files"][key] = shard(key, data, model)
    (model / "manifest.json").write_text(json.dumps(m, indent=1) + "\n")
    check()


def check():
    total = 0
    for manifest in [APP / "runtime/manifest.json", APP / "models/moonshine-tiny/manifest.json"]:
        data = json.loads(manifest.read_text())
        entries = [data["runtime"]["wasm"]] if "runtime" in data else data["files"].values()
        for e in entries:
            blob = b"".join((manifest.parent / p).read_bytes() for p in e["parts"])
            total += len(blob)
            raw = gzip.decompress(blob)
            assert len(raw) == e["size"] and sha(raw) == e["sha256"], f"bad shards in {manifest.parent}"
    print(f"Jay assets verified: {total:,} bytes of gzip shards")


if __name__ == "__main__":
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--check", action="store_true")
    (check if ap.parse_args().check else build)()
