#!/usr/bin/env bash
# Downloads Intel's Sponza 2022 scene and add-on packages into assets/sponza/ and bakes the animated knight
# into a vertex cache (assets/sponza/knight/knight.cache). CC-BY 4.0; see assets/sponza/SOURCES.md.
#
#   scripts/fetch-sponza-assets.sh [PACKAGE...]     PACKAGE: main curtains ivy trees candles knight (default: all)
#
# Only the files the renderer needs are fetched: glTF + bin + textures for the scene parts, the USD and its
# textures for the knight. The 3ds Max, FBX, USDA, Alembic and Maya files (most of the 11.5 GB of zips) are
# skipped by reading each zip's central directory and fetching just the wanted members with HTTP range
# requests. Files already present with the right size are kept.
#
# Environment: SPONZA_DIR (default <repo>/assets/sponza), BLENDER (default: blender on PATH, else
# ~/.local/bin/blender) for the knight conversion.
set -euo pipefail
repo="$(cd "$(dirname "$0")/.." && pwd)"
dest="${SPONZA_DIR:-$repo/assets/sponza}"
mkdir -p "$dest"

declare -A ids=([main]=830833 [curtains]=726650 [ivy]=726656 [trees]=726662 [candles]=726676 [knight]=763175)
packages=("$@")
[[ ${#packages[@]} -gt 0 ]] || packages=(main curtains ivy trees candles knight)

for package in "${packages[@]}"; do
  id="${ids[$package]:-}"
  [[ -n $id ]] || { echo "unknown package $package (expected one of ${!ids[*]})" >&2; exit 1; }
  # cdrdv2.intel.com serves a certificate chain curl rejects, but it only answers with a 302 to a signed
  # CloudFront URL; the archive itself is then downloaded with full TLS verification.
  url="$(curl -ks -o /dev/null -w '%{redirect_url}' "https://cdrdv2.intel.com/v1/dl/getContent/$id")"
  [[ $url == https://*.cloudfront.net/* ]] || { echo "unexpected redirect for $package ($id): '$url'" >&2; exit 1; }
  echo "== $package ($id)"
  python3 - "$url" "$dest" <<'PY'
import io, os, re, struct, sys, urllib.request, zipfile, zlib

url, dest = sys.argv[1], sys.argv[2]
SKIP = re.compile(r"(\.(max|fbx|usda|abc|ma|swatches)$)|(^|/)desktop\.ini$|/Exports/|/Scenes/", re.I)

def get(start, end):
    request = urllib.request.Request(url, headers={"Range": f"bytes={start}-{end}"})
    return urllib.request.urlopen(request, timeout=120)

class Remote(io.RawIOBase):
    """Seekable read-only view of the remote zip, for zipfile to read the central directory."""
    def __init__(self):
        self.pos = 0
        self.size = int(urllib.request.urlopen(urllib.request.Request(url, method="HEAD"), timeout=60).headers["Content-Length"])
    def seekable(self): return True
    def readable(self): return True
    def tell(self): return self.pos
    def seek(self, offset, whence=0):
        self.pos = offset if whence == 0 else self.pos + offset if whence == 1 else self.size + offset
        return self.pos
    def readinto(self, buffer):
        if self.pos >= self.size or len(buffer) == 0: return 0
        data = get(self.pos, min(self.size, self.pos + len(buffer)) - 1).read()
        buffer[:len(data)] = data
        self.pos += len(data)
        return len(data)

archive = zipfile.ZipFile(io.BufferedReader(Remote(), 1 << 20))
for info in archive.infolist():
    name = info.filename
    if not info.flag_bits & 0x800:  # names without the UTF-8 flag decode as cp437; Intel's are UTF-8
        try: name = name.encode("cp437").decode("utf-8")
        except UnicodeError: pass
    if info.is_dir() or SKIP.search(name):
        continue
    target = os.path.join(dest, name)
    if os.path.isfile(target) and os.path.getsize(target) == info.file_size:
        print(f"ok       {name}")
        continue
    os.makedirs(os.path.dirname(target), exist_ok=True)
    header = get(info.header_offset, info.header_offset + 29).read()
    signature, name_length, extra_length = struct.unpack("<I22xHH", header)
    assert signature == 0x04034B50, f"bad local header for {name}"
    start = info.header_offset + 30 + name_length + extra_length
    inflate = zlib.decompressobj(-15) if info.compress_type == zipfile.ZIP_DEFLATED else None
    assert inflate or info.compress_type == zipfile.ZIP_STORED, f"{name}: unsupported compression {info.compress_type}"
    crc, size = 0, 0
    with open(target + ".part", "wb") as out:
        if info.compress_size > 0:
            stream = get(start, start + info.compress_size - 1)
            while chunk := stream.read(1 << 22):
                data = inflate.decompress(chunk) if inflate else chunk
                out.write(data); crc = zlib.crc32(data, crc); size += len(data)
        if inflate:
            data = inflate.flush(); out.write(data); crc = zlib.crc32(data, crc); size += len(data)
    if size != info.file_size or crc != info.CRC:
        os.remove(target + ".part")
        sys.exit(f"{name}: size/CRC mismatch")
    os.replace(target + ".part", target)
    print(f"fetched  {name} ({size / 1e6:.0f} MB)")
PY
done

# Bake the knight's per-vertex animation (USD) into the renderer's vertex cache.
usd="$dest/pkg_e_knight_anim/knight_USD_PREVIEW_SURFACE_ANIM_002_1.usd"
cache="$dest/knight/knight.cache"
converter="$repo/scripts/sponza/convert-knight.py"
if [[ -f $usd ]]; then
  if [[ -f $cache && $cache -nt $usd && $cache -nt $converter ]]; then
    echo "ok       knight/knight.cache"
  else
    blender="${BLENDER:-$(command -v blender || echo "$HOME/.local/bin/blender")}"
    "$blender" -b --python "$converter" -- "$usd" "$cache" | grep -E "pieces|wrote|bounds|:" || true
    [[ -f $cache && $cache -nt $usd ]] || { echo "knight conversion failed" >&2; exit 1; }
  fi
fi
