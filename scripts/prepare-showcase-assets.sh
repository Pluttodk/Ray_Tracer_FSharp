#!/usr/bin/env bash
# Downloads the high-resolution Stanford scans used by the showcase-gallery
# scene, verifying each against a pinned SHA-256.
#
# LICENSING. These models come from the Stanford 3D Scanning Repository and the
# Stanford Large Geometric Models Archive. They are used under Stanford's
# attributed, noncommercial research terms. They are NOT GPL-relicensed, are not
# committed to this repository, and land in ignored artifact directories.
# Credit: Stanford University Computer Graphics Laboratory.
# See https://graphics.stanford.edu/data/3Dscanrep/
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
downloads="$root/artifacts/downloads"
assets="$root/artifacts/scene-assets/stanford"
mkdir -p "$downloads" "$assets"

fetch () { # url filename sha256
  local url="$1" name="$2" want="$3" path="$downloads/$2"
  if [ ! -f "$path" ]; then
    echo "downloading $name"
    curl -fL --retry 3 -o "$path" "$url"
  fi
  local got
  got="$(sha256sum "$path" | cut -d" " -f1)"
  if [ "$got" != "$want" ]; then
    echo "checksum mismatch for $name: expected $want, got $got" >&2
    echo "refusing to use a changed or incomplete asset" >&2
    exit 1
  fi
  echo "verified $name"
}

fetch "https://graphics.stanford.edu/pub/3Dscanrep/happy/happy_recon.tar.gz" \
      "happy_recon.tar.gz" "409cd294efbfd8244e15a382b95a9423f153b7776e736c9b09f19ec9d3c10ed0"
fetch "http://graphics.stanford.edu/data/3Dscanrep/xyzrgb/xyzrgb_dragon.ply.gz" \
      "xyzrgb_dragon.ply.gz" "8aa449f1966cbb50e5896ecc32cf57ab5f0cdfd3c3e37d3e6f60b948997da5c1"
fetch "http://graphics.stanford.edu/data/3Dscanrep/xyzrgb/xyzrgb_statuette.ply.gz" \
      "xyzrgb_statuette.ply.gz" "1d867b6540c02935caa777bd6746429a62d4a5d23f11c9bfdfebbaa90c05ca8b"

[ -d "$assets/happy_recon" ] || tar xzf "$downloads/happy_recon.tar.gz" -C "$assets"
[ -f "$assets/xyzrgb_dragon.ply" ] || gunzip -kc "$downloads/xyzrgb_dragon.ply.gz" > "$assets/xyzrgb_dragon.ply"
[ -f "$assets/xyzrgb_statuette.ply" ] || gunzip -kc "$downloads/xyzrgb_statuette.ply.gz" > "$assets/xyzrgb_statuette.ply"

echo
echo "prepared:"
echo "  Thai Statuette   10,000,000 triangles  $assets/xyzrgb_statuette.ply"
echo "  XYZ RGB Dragon    7,219,045 triangles  $assets/xyzrgb_dragon.ply"
echo "  Happy Buddha      1,087,716 triangles  $assets/happy_recon/happy_vrip.ply"
echo
echo "The Stanford Dragon is prepared separately by scripts/prepare-gold-dragon.fsx."
echo "Then: dotnet fsi scripts/make-showcase-scene.fsx"
