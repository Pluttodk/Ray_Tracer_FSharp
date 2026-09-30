#!/usr/bin/env bash
# Downloads the third-party assets used by the "dragon-flight" animation into assets/dragon/.
# All three are public domain (see assets/dragon/SOURCES.md); checksums pin the exact files.
set -euo pipefail
cd "$(dirname "$0")/../assets/dragon"
fetch() {
  local name=$1 url=$2 sha=$3
  if [[ -f $name ]] && echo "$sha  $name" | sha256sum --check --status; then
    echo "ok       $name"; return
  fi
  curl -fsSL --retry 3 -A "RayTracerAnimation/1.0" -o "$name.part" "$url"
  echo "$sha  $name.part" | sha256sum --check --status || { echo "checksum mismatch for $name" >&2; rm -f "$name.part"; exit 1; }
  mv "$name.part" "$name"
  echo "fetched  $name"
}
fetch dragon_evolved.glb \
  https://static.poly.pizza/90ed3740-d8c4-4910-88ce-ac2ed426022d.glb \
  f660d8f48be4ea3f39639f6206a0181127e7fe7503cb4bad8d07d27d80528f3a
fetch grizzly_roar.mp3 \
  "https://upload.wikimedia.org/wikipedia/commons/e/ef/Yellowstone_sound_library_-_Grizzly_Bears_Roar_-_001.mp3" \
  d51c07891cb576bbd62bae39fbd716183c9bf2eef79dc79e081e9b47c790dc63
fetch alligator_bellow.ogg \
  https://upload.wikimedia.org/wikipedia/commons/1/1a/Alligatorbellow1.ogg \
  72a5612e99b6a941d751efbccf1e44f816c06c7884e3108c5298a2ba84b25169
