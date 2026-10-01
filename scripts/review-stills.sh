#!/usr/bin/env bash
# Renders the five dragon-flight review frames (one per shot) and a 5-up contact sheet.
#
#   scripts/review-stills.sh LABEL [extra AnimationRunner args...]
#
# Output: artifacts/review/LABEL/frame_NNNNN.png and artifacts/review/LABEL/contact.png.
# Defaults are 960x540 at the runner's default settings; extra args come last, so they override
# (e.g. `--res 480x270 --integrator path --denoise --transfer aces --spp 64`).
# FRAMES="48 192" limits the frames rendered. NO_BUILD=1 skips the Release build.
set -euo pipefail
cd "$(dirname "$0")/.."

label=${1:?usage: scripts/review-stills.sh LABEL [extra args...]}
shift
# 2 s establish, 8 s tracking, 15.5 s summit pass (shadow on the face), 21 s roar, 27 s departure (24 fps).
frames=${FRAMES:-"48 192 372 504 648"}
out="artifacts/review/$label"
mkdir -p "$out"

if [[ -z ${NO_BUILD:-} ]]; then
  dotnet build AnimationRunner -c Release -v q -nologo >/dev/null
fi
runner=AnimationRunner/bin/Release/net10.0/AnimationRunner.dll

echo "$*" > "$out/args.txt"
for f in $frames; do
  scratch="$out/.frame-$f"
  rm -rf "$scratch"
  dotnet "$runner" --demo dragon-flight --res 960x540 --no-video --no-audio --no-resume \
    --start "$f" --end $((f + 1)) --out "$scratch" "$@"
  mv "$scratch/frame_$(printf %05d "$f").png" "$out/"
  rm -rf "$scratch"
done

shopt -s nullglob
stills=("$out"/frame_*.png)
if (( ${#stills[@]} > 0 )); then
  ffmpeg -loglevel error -y -pattern_type glob -i "$out/frame_*.png" \
    -vf "scale=640:-1,tile=${#stills[@]}x1:padding=4" -frames:v 1 "$out/contact.png"
  echo "Contact sheet: $out/contact.png"
fi
