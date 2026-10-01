#!/usr/bin/env bash
# Renders the `sponza` demo at one time with our renderer and with the Cycles ground truth
# (scripts/sponza/cycles_reference.py, same scenes/sponza.json, same post chain and ACES), and writes
#
#   artifacts/ground-truth/<label>-ours.png      our frame (AnimationRunner --demo sponza, its render defaults)
#   artifacts/ground-truth/<label>-cycles.png    Cycles (+ .exr linear radiance, .json settings)
#   artifacts/ground-truth/<label>-compare.png   ours | Cycles | |difference| x4
#
# and prints per-channel brightness ratios (ours / Cycles, measured on approximately linear values: sRGB and
# ACES inverted, so bloom/vignette/saturation affect both sides alike).
#
#   scripts/sponza/compare.sh <time-seconds> [label]
#
# Environment: RES (default 480x270), SPP (ours; default: the demo's recommendation), CYCLES_SPP (256),
# OURS_ARGS / CYCLES_ARGS (extra args), SKIP_OURS=1 / SKIP_CYCLES=1 (reuse an existing image),
# NO_BUILD=1 (skip the Release build), CLAMP (ours' indirect clamp, default 0 like Cycles), POST=ours (keep bloom, vignette and saturation on both sides; by default
# both get only exposure + ACES: ours with --bloom 0 --vignette 0 --saturation 1, Cycles with --post none).
# Our side renders one frame (frame = round(time * 24)).
set -euo pipefail
cd "$(dirname "$0")/../.."

time=${1:?usage: scripts/sponza/compare.sh <time-seconds> [label]}
label=${2:-t$time}
res=${RES:-480x270}
out=artifacts/ground-truth
mkdir -p "$out"
blender=${BLENDER:-$HOME/.local/bin/blender}
export PATH=$HOME/.dotnet:$PATH

frame=$(python3 -c "print(round($time * 24))")
if [[ ${POST:-none} == ours ]]; then
  ours_post=()
  cycles_post=()
else
  ours_post=(--bloom 0 --vignette 0 --saturation 1)
  cycles_post=(--post none)
fi
# The sponza defaults clamp indirect light at 8 (fewer fireflies, biased); Cycles runs unclamped.
ours_post+=(--clamp "${CLAMP:-0}")
ours="$out/$label-ours.png"
cycles="$out/$label-cycles.png"

if [[ -z ${SKIP_OURS:-} ]]; then
  if [[ -z ${NO_BUILD:-} ]]; then
    dotnet build AnimationRunner -c Release -v q -nologo >/dev/null
  fi
  scratch="$out/.ours-$label"
  rm -rf "$scratch"
  spp_args=()
  [[ -n ${SPP:-} ]] && spp_args=(--spp "$SPP")
  # shellcheck disable=SC2086
  /usr/bin/time -f "ours: %e s, max RSS %M KB" dotnet AnimationRunner/bin/Release/net10.0/AnimationRunner.dll \
    --demo sponza --res "$res" "${spp_args[@]}" --no-video --no-audio --no-resume \
    --start "$frame" --end $((frame + 1)) --out "$scratch" "${ours_post[@]}" ${OURS_ARGS:-}
  mv "$scratch/frame_$(printf %05d "$frame").png" "$ours"
  rm -rf "$scratch"
fi

if [[ -z ${SKIP_CYCLES:-} ]]; then
  # shellcheck disable=SC2086
  "$blender" -b --python scripts/sponza/cycles_reference.py -- --time "$time" --res "$res" \
    --spp "${CYCLES_SPP:-256}" --out "$cycles" "${cycles_post[@]}" ${CYCLES_ARGS:-} 2>&1 | grep -E '^\[gt\]|Error|Traceback' || true
fi

font=/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf
[[ -f $font ]] || font=$(fc-list : file | head -1 | cut -d: -f1)
tag() { echo "drawtext=fontfile=$font:text='$1':x=6:y=6:fontsize=14:fontcolor=white:box=1:boxcolor=black@0.5"; }
ffmpeg -loglevel error -y -i "$ours" -i "$cycles" -filter_complex \
  "[0:v]format=rgb24,split[a][a2];[1:v]format=rgb24,split[b][b2];\
[a2][b2]blend=all_mode=difference,lutrgb=r='clip(val*4,0,255)':g='clip(val*4,0,255)':b='clip(val*4,0,255)',$(tag '|ours - cycles| x4')[d];\
[a]$(tag "ours t=$time")[a1];[b]$(tag "cycles t=$time")[b1];[a1][b1][d]hstack=inputs=3" \
  -frames:v 1 "$out/$label-compare.png"

python3 - "$ours" "$cycles" <<'EOF'
import subprocess, sys
import numpy as np

def load(path):
    probe = subprocess.run(["ffprobe", "-v", "error", "-select_streams", "v:0", "-show_entries", "stream=width,height",
                            "-of", "csv=p=0", path], capture_output=True, text=True).stdout.strip().split(",")
    w, h = int(probe[0]), int(probe[1])
    raw = subprocess.run(["ffmpeg", "-loglevel", "error", "-i", path, "-f", "rawvideo", "-pix_fmt", "rgb24", "-"],
                         capture_output=True).stdout
    return np.frombuffer(raw, np.uint8).reshape(h, w, 3) / 255.0

def linearise(e):
    """Inverse sRGB then inverse ACES (Narkowicz): display value -> exposed linear value."""
    y = np.where(e <= 0.04045, e / 12.92, ((e + 0.055) / 1.055) ** 2.4)
    y = np.minimum(y, 0.999)
    a, b, c = 2.51 - 2.43 * y, 0.03 - 0.59 * y, -0.14 * y
    return (-b + np.sqrt(b * b - 4 * a * c)) / (2 * a)

o, c = load(sys.argv[1]), load(sys.argv[2])
lo, lc = linearise(o), linearise(c)
h, w, _ = o.shape
names = "RGB"
def report(name, sl):
    mo, mc = lo[sl].reshape(-1, 3).mean(0), lc[sl].reshape(-1, 3).mean(0)
    ratio = " ".join(f"{names[i]} {mo[i] / max(mc[i], 1e-6):.2f}" for i in range(3))
    lum = lambda m: 0.2126 * m[0] + 0.7152 * m[1] + 0.0722 * m[2]
    print(f"  {name:8s} ours/cycles luminance {lum(mo) / max(lum(mc), 1e-6):.2f}  ({ratio})")
print("Brightness (approx. linear, after exposure):")
report("frame", np.s_[:, :])
report("top", np.s_[: h // 3, :])
report("middle", np.s_[h // 3: 2 * h // 3, :])
report("bottom", np.s_[2 * h // 3:, :])
d = np.abs(o - c).mean()
print(f"  mean |display difference| {d * 255:.1f} / 255")
EOF
echo "Comparison: $out/$label-compare.png"
