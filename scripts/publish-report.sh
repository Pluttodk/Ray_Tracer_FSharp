#!/usr/bin/env bash
#
# Publish the HTML benchmark reports under artifacts/ to the valdbjorn web
# server, where nginx serves them at https://valdbjorn.com/fsharp-ray/.
#
# The reports link to each other with relative paths that walk up one level
# (../images/index.html, ../gpu-detailed/live.html, ...), so the published tree
# must mirror artifacts/ itself: /fsharp-ray/ IS artifacts/. Only the report
# directories are shipped - the renderer snapshots, worker binaries and scene
# assets stay local.
#
# Re-run this as often as you like: it is a plain incremental rsync, so a run
# that is still producing images can be published repeatedly while it works.
# The live.html pages refresh themselves every 20 seconds in the browser.
#
# Usage:
#   scripts/publish-report.sh                 # publish once
#   scripts/publish-report.sh --watch 120     # re-publish every 120s until Ctrl-C
#   scripts/publish-report.sh --no-raw        # skip the ~300MB of raw .pfm files
#   scripts/publish-report.sh --delete        # also remove files gone from artifacts/
#   scripts/publish-report.sh --dry-run       # show what would transfer
#
set -euo pipefail

REMOTE=${REMOTE:-valdbjorn}
DEST=${DEST:-/var/www/valdbjorn/fsharp-ray}
SITE_URL=${SITE_URL:-https://valdbjorn.com/fsharp-ray/}

REPO_ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
ARTIFACTS="$REPO_ROOT/artifacts"

watch_seconds=0
rsync_extra=()

while [ $# -gt 0 ]; do
  case "$1" in
    --watch)   watch_seconds=${2:?--watch needs an interval in seconds}; shift 2 ;;
    --watch=*) watch_seconds=${1#*=}; shift ;;
    # The reports link every render's unclamped linear PFM as a download. They
    # are by far the biggest thing here; dropping them leaves those links dead
    # but cuts the upload from ~400MB to ~90MB.
    --no-raw)  rsync_extra+=(--exclude='*.pfm' --exclude='*.rgb64'); shift ;;
    --delete)  rsync_extra+=(--delete-after --exclude='/index.html'); shift ;;
    --dry-run) rsync_extra+=(--dry-run); shift ;;
    -h|--help) sed -n '2,26p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
    *) echo "unknown option: $1" >&2; exit 2 ;;
  esac
done

[ -d "$ARTIFACTS" ] || { echo "no artifacts/ directory at $ARTIFACTS" >&2; exit 1; }

# Which directories make up the published site: everything that has a report
# page of its own, plus the shared image folders those reports point at. Picking
# them up by inspection rather than from a fixed list means a report directory
# added by a later run (gpu-detailed-materials, say) is published automatically.
select_dirs() {
  local d
  for d in "$ARTIFACTS"/*/; do
    d=${d%/}
    if [ -e "$d/index.html" ] || [ -e "$d/live.html" ]; then
      basename "$d"
    fi
  done
  for d in images gold-dragon; do
    [ -d "$ARTIFACTS/$d" ] && echo "$d"
  done
}

# The landing page. Regenerated on every publish so its "updated" line and the
# set of links track whatever the run has produced so far.
write_landing_page() {
  local out=$1 gpu_link cpu_link
  {
    cat <<'HTML'
<!doctype html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>F# ray tracer: benchmark reports</title>
<style>
body{font:16px/1.55 system-ui,sans-serif;margin:0;background:#12161d;color:#e4eaf2}
main{max-width:820px;margin:auto;padding:2.5rem 1.5rem 4rem}
h1{line-height:1.2;margin:0 0 .4rem}
h2{font-size:1.15rem;margin:0 0 .35rem}
a{color:#83bcff}
p.lede{color:#adbacb;margin:0 0 2rem}
.card{display:block;padding:1.1rem 1.3rem;margin:0 0 1rem;background:#1a2330;
      border:1px solid #2c3849;border-radius:.6rem;text-decoration:none;color:inherit}
.card:hover{border-color:#83bcff}
.card p{margin:0;color:#adbacb;font-size:.92rem}
.more{margin-top:2.2rem;font-size:.92rem;color:#adbacb}
.more li{margin:.3rem 0}
footer{margin-top:2.5rem;color:#7e8b9d;font-size:.82rem}
</style></head><body><main>
<h1>F# ray tracer &mdash; benchmark reports</h1>
<p class="lede">Rendered output and timings for the modernised renderer, measured against
the frozen legacy port on the CPU and against the CUDA GPU backend.</p>
HTML

    # The CPU-versus-CPU report: the final page appears when the run finishes,
    # the live page is what exists while it is still measuring.
    cpu_link=""
    [ -e "$ARTIFACTS/benchmarks/live.html" ]  && cpu_link="benchmarks/live.html"
    [ -e "$ARTIFACTS/benchmarks/index.html" ] && cpu_link="benchmarks/index.html"
    if [ -n "$cpu_link" ]; then
      cat <<HTML
<a class="card" href="$cpu_link">
<h2>CPU vs CPU &mdash; legacy port against the modernised renderer</h2>
<p>Cold wall time, per-phase timings and linear-image differences across the
benchmark scenes. Every repeat is retained.</p></a>
HTML
      if [ "$cpu_link" != "benchmarks/live.html" ] && [ -e "$ARTIFACTS/benchmarks/live.html" ]; then
        echo '<p class="more" style="margin:-0.6rem 0 1rem"><a href="benchmarks/live.html">Live progress page for the CPU run</a></p>'
      fi
    fi

    gpu_link=""
    [ -e "$ARTIFACTS/gpu-detailed/live.html" ]  && gpu_link="gpu-detailed/live.html"
    [ -e "$ARTIFACTS/gpu-detailed/index.html" ] && gpu_link="gpu-detailed/index.html"
    if [ -n "$gpu_link" ]; then
      cat <<HTML
<a class="card" href="$gpu_link">
<h2>GPU &mdash; detailed CUDA renders</h2>
<p>Full-size images from the CUDA backend for the authored scenes. Refreshes
itself while the run is still producing images.</p></a>
HTML
    fi

    if [ -e "$ARTIFACTS/gpu-detailed-materials/live.html" ] || [ -e "$ARTIFACTS/gpu-detailed-materials/index.html" ]; then
      cat <<'HTML'
<a class="card" href="gpu-detailed-materials/live.html">
<h2>GPU &mdash; material variants</h2>
<p>The same scenes rendered across the material presets on the GPU.</p></a>
HTML
    fi

    if [ -e "$ARTIFACTS/images/index.html" ]; then
      cat <<'HTML'
<a class="card" href="images/index.html">
<h2>Images: old versus new</h2>
<p>Side-by-side renders, plus the plainly named PNG folders.</p></a>
HTML
    fi

    echo '<div class="more"><p>Supporting runs:</p><ul>'
    local d name
    for d in $(select_dirs | sort); do
      case "$d" in benchmarks|gpu-detailed|gpu-detailed-materials|images|gold-dragon) continue ;; esac
      name=$([ -e "$ARTIFACTS/$d/index.html" ] && echo index.html || echo live.html)
      echo "<li><a href=\"$d/$name\">$d</a></li>"
    done
    echo '</ul></div>'

    echo "<footer>Published $(date -u '+%Y-%m-%d %H:%M UTC') from the working tree on $(hostname).</footer>"
    echo '</main></body></html>'
  } > "$out"
}

publish_once() {
  local dirs landing
  dirs=$(select_dirs | sort -u)
  [ -n "$dirs" ] || { echo "no report directories found under artifacts/" >&2; return 1; }

  echo "==> publishing to $REMOTE:$DEST"
  ssh "$REMOTE" "mkdir -p '$DEST'"

  local d
  for d in $dirs; do
    printf '  %-32s' "$d"
    rsync -a --chmod=D755,F644 "${rsync_extra[@]}" \
      "$ARTIFACTS/$d/" "$REMOTE:$DEST/$d/" \
      --info=stats2 | awk '/Number of regular files transferred/{f=$NF} /Total transferred file size/{s=$(NF-1)} END{printf "%s files, %s bytes\n", (f==""?"0":f), (s==""?"0":s)}'
  done

  landing=$(mktemp)
  write_landing_page "$landing"
  rsync -a --chmod=F644 "$landing" "$REMOTE:$DEST/index.html"
  rm -f "$landing"

  echo "==> done: $SITE_URL"
}

if [ "$watch_seconds" -gt 0 ] 2>/dev/null; then
  echo "publishing every ${watch_seconds}s - Ctrl-C to stop"
  while true; do
    publish_once || echo "publish failed, retrying next cycle" >&2
    sleep "$watch_seconds"
  done
else
  publish_once
fi
