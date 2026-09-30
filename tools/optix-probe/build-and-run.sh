#!/usr/bin/env bash
# Milestone 0: does hardware-accelerated OptiX initialize on this machine?
#
# Needs no CUDA toolkit. The OptiX headers are self-contained apart from two
# opaque handle types (supplied by shim/cuda.h), the CUDA driver API is resolved
# at runtime out of libcuda.so.1, and OptiX resolves itself out of
# libnvoptix.so.1. Fetch the headers first:
#
#   git clone --depth 1 https://github.com/NVIDIA/optix-dev
#
# then run:  ./build-and-run.sh /path/to/optix-dev
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
optix="${1:-$here/optix-dev}"
[ -d "$optix/include" ] || { echo "OptiX headers not found at $optix/include"; exit 2; }
g++ -O2 -I"$optix/include" -I"$here/shim" "$here/probe.cpp" -o "$here/probe" -ldl
LD_LIBRARY_PATH="/usr/lib/wsl/lib:${LD_LIBRARY_PATH:-}" "$here/probe"
