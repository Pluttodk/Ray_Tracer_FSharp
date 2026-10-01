"""Blender Cycles ground truth for the `sponza` demo.

Builds the scene that RayTracer.Animation/Sponza.fs builds, from the same scenes/sponza.json, renders it
with Cycles on the GPU and tone maps it offline with an exact copy of our post chain (Post.fs: bloom,
exposure, white balance, saturation, vignette; then Colour.fs: ACES Narkowicz fit and sRGB encoding), so
the two images differ only by light transport, materials and geometry handling.

    ~/.local/bin/blender -b --python scripts/sponza/cycles_reference.py -- \
        --time 5.0 --res 480x270 --spp 256 --out artifacts/ground-truth/<label>.png [--json scenes/sponza.json]

Writes <label>.png (tone mapped like ours), <label>.exr (linear radiance, before exposure) and
<label>.json (the settings used). Other options: see `--help`. Calibration (no Sponza):

    blender -b --python scripts/sponza/cycles_reference.py -- --calib sun|lamp --res 64x64 --out X.png
    python3 scripts/sponza/cycles_reference.py --write-calib-gltf X.gltf --calib sun   # the same scene for ours

Coordinates. Our scene is glTF's +Y up; Blender's glTF importer maps glTF (x, y, z) to Blender (x, -z, y).
Every direction and position from the json goes through `to_blender`.

UNIT CONVERSION (calibrated, see `--calib`; one constant for every light type)

  Our renderer's units are "W"-like render units and Cycles' are W, and they agree 1:1:
  * Sun: our DirectionalLight(colour, E) gives irradiance colour*E on a surface facing it (a white
    Lambertian surface facing it has radiance E/pi). Cycles' Sun strength is irradiance in W/m^2, same
    meaning, so  strength = K * irradiance  with K = UNIT_SCALE = 1.
  * Lamps: our SphereLight(colour, I, radius) has radiant intensity colour*I per sr (irradiance I/d^2 at
    distance d, power 4 pi I). A Cycles point light of power P W has radiant intensity P / (4 pi) W/sr, so
    P = 4 pi * K * I  with the same K.
  * Sky / HDRI: environment radiance maps 1:1 (Background strength = K * intensity); the Preetham sky of
    Sky.fs is baked into an equirectangular EXR in render units, so it needs no extra factor.

MATCHING CHOICES
  * Sky: `--sky auto` uses the json's sky.model if present, else follows Sponza.fs: with sky.hdri set (and
    Sponza.fs using HdrEnvironment) the world is that HDRI with its own sun clamped out exactly like
    HdrEnvironment.extractSun (auto level max(peak/1000, 50*median), 5 degrees; numpy port, cached as
    artifacts/ground-truth/cache/hdri-*.hdr), through a Mapping node (Point, Rotation Z = rotationDeg; image
    centre towards +X, u = 0.25 towards -Z, the convention of HdrImage.fs) at Background strength =
    intensity. Without an HDRI it is Sky.fs's Preetham sky: a numpy port of Sky.radiance (sun disc
    excluded, darkened horizon below) baked to a 1024x512 equirect.
  * Lamps: when lamps.enabled, Cycles point lights (sphere radius lamps.radius, invisible to the camera
    like our SphereLight) at the main file's lamp_light_* positions + lamps.offset.
  * Sun: delta light by default (`--sun-angle 0`, like our DirectionalLight); 0.53 gives the real penumbra.
  * Bounces 4 (Sponza MaxBounces), transparent bounces 64, no clamping, no motion blur (ours has a 0.5
    frame shutter; the cameras move slowly), Blackman-Harris pixel filter, OIDN with albedo + normal.
  * Post: our sponza render defaults (Film.fs renderDefaults: bloom 0.1 above 1.5, vignette 0.15,
    saturation 1.05, exposure = json exposure, ACES); `--post none` for the bare exposure + transfer.
  * Geometry: the glTF parts are imported once and cached as artifacts/ground-truth/cache/parts-*.blend
    (keyed by file paths and mtimes; ~1.4 GB), so a frame takes ~30 s including a 256 spp render.

CALIBRATION (2026-10-01; Blender 4.2.23 Cycles OptiX 256 spp vs ours: path integrator 256 spp, --transfer linear,
no post; 64x64 frames of a 4 x 4 m plane, albedo 0.8, roughness 1, metallic 0, seen from 3 m straight above)
  sun  E = 2.0 head-on:   analytic 0.8 * 2 / pi = 0.5093
                          Cycles  0.5086 (-0.1 %)   ours 0.5059..0.5098 (8-bit PNG; within 0.7 %)
  lamp I = 2.0 W/sr, 1 m above the plane centre (point, radius 0):
                          analytic centre 0.5093, 12 px off-centre 0.4478, 24 px 0.3269
                          Cycles  0.5076 / 0.4476 / 0.3270 (within 0.3 %, inverse-square + cosine confirmed)
                          ours: SphereLight is defined as exactly this analytic falloff (BasicLight.fs on
                          sponza-r2); the base branch has no attenuated lamp, so it was not rendered.
  => UNIT_SCALE = 1 for every light type. Reproduce:
     blender -b --python scripts/sponza/cycles_reference.py -- --calib sun --res 64x64 --out artifacts/ground-truth/calib/cycles-sun.png
     dotnet AnimationRunner/bin/Release/net10.0/AnimationRunner.dll --scene artifacts/ground-truth/calib/cycles-sun-scene.gltf \
         --res 64x64 --spp 256 --integrator path --transfer linear --light-scale 3.3333333 --no-video --no-audio \
         --start 0 --end 1 --out artifacts/ground-truth/calib/ours-sun
     (--light-scale undoes the importer's DirectionalLightScale = 0.3.)

Knight / extra geometry. Every part listed in the json's `include` is imported by file extension
(.gltf/.glb via the glTF importer, .usd/.usdc/.usda via the USD importer, .fbx via the FBX importer);
`--extra FILE` adds more. The knight's vertex cache (assets/sponza/knight/knight.cache, written by
scripts/sponza/convert-knight.py, played by Knight.fs) is read directly and posed at the render time when
the json has a `knight` object (or with `--knight CACHE`), mirroring Knight.placeAt / Knight.clock:
    "knight": {"enabled": true, "position": [x, y, z], "yawDeg": 0, "start": 0, "speed": 1}
(animation time = (t - start) * speed, linear between the cache's 24 fps frames, clamped; at yaw 0 the
knight faces -X; optional "cache", default knight/knight.cache). Using the cache rather than the USD gives
exactly the geometry our renderer sees (same points, same smooth normals across UV seams). Its materials
follow Knight.describe (base colour x map, roughness = red channel of the roughness map, tangent-space
normal map); maxTexture is ignored (full-resolution maps).
"""

import argparse
import hashlib
import json
import math
import os
import struct
import sys
import time
import zlib

import numpy as np

UNIT_SCALE = 1.0  # our render units -> Cycles W (sun strength W/m^2, point power W/(4 pi sr)); see header

# ------------------------------------------------------------------------------------------------ arguments


def parse_args(argv):
    p = argparse.ArgumentParser(description="Cycles ground truth for the sponza demo")
    p.add_argument("--json", default=None, help="scene description (default: scenes/sponza.json, searched upwards)")
    p.add_argument("--time", type=float, default=0.0, help="time in seconds (picks the shot and camera pose)")
    p.add_argument("--res", default="480x270")
    p.add_argument("--spp", type=int, default=256)
    p.add_argument("--out", default="artifacts/ground-truth/cycles.png")
    p.add_argument("--bounces", type=int, default=4, help="max bounces (Sponza.fs MaxBounces = 4)")
    p.add_argument("--sky", default="auto", choices=["auto", "preetham", "hdri", "none"],
                   help="auto: json sky.model if present, else whatever Sponza.fs uses (Sky.light -> preetham)")
    p.add_argument("--sun-angle", type=float, default=0.0,
                   help="sun angular diameter in degrees (0 = delta light like our DirectionalLight; real sun 0.53)")
    p.add_argument("--lamps", default="auto", choices=["auto", "on", "off"], help="auto follows json lamps.enabled")
    p.add_argument("--no-denoise", action="store_true")
    p.add_argument("--clamp", type=float, default=0.0, help="Cycles indirect clamp (0 = off, unbiased)")
    # Post chain; defaults are Film.fs renderDefaults "sponza".
    p.add_argument("--exposure", type=float, default=None, help="default: json exposure, else 3")
    p.add_argument("--bloom", type=float, default=0.1)
    p.add_argument("--bloom-threshold", type=float, default=1.5)
    p.add_argument("--vignette", type=float, default=0.15)
    p.add_argument("--saturation", type=float, default=1.05)
    p.add_argument("--white-balance", type=float, default=0.0)
    p.add_argument("--transfer", default="aces", choices=["aces", "srgb", "linear", "gamma2"])
    p.add_argument("--post", default="ours", choices=["ours", "none"],
                   help="none: no bloom/grade/vignette (exposure and transfer still apply)")
    p.add_argument("--gpus", default="auto", help="auto (GPUs with >= 20 GB free), all, cpu, or indices like 0,1")
    p.add_argument("--extra", action="append", default=[], help="extra geometry file(s) to import (glb/gltf/usd/fbx)")
    p.add_argument("--knight", default=None,
                   help="knight vertex cache (relative to assets/sponza or absolute); placement from json `knight`")
    p.add_argument("--force-opaque", default="",
                   help="diagnostic: comma-separated material name prefixes to render with alpha 1 (e.g. dirt_decal,LeafSpring)")
    p.add_argument("--hdri-keep-sun", action="store_true", help="diagnostic: use the HDRI unclamped (its own sun too)")
    p.add_argument("--no-cache", action="store_true", help="re-import the glTF parts instead of using the .blend cache")
    p.add_argument("--calib", default=None, choices=["sun", "lamp"], help="render a calibration scene instead")
    p.add_argument("--write-calib-gltf", default=None, help="(plain python) write the calibration scene as glTF")
    return p.parse_args(argv)


def script_argv():
    return sys.argv[sys.argv.index("--") + 1:] if "--" in sys.argv else sys.argv[1:]


def repo_root():
    return os.path.abspath(os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", ".."))


def search_up(relative):
    for start in (os.getcwd(), repo_root()):
        d = os.path.abspath(start)
        while True:
            candidate = os.path.join(d, relative)
            if os.path.exists(candidate):
                return candidate
            parent = os.path.dirname(d)
            if parent == d:
                break
            d = parent
    return None


def asset(name):
    path = search_up(os.path.join("assets", "sponza", name))
    if path is None:
        raise SystemExit(f"assets/sponza/{name} is missing; run scripts/fetch-sponza-assets.sh first")
    return path


def to_blender(v):
    """glTF / our +Y-up coordinates -> Blender Z-up (the glTF importer's convention)."""
    return (v[0], -v[2], v[1])


# ------------------------------------------------------------------------------------------------ spec


def sun_direction(spec):
    """Towards the sun, our coordinates (Sponza.sunDirection)."""
    az, el = math.radians(spec["sun"]["azimuthDeg"]), math.radians(spec["sun"]["elevationDeg"])
    return (math.cos(el) * math.sin(az), math.sin(el), math.cos(el) * math.cos(az))


def smooth_tangents(times, values):
    """Smooth.vector's tangents (Tracks.fs): zero at the ends and at extrema, Catmull-Rom elsewhere."""
    n = len(times)
    out = []
    for k in range(n):
        if k == 0 or k == n - 1:
            out.append([0.0, 0.0, 0.0])
        else:
            m = []
            for c in range(3):
                before = values[k][c] - values[k - 1][c]
                after = values[k + 1][c] - values[k][c]
                m.append(0.0 if before * after <= 0 else (values[k + 1][c] - values[k - 1][c]) / (times[k + 1] - times[k - 1]))
            out.append(m)
    return out


def evaluate_track(keys, t):
    """Sponza.cameras' `track`: one key -> constant, else Smooth.vector (glTF cubic spline), clamped."""
    times = [k[0] for k in keys]
    values = [list(k[1]) for k in keys]
    if len(keys) == 1 or t <= times[0]:
        return values[0]
    if t >= times[-1]:
        return values[-1]
    m = smooth_tangents(times, values)
    k = max(i for i in range(len(times) - 1) if times[i] <= t)
    td = times[k + 1] - times[k]
    s = (t - times[k]) / td
    s2, s3 = s * s, s * s * s
    h00, h10, h01, h11 = 2 * s3 - 3 * s2 + 1, s3 - 2 * s2 + s, -2 * s3 + 3 * s2, s3 - s2
    return [h00 * values[k][c] + h10 * td * m[k][c] + h01 * values[k + 1][c] + h11 * td * m[k + 1][c] for c in range(3)]


def camera_at(spec, t):
    """The active shot at t (cuts at each shot's start, like AnimatedScene.activeCamera) and its pose."""
    shots = sorted(spec["shots"], key=lambda s: s["start"])
    active = shots[0]
    for s in shots[1:]:
        if s["start"] <= t:
            active = s
    position = evaluate_track([(k[0], k[1]) for k in active["keys"]], t)
    target = evaluate_track([(k[0], k[2]) for k in active["keys"]], t)
    return active["name"], position, target, math.radians(active["yfovDeg"]), active.get("aperture", 0.0)


def gltf_node_world_positions(path, prefix):
    """World translations of every node whose name starts with `prefix` (lamp_light_*), straight from the glTF."""
    with open(path) as f:
        doc = json.load(f)
    nodes = doc["nodes"]

    def local(n):
        if "matrix" in n:
            return np.array(n["matrix"], dtype=float).reshape(4, 4).T
        t = n.get("translation", [0, 0, 0])
        x, y, z, w = n.get("rotation", [0, 0, 0, 1])
        s = n.get("scale", [1, 1, 1])
        r = np.array([[1 - 2 * (y * y + z * z), 2 * (x * y - z * w), 2 * (x * z + y * w)],
                      [2 * (x * y + z * w), 1 - 2 * (x * x + z * z), 2 * (y * z - x * w)],
                      [2 * (x * z - y * w), 2 * (y * z + x * w), 1 - 2 * (x * x + y * y)]])
        m = np.eye(4)
        m[:3, :3] = r * np.array(s)[None, :]
        m[:3, 3] = t
        return m

    found = []

    def walk(i, parent):
        world = parent @ local(nodes[i])
        if nodes[i].get("name", "").startswith(prefix):
            found.append(tuple(world[:3, 3]))
        for c in nodes[i].get("children", []):
            walk(c, world)

    scene = doc["scenes"][doc.get("scene", 0)]
    for root in scene["nodes"]:
        walk(root, np.eye(4))
    return found


# ------------------------------------------------------------------------------------------------ Preetham sky (Sky.fs)

SOLAR_ILLUMINANCE = 127.5
SUN_ANGULAR_RADIUS = 0.533 / 2 * math.pi / 180
SUN_SOLID_ANGLE = 2 * math.pi * (1 - math.cos(SUN_ANGULAR_RADIUS))
WAVELENGTHS = [0.680, 0.550, 0.440]


def _lum(r, g, b):
    return 0.2126 * r + 0.7152 * g + 0.0722 * b


class Preetham:
    """A line-by-line port of Sky.create / Sky.radiance (without the sun disc, as Sky.light lights)."""

    def __init__(self, sun_dir, turbidity, sun_irradiance, sun_to_sky):
        s = np.array(sun_dir, dtype=float)
        s /= np.linalg.norm(s)
        self.s = s
        t = turbidity
        theta_s = math.acos(s[1])
        self.pY = [0.1787 * t - 1.4630, -0.3554 * t + 0.4275, -0.0227 * t + 5.3251, 0.1206 * t - 2.5771, -0.0670 * t + 0.3703]
        self.px = [-0.0193 * t - 0.2592, -0.0665 * t + 0.0008, -0.0004 * t + 0.2125, -0.0641 * t - 0.8989, -0.0033 * t + 0.0452]
        self.py = [-0.0167 * t - 0.2608, -0.0950 * t + 0.0092, -0.0079 * t + 0.2102, -0.0441 * t - 1.6537, -0.0109 * t + 0.0529]
        chi = (4 / 9 - t / 120) * (math.pi - 2 * theta_s)
        self.zY = max(1e-3, (4.0453 * t - 4.9710) * math.tan(chi) - 0.2155 * t + 2.4192)

        def cubic(a):
            return a[0] * theta_s ** 3 + a[1] * theta_s ** 2 + a[2] * theta_s + a[3]

        t2 = t * t
        self.zx = t2 * cubic([0.00166, -0.00375, 0.00209, 0]) + t * cubic([-0.02903, 0.06377, -0.03202, 0.00394]) + cubic([0.11693, -0.21196, 0.06052, 0.25886])
        self.zy = t2 * cubic([0.00275, -0.00610, 0.00317, 0]) + t * cubic([-0.04214, 0.08970, -0.04153, 0.00516]) + cubic([0.15346, -0.26756, 0.06670, 0.26688])
        at_zenith = lambda c: self._perez(c, np.array(1.0), np.array(theta_s), np.array(s[1]))
        self.fY, self.fx, self.fy = at_zenith(self.pY), at_zenith(self.px), at_zenith(self.py)
        self.scale = 1.0
        mass_deg = min(93.9, theta_s * 180 / math.pi)
        mass = 1 / (math.cos(math.radians(mass_deg)) + 0.50572 * (96.07995 - mass_deg) ** -1.6364)
        beta = 0.04608 * t - 0.04586
        trans = [math.exp(-mass * (0.008735 * l ** -4.08 + beta * l ** -1.3)) for l in WAVELENGTHS]
        sun_model = np.array(trans) * (SOLAR_ILLUMINANCE / SUN_SOLID_ANGLE)
        sun_normal = _lum(*sun_model) * SUN_SOLID_ANGLE
        sky_horizontal = self._horizontal_irradiance()
        boost = (sun_to_sky * sky_horizontal / sun_normal) if sun_to_sky else 1.0
        self.scale = sun_irradiance / (sun_normal * boost)

    @staticmethod
    def _perez(c, cos_theta, gamma, cos_gamma):
        return (1 + c[0] * np.exp(c[1] / cos_theta)) * (1 + c[2] * np.exp(c[3] * gamma) + c[4] * cos_gamma * cos_gamma)

    def _model(self, d):
        """d: (..., 3) unit directions at or above the horizon -> linear sRGB (kcd/m^2)."""
        cos_theta = np.maximum(1e-3, d[..., 1])
        cos_gamma = np.clip(d @ self.s, -1, 1)
        gamma = np.arccos(cos_gamma)
        Y = self.zY * self._perez(self.pY, cos_theta, gamma, cos_gamma) / self.fY
        x = self.zx * self._perez(self.px, cos_theta, gamma, cos_gamma) / self.fx
        y = self.zy * self._perez(self.py, cos_theta, gamma, cos_gamma) / self.fy
        ok = (y > 0) & (Y > 0)
        ys = np.where(ok, y, 1)
        X = x / ys * Y
        Z = (1 - x - y) / ys * Y
        r = 3.2406 * X - 1.5372 * Y - 0.4986 * Z
        g = -0.9689 * X + 1.8758 * Y + 0.0415 * Z
        b = 0.0557 * X - 0.2040 * Y + 1.0570 * Z
        rgb = np.maximum(0, np.stack([r, g, b], -1))
        return np.where(ok[..., None], rgb, 0)

    def _horizontal_irradiance(self):
        rows, cols = 96, 192
        j = np.arange(rows)
        c0, c1 = 1 - j / rows, 1 - (j + 1) / rows
        cosine = 0.5 * (c0 + c1)
        sine = np.sqrt(1 - cosine ** 2)
        phi = 2 * math.pi * (np.arange(cols) + 0.5) / cols
        d = np.stack([sine[:, None] * np.sin(phi)[None, :], np.broadcast_to(cosine[:, None], (rows, cols)),
                      sine[:, None] * np.cos(phi)[None, :]], -1)
        rgb = self._model(d)
        lum = _lum(rgb[..., 0], rgb[..., 1], rgb[..., 2])
        return float(np.sum(lum * (cosine * (c0 - c1))[:, None] * (2 * math.pi / cols)))

    def radiance(self, d):
        """Sky.radiance for unit directions (..., 3), our coordinates, render units."""
        up = d[..., 1] >= 0
        horizontal = d.copy()
        horizontal[..., 1] = 0
        n = np.linalg.norm(horizontal, axis=-1, keepdims=True)
        h = np.where(n > 1e-9, horizontal / np.maximum(n, 1e-12), np.array([1.0, 0, 0]))
        fade = 0.35 + 0.65 * np.exp(d[..., 1] * 30)
        above = self._model(d) * self.scale
        below = self._model(h) * (self.scale * fade)[..., None]
        return np.where(up[..., None], above, below)


def bake_equirect(sky, width=1024):
    """An equirect image in Blender's Environment Texture convention, rows bottom-up (Blender pixel order).

    Blender reads u = 0.5 - atan2(y_b, x_b) / 2 pi and v = 0.5 + elevation / pi (v = 0 at the bottom).
    """
    height = width // 2
    u = (np.arange(width) + 0.5) / width
    v = (np.arange(height) + 0.5) / height
    phi = math.pi - 2 * math.pi * u  # atan2(y_b, x_b)
    el = math.pi * (v - 0.5)
    xb = np.cos(el)[:, None] * np.cos(phi)[None, :]
    yb = np.cos(el)[:, None] * np.sin(phi)[None, :]
    zb = np.broadcast_to(np.sin(el)[:, None], (height, width))
    ours = np.stack([xb, zb, -yb], -1)  # Blender (x, y, z) -> ours (x, z, -y)
    return sky.radiance(ours)  # (height, width, 3), row 0 = bottom


# ------------------------------------------------------------------------------------------------ post (Post.fs + Colour.fs)


def gaussian_kernel(sigma):
    radius = max(1, int(math.ceil(3 * sigma)))
    x = np.arange(-radius, radius + 1, dtype=float)
    k = np.exp(-(x * x) / (2 * sigma * sigma))
    return k / k.sum()


def blur(img, sigma):
    """Separable Gaussian, pixels outside the image count as black (zero padding), like Post.blur."""
    k = gaussian_kernel(sigma)
    r = len(k) // 2
    h, w, _ = img.shape
    pad = np.pad(img, ((0, 0), (r, r), (0, 0)))
    horiz = sum(k[i] * pad[:, i:i + w, :] for i in range(len(k)))
    pad = np.pad(horiz, ((r, r), (0, 0), (0, 0)))
    return sum(k[i] * pad[i:i + h, :, :] for i in range(len(k)))


def luminance(img):
    return 0.2126 * img[..., 0] + 0.7152 * img[..., 1] + 0.0722 * img[..., 2]


def post_ours(img, a):
    """Post.apply: bloom, exposure * white balance, saturation, vignette. img: (h, w, 3) linear, row 0 top."""
    h, w, _ = img.shape
    out = img.copy()
    if a.post == "ours" and a.bloom > 0:
        l = luminance(img)
        keep = np.where(l > a.bloom_threshold, (l - a.bloom_threshold) / np.maximum(l, 1e-300), 0)
        bright = img * keep[..., None]
        glow = np.zeros_like(img)
        for sigma, weight in [(0.004, 0.3), (0.012, 0.35), (0.035, 0.35)]:
            glow += weight * blur(bright, max(0.75, sigma * h))
        out = out + a.bloom * glow
    wb = a.white_balance if a.post == "ours" else 0.0
    gains = np.array([math.exp(0.25 * wb), 1.0, math.exp(-0.25 * wb)])
    out = out * a.exposure * gains
    if a.post == "ours":
        l = luminance(out)[..., None]
        out = np.maximum(0, l + a.saturation * (out - l))
        cx, cy = (w - 1) / 2, (h - 1) / 2
        corner2 = max(1e-9, cx * cx + cy * cy)
        yy, xx = np.mgrid[0:h, 0:w]
        falloff = 1 - a.vignette * (((xx - cx) ** 2 + (yy - cy) ** 2) / corner2)
        out = out * falloff[..., None]
    return out


def encode_srgb(x):
    return np.where(x <= 0.0031308, 12.92 * x, 1.055 * np.power(np.maximum(x, 0), 1 / 2.4) - 0.055)


def display(img, transfer):
    """Colour.ToDisplayColor, returning uint8 (truncation like `int (... * 255.)`)."""
    if transfer == "aces":
        x = np.maximum(0, img)
        e = encode_srgb(np.clip((x * (2.51 * x + 0.03)) / (x * (2.43 * x + 0.59) + 0.14), 0, 1))
    elif transfer == "srgb":
        v = np.minimum(1, img)
        e = np.where(v == 1, 1, encode_srgb(v))
    elif transfer == "gamma2":
        e = np.sqrt(np.clip(img, 0, 1))
    else:
        e = np.minimum(1, img)
    return (np.clip(e, 0, 1) * 255).astype(np.uint8)


def extract_sun(px, max_angle_deg=5.0, threshold=None):
    """HdrEnvironment.extractSun (HdrImage.fs): from the brightest pixel, flood-fill (8-connected, wrapping
    horizontally) the pixels brighter than the level and within `max_angle_deg` of the peak, and scale each
    to luminance = level. Level: `threshold`, else max(peak / 1000, 50 * median). px: (h, w, 3), row 0 at
    the top. Returns (clamped copy, info)."""
    h, w, _ = px.shape
    lum = 0.2126 * px[..., 0] + 0.7152 * px[..., 1] + 0.0722 * px[..., 2]
    peak_index = int(np.argmax(lum))
    peak = float(lum.flat[peak_index])
    level = threshold if threshold is not None else max(peak / 1000.0, 50.0 * float(np.median(lum)))
    # Lat-long directions (any consistent convention: only angles between pixels matter here).
    v = (np.arange(h) + 0.5) / h * math.pi
    u = (np.arange(w) + 0.5) / w * 2 * math.pi

    def direction(k):
        j, i = divmod(k, w)
        return np.array([math.sin(v[j]) * math.cos(u[i]), math.cos(v[j]), math.sin(v[j]) * math.sin(u[i])])

    peak_dir = direction(peak_index)
    cos_limit = math.cos(math.radians(max_angle_deg))
    out = px.copy()
    count = 0
    if peak > level:
        stack, visited = [peak_index], {peak_index}
        while stack:
            k = stack.pop()
            j, i = divmod(k, w)
            out[j, i] *= level / lum[j, i]
            count += 1
            for dj in (-1, 0, 1):
                jj = j + dj
                if 0 <= jj < h:
                    for di in (-1, 0, 1):
                        n = jj * w + (i + di) % w
                        if n not in visited and lum.flat[n] > level and direction(n) @ peak_dir >= cos_limit:
                            visited.add(n)
                            stack.append(n)
    return out, {"peak": peak, "level": level, "pixels": count}


def write_hdr(path, rgb_rows_bottom_up):
    """Radiance RGBE (.hdr), flat scanlines. (Blender's Image.save() sRGB-encodes a generated float image even
    as EXR, so the sky is written here instead.)"""
    rgb = np.ascontiguousarray(rgb_rows_bottom_up[::-1], dtype=np.float64)
    h, w, _ = rgb.shape
    m = rgb.max(-1)
    mant, exp = np.frexp(m)
    scale = np.where(m > 1e-32, mant * 256.0 / np.maximum(m, 1e-300), 0)
    rgbe = np.zeros((h, w, 4), np.uint8)
    rgbe[..., :3] = np.clip(np.floor(rgb * scale[..., None]), 0, 255)
    rgbe[..., 3] = np.where(m > 1e-32, exp + 128, 0)
    with open(path, "wb") as f:
        f.write(f"#?RADIANCE\nFORMAT=32-bit_rle_rgbe\n\n-Y {h} +X {w}\n".encode())
        f.write(rgbe.tobytes())


def write_png(path, rgb8):
    h, w, _ = rgb8.shape
    raw = b"".join(b"\x00" + rgb8[y].tobytes() for y in range(h))

    def chunk(tag, data):
        return struct.pack(">I", len(data)) + tag + data + struct.pack(">I", zlib.crc32(tag + data) & 0xFFFFFFFF)

    with open(path, "wb") as f:
        f.write(b"\x89PNG\r\n\x1a\n" + chunk(b"IHDR", struct.pack(">IIBBBBB", w, h, 8, 2, 0, 0, 0))
                + chunk(b"IDAT", zlib.compress(raw, 6)) + chunk(b"IEND", b""))


# ------------------------------------------------------------------------------------------------ calibration glTF (plain python)

CALIB = {
    # Plane 4 x 4 m at y = 0, albedo 0.8, roughness 1. Camera 3 m above, looking straight down.
    "albedo": 0.8,
    "sun_irradiance": 2.0,  # head-on
    "lamp_intensity": 2.0,  # W/sr, 1 m above the plane centre
    "lamp_height": 1.0,
    "camera_height": 3.0,
    "yfov": 0.5,
}


def write_calib_gltf(path, kind):
    """The calibration scene as glTF for our renderer (`--scene`). Our importer scales a directional light's
    lux by DirectionalLightScale = 0.3, so render with --light-scale 3.3333333 to get E = intensity."""
    s = 2.0
    pos = np.array([[-s, 0, -s], [s, 0, -s], [s, 0, s], [-s, 0, s]], dtype=np.float32)
    nrm = np.array([[0, 1, 0]] * 4, dtype=np.float32)
    idx = np.array([0, 2, 1, 0, 3, 2], dtype=np.uint16)  # counter-clockwise seen from +Y
    blob = pos.tobytes() + nrm.tobytes() + idx.tobytes()
    import base64
    q = [-math.sin(math.pi / 4), 0, 0, math.cos(math.pi / 4)]  # local -Z -> world -Y
    light = ({"type": "directional", "intensity": CALIB["sun_irradiance"], "color": [1, 1, 1]} if kind == "sun"
             else {"type": "point", "intensity": CALIB["lamp_intensity"], "color": [1, 1, 1]})
    light_node = ({"name": "light", "rotation": q, "extensions": {"KHR_lights_punctual": {"light": 0}}} if kind == "sun"
                  else {"name": "light", "translation": [0, CALIB["lamp_height"], 0], "extensions": {"KHR_lights_punctual": {"light": 0}}})
    a = CALIB["albedo"]
    doc = {
        "asset": {"version": "2.0"},
        "extensionsUsed": ["KHR_lights_punctual"],
        "extensions": {"KHR_lights_punctual": {"lights": [light]}},
        "scene": 0,
        "scenes": [{"nodes": [0, 1, 2]}],
        "nodes": [{"name": "plane", "mesh": 0},
                  {"name": "camera", "camera": 0, "translation": [0, CALIB["camera_height"], 0], "rotation": q},
                  light_node],
        "cameras": [{"type": "perspective", "perspective": {"yfov": CALIB["yfov"], "znear": 0.01, "aspectRatio": 1.0}}],
        "meshes": [{"primitives": [{"attributes": {"POSITION": 0, "NORMAL": 1}, "indices": 2, "material": 0}]}],
        "materials": [{"pbrMetallicRoughness": {"baseColorFactor": [a, a, a, 1], "metallicFactor": 0, "roughnessFactor": 1}}],
        "buffers": [{"byteLength": len(blob), "uri": "data:application/octet-stream;base64," + base64.b64encode(blob).decode()}],
        "bufferViews": [{"buffer": 0, "byteOffset": 0, "byteLength": 48},
                        {"buffer": 0, "byteOffset": 48, "byteLength": 48},
                        {"buffer": 0, "byteOffset": 96, "byteLength": 12}],
        "accessors": [{"bufferView": 0, "componentType": 5126, "count": 4, "type": "VEC3", "min": [-s, 0, -s], "max": [s, 0, s]},
                      {"bufferView": 1, "componentType": 5126, "count": 4, "type": "VEC3"},
                      {"bufferView": 2, "componentType": 5123, "count": 6, "type": "SCALAR"}],
    }
    os.makedirs(os.path.dirname(os.path.abspath(path)), exist_ok=True)
    with open(path, "w") as f:
        json.dump(doc, f)
    print("wrote", path)


# ------------------------------------------------------------------------------------------------ Blender scene


def blender_main(a):
    import bpy
    import mathutils

    t0 = time.time()
    width, height = (int(x) for x in a.res.lower().split("x"))
    out_png = os.path.abspath(a.out)
    base = os.path.splitext(out_png)[0]
    os.makedirs(os.path.dirname(out_png), exist_ok=True)
    info = {"args": vars(a)}

    def clear_scene():
        bpy.ops.wm.read_factory_settings(use_empty=True)

    def remove_lights_and_cameras():
        for o in list(bpy.data.objects):
            if o.type in ("LIGHT", "CAMERA"):
                bpy.data.objects.remove(o, do_unlink=True)

    def import_file(path):
        ext = os.path.splitext(path)[1].lower()
        if ext in (".gltf", ".glb"):
            bpy.ops.import_scene.gltf(filepath=path, import_pack_images=False)
        elif ext in (".usd", ".usda", ".usdc", ".usdz"):
            bpy.ops.wm.usd_import(filepath=path)
        elif ext == ".fbx":
            bpy.ops.import_scene.fbx(filepath=path)
        else:
            raise SystemExit(f"Don't know how to import {path} (glTF/glb, USD and FBX are supported)")

    def add_sun(direction_ours, irradiance, colour, angle_deg):
        data = bpy.data.lights.new("sun", type="SUN")
        data.energy = irradiance * UNIT_SCALE
        data.color = colour
        data.angle = math.radians(angle_deg)
        obj = bpy.data.objects.new("sun", data)
        bpy.context.scene.collection.objects.link(obj)
        # A sun light shines along its local -Z, so its +Z points towards the sun.
        obj.rotation_mode = "QUATERNION"
        obj.rotation_quaternion = mathutils.Vector(to_blender(direction_ours)).to_track_quat("Z", "Y")
        return obj

    def add_lamp(name, position_ours, intensity, colour, radius):
        data = bpy.data.lights.new(name, type="POINT")
        data.energy = 4 * math.pi * intensity * UNIT_SCALE
        data.color = colour
        data.shadow_soft_size = radius
        obj = bpy.data.objects.new(name, data)
        obj.location = to_blender(position_ours)
        obj.visible_camera = False  # our SphereLight is not geometry: camera rays pass through it
        bpy.context.scene.collection.objects.link(obj)
        return obj

    def add_camera(position_ours, target_ours, yfov, aperture=0.0):
        data = bpy.data.cameras.new("camera")
        data.sensor_fit = "VERTICAL"
        data.sensor_height = 24.0
        data.lens = data.sensor_height / (2 * math.tan(yfov / 2))
        data.clip_start = 0.01
        data.clip_end = 1000
        obj = bpy.data.objects.new("camera", data)
        bpy.context.scene.collection.objects.link(obj)
        p, q = mathutils.Vector(to_blender(position_ours)), mathutils.Vector(to_blender(target_ours))
        obj.location = p
        obj.rotation_mode = "QUATERNION"
        obj.rotation_quaternion = (q - p).to_track_quat("-Z", "Y")
        if aperture > 0:
            # Sponza.cameras: thin lens of radius `aperture` (m) focused on the aim point. Cycles' aperture
            # radius is lens / (2 f-stop) in scene units, with the lens in mm.
            data.dof.use_dof = True
            data.dof.focus_distance = (q - p).length
            data.dof.aperture_fstop = (data.lens * 1e-3) / (2 * aperture)
        bpy.context.scene.camera = obj
        return obj

    def world_background(colour=None, image_path=None, rotation_deg=0.0, strength=1.0):
        world = bpy.data.worlds.new("world")
        bpy.context.scene.world = world
        world.use_nodes = True
        nt = world.node_tree
        bg = nt.nodes["Background"]
        bg.inputs["Strength"].default_value = strength * UNIT_SCALE
        if image_path is None:
            bg.inputs["Color"].default_value = (*(colour or (0, 0, 0)), 1)
            return
        env = nt.nodes.new("ShaderNodeTexEnvironment")
        env.image = bpy.data.images.load(image_path, check_existing=True)
        env.image.colorspace_settings.name = "Linear Rec.709"
        env.interpolation = "Linear"
        mapping = nt.nodes.new("ShaderNodeMapping")
        mapping.vector_type = "POINT"
        mapping.inputs["Rotation"].default_value = (0, 0, math.radians(rotation_deg))
        coord = nt.nodes.new("ShaderNodeTexCoord")
        nt.links.new(coord.outputs["Generated"], mapping.inputs["Vector"])
        nt.links.new(mapping.outputs["Vector"], env.inputs["Vector"])
        nt.links.new(env.outputs["Color"], bg.inputs["Color"])
        # Sample the world as a light with a map big enough for a small sun.
        world.cycles.sampling_method = "MANUAL"
        world.cycles.sample_map_resolution = 2048

    def import_knight_cache(path, anim_time, position, yaw_deg):
        """The knight's vertex cache (scripts/sponza/convert-knight.py, Knight.fs) posed at `anim_time` seconds
        (linear between cache frames, clamped), placed like Knight.placeAt (origin at `position`, yaw about +Y)."""
        with open(path, "rb") as f:
            if f.read(8) != b"KNCACHE1":
                raise SystemExit(f"{path} is not a knight cache")
            n = struct.unpack("<i", f.read(4))[0]
            header = json.loads(f.read(n))
            f.read((4 - n % 4) % 4)
            groups = []
            for g in header["groups"]:
                nv, nt = g["vertices"], g["triangles"]
                src = np.frombuffer(f.read(4 * nv), "<i4")
                uv = np.frombuffer(f.read(8 * nv), "<f4").reshape(nv, 2)
                tri = np.frombuffer(f.read(12 * nt), "<i4").reshape(nt, 3)
                groups.append((g["material"], src, uv, tri))
            frames, points = header["frames"], header["points"]
            positions = np.frombuffer(f.read(12 * frames * points), "<f4").reshape(frames, points, 3)
        fpos = min(max(anim_time * header["fps"], 0.0), frames - 1.0)
        i = min(frames - 2, int(math.floor(fpos)))
        w = fpos - i
        pose = positions[i] * (1 - w) + positions[i + 1] * w
        yaw = math.radians(yaw_deg)
        c, s = math.cos(yaw), math.sin(yaw)
        x, y, z = pose[:, 0], pose[:, 1], pose[:, 2]
        world = np.stack([c * x + s * z + position[0], y + position[1], -s * x + c * z + position[2]], -1)
        blender_pts = np.stack([world[:, 0], -world[:, 2], world[:, 1]], -1)
        base_dir = os.path.dirname(path)

        def material(m):
            mat = bpy.data.materials.new("knight:" + m["name"])
            mat.use_nodes = True
            nt = mat.node_tree
            bsdf = nt.nodes["Principled BSDF"]
            bsdf.inputs["Base Color"].default_value = (*m["baseColour"], 1)
            bsdf.inputs["Metallic"].default_value = m["metallic"]
            bsdf.inputs["Roughness"].default_value = m["roughness"]
            bsdf.inputs["IOR"].default_value = m.get("ior", 1.5)

            def tex(rel, colour):
                node = nt.nodes.new("ShaderNodeTexImage")
                node.image = bpy.data.images.load(os.path.normpath(os.path.join(base_dir, rel)), check_existing=True)
                node.image.colorspace_settings.name = "sRGB" if colour else "Non-Color"
                return node

            if m.get("baseColourMap"):
                t = tex(m["baseColourMap"], True)
                mix = nt.nodes.new("ShaderNodeMix")
                mix.data_type, mix.blend_type = "RGBA", "MULTIPLY"
                mix.inputs["Factor"].default_value = 1
                mix.inputs[6].default_value = (*m["baseColour"], 1)
                nt.links.new(t.outputs["Color"], mix.inputs[7])
                nt.links.new(mix.outputs[2], bsdf.inputs["Base Color"])
            if m.get("roughnessMap"):  # Knight.fs: roughness = the map's red channel
                t = tex(m["roughnessMap"], False)
                sep = nt.nodes.new("ShaderNodeSeparateColor")
                nt.links.new(t.outputs["Color"], sep.inputs["Color"])
                nt.links.new(sep.outputs["Red"], bsdf.inputs["Roughness"])
            if m.get("normalMap"):
                t = tex(m["normalMap"], False)
                nm = nt.nodes.new("ShaderNodeNormalMap")
                nt.links.new(t.outputs["Color"], nm.inputs["Color"])
                nt.links.new(nm.outputs["Normal"], bsdf.inputs["Normal"])
            return mat

        mats = [material(m) for m in header["materials"]]
        for gi, (mi, src, uv, tri) in enumerate(groups):
            mesh = bpy.data.meshes.new(f"knight{gi}")
            # One vertex per source point (not per render vertex), so normals stay smooth across UV seams
            # like Knight.normalsOf.
            unique, inverse = np.unique(src, return_inverse=True)
            verts = blender_pts[unique]
            mesh.vertices.add(len(verts))
            mesh.vertices.foreach_set("co", verts.astype(np.float32).ravel())
            mesh.loops.add(tri.size)
            mesh.loops.foreach_set("vertex_index", inverse[tri].astype(np.int32).ravel())
            mesh.polygons.add(len(tri))
            mesh.polygons.foreach_set("loop_start", np.arange(0, tri.size, 3, dtype=np.int32))
            mesh.polygons.foreach_set("loop_total", np.full(len(tri), 3, dtype=np.int32))
            layer = mesh.uv_layers.new(name="UVMap")
            layer.data.foreach_set("uv", uv[tri.ravel()].astype(np.float32).ravel())
            mesh.update()
            mesh.validate()
            mesh.polygons.foreach_set("use_smooth", np.ones(len(tri), dtype=bool))
            mesh.materials.append(mats[mi])
            obj = bpy.data.objects.new(f"knight{gi}", mesh)
            bpy.context.scene.collection.objects.link(obj)
        print(f"[gt] knight posed at animation time {anim_time:.3f}s ({len(groups)} meshes)")

    # -------------------------------------------------------------- geometry
    if a.calib:
        clear_scene()
        gltf = base + "-scene.gltf"
        write_calib_gltf(gltf, a.calib)
        bpy.ops.import_scene.gltf(filepath=gltf)
        remove_lights_and_cameras()
        add_camera((0, CALIB["camera_height"], 0), (0, 0, 0.0001), CALIB["yfov"])  # tiny offset: avoid a degenerate up
        if a.calib == "sun":
            add_sun((0, 1, 0), CALIB["sun_irradiance"], (1, 1, 1), a.sun_angle)
        else:
            add_lamp("lamp", (0, CALIB["lamp_height"], 0), CALIB["lamp_intensity"], (1, 1, 1), 0.0)
        world_background((0, 0, 0))
        a.post = "none"
        a.exposure = 1.0 if a.exposure is None else a.exposure
        a.transfer = "linear"
        spec = None
    else:
        spec_path = a.json or search_up(os.path.join("scenes", "sponza.json"))
        with open(spec_path) as f:
            spec = json.load(f)
        info["spec"] = spec_path
        if a.exposure is None:
            a.exposure = float(spec.get("exposure", 3.0))
        parts = [asset(spec["parts"][name]) for name in spec["include"]] + [os.path.abspath(e) for e in a.extra]
        key = hashlib.sha1(json.dumps([(p, os.path.getmtime(p)) for p in parts]).encode()).hexdigest()[:12]
        cache_dir = os.path.join(repo_root(), "artifacts", "ground-truth", "cache")
        os.makedirs(cache_dir, exist_ok=True)
        cache = os.path.join(cache_dir, f"parts-{key}.blend")
        if os.path.exists(cache) and not a.no_cache:
            bpy.ops.wm.open_mainfile(filepath=cache)
            print(f"[gt] opened cached geometry {cache} ({time.time() - t0:.1f}s)")
        else:
            clear_scene()
            for p in parts:
                t1 = time.time()
                import_file(p)
                print(f"[gt] imported {p} ({time.time() - t1:.1f}s)")
            remove_lights_and_cameras()
            bpy.ops.wm.save_as_mainfile(filepath=cache, compress=False)
            print(f"[gt] cached geometry to {cache}")
        remove_lights_and_cameras()
        for w in list(bpy.data.worlds):
            bpy.data.worlds.remove(w)

        prefixes = [x for x in a.force_opaque.split(",") if x]
        for mat in bpy.data.materials:
            if mat.use_nodes and any(mat.name.startswith(x) for x in prefixes):
                for node in mat.node_tree.nodes:
                    if node.type == "BSDF_PRINCIPLED":
                        for link in list(node.inputs["Alpha"].links):
                            mat.node_tree.links.remove(link)
                        node.inputs["Alpha"].default_value = 1.0
                print(f"[gt] forced opaque: {mat.name}")

        # ---------------------------------------------------------- lights
        sun = spec["sun"]
        towards = sun_direction(spec)
        add_sun(towards, sun["irradiance"], tuple(sun["colour"]), a.sun_angle)
        sky = spec.get("sky", {})
        mode = a.sky
        if mode == "auto":
            mode = sky.get("model")
            if mode is None:
                sponza_fs = search_up(os.path.join("RayTracer.Animation", "Sponza.fs"))
                text = open(sponza_fs).read() if sponza_fs else ""
                # Sponza.fs lights with the HDRI whenever the json names one (Sky.light is its fallback).
                uses_hdri = bool(sky.get("hdri")) and "HdrEnvironment" in text
                mode = "hdri" if uses_hdri else "preetham"
        info["sky"] = mode
        if mode == "preetham":
            model = Preetham(towards, sky.get("turbidity", 2.5), sun["irradiance"], sky.get("sunToSky"))
            sky_exr = os.path.join(cache_dir, f"preetham-{hashlib.sha1(json.dumps([towards, sky, sun]).encode()).hexdigest()[:12]}.hdr")
            if not os.path.exists(sky_exr):
                write_hdr(sky_exr, bake_equirect(model))
            world_background(image_path=sky_exr, rotation_deg=0.0, strength=1.0)
        elif mode == "hdri":
            # Sponza.lights: the HDRI's own sun is clamped out (extractSun, auto level, 5 degrees) for lighting
            # and camera rays alike; the json sun is the only sun.
            source = asset(sky["hdri"])
            clamped = os.path.join(cache_dir, "hdri-%s.hdr" % hashlib.sha1(
                json.dumps([source, os.path.getmtime(source), "extractSun-auto-5"]).encode()).hexdigest()[:12])
            if not os.path.exists(clamped) and not a.hdri_keep_sun:
                img = bpy.data.images.load(source)
                iw, ih = img.size
                buf = np.empty(iw * ih * 4, np.float32)
                img.pixels.foreach_get(buf)
                px = buf.reshape(ih, iw, 4)[::-1, :, :3].astype(np.float64)
                out, sun_info = extract_sun(px, 5.0)
                print(f"[gt] HDRI sun clamped: {sun_info}")
                write_hdr(clamped, out[::-1])
                bpy.data.images.remove(img)
            world_background(image_path=source if a.hdri_keep_sun else clamped,
                             rotation_deg=sky.get("rotationDeg", 0.0), strength=sky.get("intensity", 1.0))
        else:
            world_background((0, 0, 0))

        lamps = spec.get("lamps", {})
        lamps_on = lamps.get("enabled", False) if a.lamps == "auto" else a.lamps == "on"
        if lamps_on and "main" in spec["include"]:
            positions = gltf_node_world_positions(asset(spec["parts"]["main"]), "lamp_light")
            offset = lamps.get("offset", [0.0, 0.0, 0.0])
            for i, pos in enumerate(positions):
                add_lamp(f"lamp{i}", [pos[c] + offset[c] for c in range(3)], lamps["intensity"],
                         tuple(lamps["colour"]), lamps["radius"])
            info["lamps"] = len(positions)

        # ---------------------------------------------------------- camera
        shot, pos, target, yfov, aperture = camera_at(spec, a.time)
        add_camera(pos, target, yfov, aperture)
        knight = spec.get("knight")
        if a.knight:
            knight = dict(knight or {}, cache=a.knight, enabled=True)
        if knight and knight.get("enabled", True):
            knight.setdefault("cache", "knight/knight.cache")
            # Knight.fs: placeAt position yaw, clock start speed (animation time = (t - start) * speed).
            import_knight_cache(asset(knight["cache"]) if not os.path.isabs(knight["cache"]) else knight["cache"],
                                (a.time - knight.get("start", 0.0)) * knight.get("speed", 1.0),
                                knight.get("position", [0, 0, 0]), knight.get("yawDeg", 0.0))
            info["knight"] = knight
        info["camera"] = {"shot": shot, "position": pos, "target": target, "yfovDeg": math.degrees(yfov), "aperture": aperture}
        print(f"[gt] t={a.time}: {shot} at {pos} -> {target}, yfov {math.degrees(yfov):.1f}")

    # -------------------------------------------------------------- render settings
    scene = bpy.context.scene
    scene.render.engine = "CYCLES"
    prefs = bpy.context.preferences.addons["cycles"].preferences
    use_gpu = a.gpus != "cpu"
    if use_gpu:
        prefs.compute_device_type = "OPTIX"
        prefs.refresh_devices()
        free = {}
        try:
            import subprocess
            q = subprocess.run(["nvidia-smi", "--query-gpu=index,pci.bus_id,memory.free", "--format=csv,noheader,nounits"],
                               capture_output=True, text=True).stdout
            for line in q.strip().splitlines():
                i, bus, mb = [x.strip() for x in line.split(",")]
                free[bus[-12:-2].lower()] = (int(i), int(mb))  # "0000:41:00"
        except Exception:
            pass
        chosen = []
        for d in prefs.devices:
            d.use = False
            if d.type != "OPTIX":
                continue
            bus = d.id.split("_")[2].lower() if d.id.count("_") >= 3 else ""
            index, mb = free.get(bus, (-1, 10 ** 9))
            if a.gpus == "all" or (a.gpus == "auto" and mb >= 20000) or (a.gpus not in ("all", "auto") and str(index) in a.gpus.split(",")):
                d.use = True
                chosen.append((index, mb))
        if not chosen:  # nothing qualified: take the device with the most free memory
            best = max((d for d in prefs.devices if d.type == "OPTIX"), key=lambda d: free.get(d.id.split("_")[2].lower(), (0, 0))[1], default=None)
            if best is not None:
                best.use = True
                chosen.append(("best", 0))
        use_gpu = bool(chosen)
        print(f"[gt] GPUs: {chosen}")
    scene.cycles.device = "GPU" if use_gpu else "CPU"
    scene.cycles.samples = a.spp
    scene.cycles.use_adaptive_sampling = False
    scene.cycles.use_denoising = not a.no_denoise
    scene.cycles.denoiser = "OPENIMAGEDENOISE"
    scene.cycles.denoising_input_passes = "RGB_ALBEDO_NORMAL"
    scene.cycles.max_bounces = a.bounces
    scene.cycles.diffuse_bounces = a.bounces
    scene.cycles.glossy_bounces = a.bounces
    scene.cycles.transmission_bounces = a.bounces
    scene.cycles.volume_bounces = 0
    scene.cycles.transparent_max_bounces = 64  # alpha-cut leaves and curtains
    scene.cycles.sample_clamp_direct = 0
    scene.cycles.sample_clamp_indirect = a.clamp
    scene.cycles.seed = 2026
    scene.render.resolution_x, scene.render.resolution_y = width, height
    scene.render.resolution_percentage = 100
    scene.render.film_transparent = False
    scene.render.use_motion_blur = False
    scene.view_settings.view_transform = "Standard"
    scene.view_settings.look = "None"
    scene.view_settings.exposure = 0
    scene.view_settings.gamma = 1
    scene.render.image_settings.file_format = "OPEN_EXR"
    scene.render.image_settings.color_depth = "32"
    scene.render.image_settings.exr_codec = "ZIP"
    scene.render.filepath = base + ".exr"

    t1 = time.time()
    bpy.ops.render.render(write_still=True)
    info["renderSeconds"] = round(time.time() - t1, 1)
    print(f"[gt] rendered {base}.exr in {info['renderSeconds']}s")

    # -------------------------------------------------------------- tone map like ours
    img = bpy.data.images.load(base + ".exr")
    buf = np.empty(width * height * 4, dtype=np.float32)
    img.pixels.foreach_get(buf)
    linear = buf.reshape(height, width, 4)[::-1, :, :3].astype(np.float64)  # row 0 = top
    np.save(base + "-linear.npy", linear.astype(np.float32))
    write_png(out_png, display(post_ours(linear, a), a.transfer))
    stats = {"meanLinear": linear.reshape(-1, 3).mean(0).round(5).tolist(),
             "centreLinear": linear[height // 2 - 2:height // 2 + 2, width // 2 - 2:width // 2 + 2].reshape(-1, 3).mean(0).round(5).tolist()}
    info.update(stats)
    info["totalSeconds"] = round(time.time() - t0, 1)
    with open(base + ".json", "w") as f:
        json.dump(info, f, indent=1)
    print(f"[gt] wrote {out_png}; {stats}")


def main():
    a = parse_args(script_argv())
    if a.write_calib_gltf:
        write_calib_gltf(a.write_calib_gltf, a.calib or "sun")
        return
    try:
        import bpy  # noqa: F401
    except ImportError:
        raise SystemExit("Run inside Blender: blender -b --python scripts/sponza/cycles_reference.py -- ...")
    blender_main(a)


if __name__ == "__main__":
    main()
