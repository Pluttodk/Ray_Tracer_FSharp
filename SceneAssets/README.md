# Original benchmark scenes

This **F# executable** creates original assets locally. It does not download,
trace, scan, or import a character, furniture model, image, or font. The existing
unprovenanced `ply/` and `textures/` directories are not inputs.

## Generate once, render separately

From the repository root, after the normal .NET dependency restore/build:

```sh
dotnet run --project SceneAssets -c Release -- generate
dotnet run --project SceneAssets -c Release --no-build -- verify
```

On the implementation machine, the host is `/home/movj/.dotnet/dotnet`.
For strictly offline generation after building, use:

```sh
dotnet SceneAssets/bin/Release/net10.0/SceneAssets.dll generate
dotnet SceneAssets/bin/Release/net10.0/SceneAssets.dll verify
```

The seed is fixed at **2026**, with explicitly implemented integer-hash noise.
There are no time-dependent values or runtime-random identifiers in the assets
or manifest. ASCII geometry is rounded to nine fractional digits, serialized
with invariant culture and LF endings. Image encoding uses the project's
managed, pinned `RayTracer.ImageIO` library; there is no second image codec.
Repeated generation leaves byte-identical asset files in place rather than
rewriting them when only source/provenance metadata needs refreshing.

Outputs:

- `benchmarks/scenes/{chair,roman-bust,space-sentinel,sky-arena}.json` — the
  four required scene IDs. These renderer-neutral scene files are tracked.
- `benchmarks/scenes/material-lab.json` — an additional direct-light box.
- `benchmarks/scenes/provenance.json` — per-asset authorship, original recipe,
  license, file hash, topology, texture interpretation, and generator hashes.
- `artifacts/scene-assets/meshes/*.ply` and `textures/*.png` — ignored,
  reproducible generated assets. JSON paths resolve relative to each JSON file.

Asset preparation and validation run **before** the measured render. The
benchmark runner invokes this generator by default, so mesh/texture baking must
not be added to worker trace timing.

### Separate Gold Dragon bonus

`gold-dragon` is a separately prepared **authored/gold-only** bonus, not an
output of the default original-asset generator. Its externally acquired Stanford scan
must retain independently verified licensing and provenance; neither the
original/procedural authorship claims nor the generated-assets license in
`provenance.json` apply to that scan. Keep its manifest separate from this
generator's canonical inventory. Generation does not remove extra scene JSONs.
Stanford's terms allow attributed noncommercial research/free redistribution;
commercial use requires permission. Keep the optional dataset and its rendered
images ignored and credited, never relicensed under GPL. The
[bonus record](../benchmarks/gold-dragon.json) supplies the scan credit, hash,
terms link and original settings.

The primary `--scenes all` matrix remains exactly the four required IDs
(48 standard/high scene-material cases per engine). Select the bonus separately
with `--scene benchmarks/scenes/gold-dragon.json --materials authored`; it must not enter the uniform
material/glass sweep, because an open scan is not a closed dielectric solid.
Its supplied historical settings, including a 1024x768 camera, sixteen
multi-jittered samples, two bounces, glossy gold, two point lights, environment
light and a blue mirror plane, belong to the bonus workstream. Unsupported
environment lighting on a backend must remain an explicit failure.

After the source-acquisition script has supplied the raw scan,
create the extra scene without downloading or modifying it:

```sh
dotnet fsi scripts/prepare-gold-dragon.fsx
dotnet fsi SceneAssets/GoldDragon.fsx
```

This writes `benchmarks/scenes/gold-dragon.json` using the pinned raw scan with
`smooth=false`, `closed=false`, exact historical material colours/coefficients,
transform, camera and lights. Its separately generated GPL-licensed staging
slab is a **finite 200000x200000 approximation** with top surface `y=0`.
Its license, topology, source hashes and transform are recorded separately in
`artifacts/scene-assets/gold-dragon/staging-plane.provenance.json`.
The script also writes a complete `RenderSettings` input at
`artifacts/scene-assets/gold-dragon/original-settings.json`, with the original
resolution/sample counts and the local runtime's processor count.
The restored `renderGoldDragon` function remains the authoritative
infinite-plane version. This optional script is not called by `SceneAssets
generate`, does not add the scan to `provenance.json`, and leaves all four
primary scene files unchanged.

## Compositions

| ID | Original design |
| --- | --- |
| `chair` | **Arcback**: walnut splayed legs, stretchers, brass join pins, bowed bentwood rails, five curved slats, teal woven upholstery and matching piping on a low studio dais. |
| `roman-bust` | **An imagined orator**: a sculpted anatomical head with indented sockets, separate eyelids/irises, modeled nasal bridge and nostrils, lips, ear helices, spiral curls, bronze laurel, shoulders and a folded stone toga on a moulded plinth. It is neither a scan nor a claimed historical portrait. |
| `space-sentinel` | **Vesper Sentinel**: an original elongated faceted helmet with an offset fin, broken-chevron visor, ungrilled asymmetric mask, copper-edged chest/shoulder armor and purple cape. No licensed costume or character model is used. |
| `sky-arena` | **Aster at the suspended arena**: a teal/tangerine martial-arts courier with a planted leg, lifted bent knee, open palm, raised fist, swept short hair, wraps, boots and flowing scarf; floating rocks, a ruined arc, blue sky/cloud staging and cyan/gold energy ornaments. |
| `material-lab` | A Cornell-style box with independently colored walls, glass/mirror/clay spheres, a glossy block, a polished sphere and explicit point plus rectangle lights. |

The descriptions deliberately distinguish these original procedural designs
from third-party scans and familiar film/anime characters. They are
stylizations, not anatomical, textile, or physical-fluid simulations.

### Lighting and material matrix

All cameras are square pinholes (`viewDistance=1`, view size
`2*tan(fov/2)`). Resolution, samples and bounce count come from the shared
quick/standard/high presets, not from alternate scene geometry.

The five uniform material variants override every ID in `subjects`; `authored`
preserves the original wood, fabric, stone, alloy, skin and accent regions.
Floors, plinths, arena scenery and lights remain fixed. All geometry is indexed
and repeated components share mesh files.

The scenes use explicit constant-intensity point lights, plus directional
lights whose direction points **from the surface toward the light**. The
diagnostic rectangle's direction is its **outward emitter normal**. They do
not rely on the frozen legacy area-light defect for their main illumination.
Ambient light is an explicit classic-renderer fill term, not global
illumination.

Visors, energy ornaments and reflection cards have emissive materials for
direct visibility/reflection. They do **not** magically light other surfaces:
where illumination is wanted, a separate explicit light supplies it. Cloud
forms are opaque modeled staging, not volume scattering. No indirect color
bleeding or path tracing is claimed.
The blue sky panel uses an ambient-only matte paint, not an emitter or
environment light. This prevents hard rock shadows on the illustrated sky
and avoids making the frozen renderer's per-light emission overcount dominate
the whole background. The energy-accent point light is outside the opaque
energy orb so the orb cannot enclose and block its illumination.

### Texture convention

The six 512×512 PNGs are original procedural **modulation masks**, not
photographs. Recipes cover wood growth bands/end grain, stone veining, woven
yarn, basalt and brushed alloy. Their light colors are intentional: the
shared adapter decodes gamma 2, then multiplies the authored material's linear
base/ambient color by the texel. This retains the same texture pixels and
interpretation in legacy, CPU and GPU inputs. These are not sRGB albedo images.

PNG rows run top to bottom. PLY UVs use `v=0` at the bottom and `v=1` at the
top. The shared benchmark adapter rejects non-finite coordinates and clamps
each UV component to `[0,1]` before nearest sampling:
`x=min(width-1,int(u*width))`, `y=min(height-1,int((1-v)*height))`.
Clamping prevents tiny CPU/GPU rounding differences near `v=1` from wrapping
to opposite image edges. Only base/ambient colours are modulated; specular
and reflection colours are unchanged.

Mesh seams share indices rather than duplicating vertices; this keeps topology
closed. Seam-adjacent triangles therefore interpolate across the texture chart
seam, a documented tradeoff for this legacy-compatible indexed format.
Procedural low-contrast masks limit the visible discontinuity.

### Solid topology and verification

Every mesh is a connected, closed, consistently outward-wound solid, including
cloth, caps, visor pieces and thin inlays. Rings/piping are genus-one solids;
all other reusable parts are genus zero. Sculptures are compositions of
individually closed, sometimes intersecting solids, **not** a Boolean-unioned
single dielectric shell. The glass variant exercises those component
interfaces; it should not be described as a single cast-glass statue.

`verify` checks the **serialized** PLY, not just construction-time data:

- finite positions, unit normals, finite in-range UVs and valid indices;
- nondegenerate triangles and outward-facing shading normals;
- exactly two oppositely directed uses of every edge;
- connected incident-face fans at every vertex and one mesh component;
- positive signed volume, bounds, counts and manifest agreement;
- orientation-preserving affine transforms, including axis-aligned segments;
- every transformed subject vertex inside its square camera with a margin;
- all six material variants using `SceneFiles.validate`;
- mesh/texture/scene/source hashes and exact regenerated PLY/PNG bytes;
- complete canonical inventories and scene references confined to manifested original assets;
- PNG decode/encode round trips and negative tests for malformed/open solids.

The PLY contract is triangulated ASCII with properties
`x y z nx ny nz u v`, followed by
`property list uchar int vertex_indices`. No external converter is needed.
Recipe construction keeps individual primitive/sweep/loft surfaces simple;
the verifier is a manifold/winding validator, not a general self-intersection
or Boolean-solid solver.

After the legacy library has been built, a separate compatibility check invokes
its **unchanged PLY parser** on every generated mesh:

```sh
dotnet fsi SceneAssets/CheckLegacyPly.fsx
```

This checks element counts, triangle indices, positions and all normal/UV
properties through the frozen reader rather than relying on the generator's
own validator.

## Inspecting render previews

The scene geometry and framing have been checked in actual CPU and
`legacy-port` renders, not just projected bounds. Reproduce authored previews
through the real benchmark workers:

```sh
dotnet fsi scripts/benchmark.fsx -- \
  --presets quick --scenes all --materials authored --engines cpu,legacy \
  --warmups 0 --repeats 1 --threads 4 \
  --output artifacts/scene-assets/previews/authored
```

For the twenty uniform-material geometry checks:

```sh
dotnet fsi scripts/benchmark.fsx -- \
  --presets quick --scenes all --materials matte,phong,mirror,glossy,glass \
  --engines cpu --max-bounces 4 --warmups 0 --repeats 1 --threads 4 \
  --output artifacts/scene-assets/previews/uniform-materials
```

The runner writes a local `index.html`, images, linear data and metrics. These
small runs are **asset acceptance previews**, not the approved standard/high
performance workload. Do not cite timings from a run whose source-change
checks indicate that other work was occurring during the invocation.

## License and provenance

The new recipes, scene compositions, generated geometry and original texture
pixels are explicitly offered under **GPL-2.0-only**, consistent with the
project's existing GPL version 2 license. Authorship is
“Ray_Tracer_FSharp contributors (procedural design with Copilot assistance).”
No attribution to, or license assumption about, a bundled third-party asset is
made. Per-asset `sourceUrl` is intentionally empty: there is no external asset
source. The image codec dependencies retain their own audited licenses; see
`RayTracer.ImageIO/README.md`.
