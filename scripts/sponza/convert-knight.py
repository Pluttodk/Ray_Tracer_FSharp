"""Converts Intel's animated Sponza knight (USD with baked per-vertex animation) into a vertex cache.

Run with Blender's Python, which bundles the USD (pxr) and numpy modules:

    blender -b --python scripts/sponza/convert-knight.py -- \
        assets/sponza/pkg_e_knight_anim/knight_USD_PREVIEW_SURFACE_ANIM_002_1.usd assets/sponza/knight/knight.cache

The source has 135 armour pieces (61,666 points, 122k triangles), every one of them point-cached for
frames 1..300 at 24 fps; many pieces deform (straps, cloth, plates bending with the skin), so neither a
skin nor rigid node animation reproduces it. The cache keeps it exactly: it holds, in metres and glTF axes
(+Y up, which USD shares), the positions of every source point at every frame, plus static render
topology grouped by material (triangles over render vertices, each a point + texture coordinate) and the
UsdPreviewSurface material parameters with texture paths relative to the cache. Normals are left to the
loader, which recomputes smooth normals for each pose.

Layout (little endian):
    8 bytes   magic "KNCACHE1"
    int32     JSON header length L, then L bytes of UTF-8 JSON (see `header` below), padded to 4 bytes
    per group: int32 source[vertices], float32 uv[2 * vertices], int32 triangles[3 * triangles]
    float32   positions[frames][points][3]
    float32   markers[frames][markers][3]   (named reference points for cameras: pelvis, head, chest
              front/back, sword tip)
"""
import json, os, struct, sys, time
import numpy as np
from pxr import Usd, UsdGeom, UsdShade

args = sys.argv[sys.argv.index("--") + 1:] if "--" in sys.argv else []
if len(args) != 2:
    sys.exit("usage: blender -b --python convert-knight.py -- <knight.usd> <out.cache>")
source, target = os.path.abspath(args[0]), os.path.abspath(args[1])
os.makedirs(os.path.dirname(target), exist_ok=True)
t0 = time.time()
stage = Usd.Stage.Open(source)
scale = UsdGeom.GetStageMetersPerUnit(stage)  # 0.01: the knight is authored in centimetres
assert UsdGeom.GetStageUpAxis(stage) == "Y"
first, last = int(stage.GetStartTimeCode()), int(stage.GetEndTimeCode())
fps = stage.GetTimeCodesPerSecond()
frames = list(range(first, last + 1))

# ------------------------------------------------------------------ materials
def shader_inputs(material):
    surface = material.ComputeSurfaceSource()[0]
    if not surface:
        return {}
    out = {}
    for inp in surface.GetInputs():
        if inp.HasConnectedSource():
            src = UsdShade.Shader(inp.GetConnectedSource()[0].GetPrim())
            path = src.GetInput("file").Get()
            channel = str(inp.GetConnectedSource()[1])
            out[inp.GetBaseName()] = {"file": path.resolvedPath or path.path, "channel": channel}
        else:
            out[inp.GetBaseName()] = inp.Get()
    return out

def describe(material):
    name = material.GetPrim().GetName().replace("knight_rig_", "")
    ins = shader_inputs(material)
    rel = lambda e: os.path.relpath(e["file"], os.path.dirname(target)).replace(os.sep, "/")
    m = {"name": name, "baseColour": [0.5, 0.5, 0.5], "baseColourMap": None, "metallic": 0.0,
         "roughness": 0.5, "roughnessMap": None, "normalMap": None, "ior": 1.5}
    d = ins.get("diffuseColor")
    if isinstance(d, dict): m["baseColourMap"] = rel(d); m["baseColour"] = [1.0, 1.0, 1.0]
    elif d is not None: m["baseColour"] = [float(x) for x in d]
    met = ins.get("metallic")
    if met is not None and not isinstance(met, dict): m["metallic"] = float(met)
    r = ins.get("roughness")
    if isinstance(r, dict): m["roughnessMap"] = rel(r)
    elif r is not None: m["roughness"] = float(r)
    if isinstance(ins.get("normal"), dict): m["normalMap"] = rel(ins["normal"])
    if ins.get("ior") is not None: m["ior"] = float(ins["ior"])
    return m

materials, material_index = [], {}
def material_of(prim):
    mat = UsdShade.MaterialBindingAPI(prim).ComputeBoundMaterial()[0]
    key = str(mat.GetPath()) if mat else ""
    if key not in material_index:
        material_index[key] = len(materials)
        materials.append(describe(mat) if mat else {"name": "default", "baseColour": [0.5, 0.5, 0.5], "baseColourMap": None,
                                                   "metallic": 0.0, "roughness": 0.6, "roughnessMap": None, "normalMap": None, "ior": 1.5})
    return material_index[key]

# ------------------------------------------------------------------ topology
meshes = [UsdGeom.Mesh(p) for p in stage.Traverse() if p.GetTypeName() == "Mesh"]
offset = 0
groups = {}  # material -> {"key": {(point, uv): render index}, "source": [], "uv": [], "tri": []}
pieces = {}  # mesh name -> (offset, count)
for mesh in meshes:
    prim = mesh.GetPrim()
    if mesh.ComputeVisibility(1.0) == UsdGeom.Tokens.invisible or mesh.ComputePurpose() not in ("default", "render"):
        print("skipping hidden", prim.GetPath())
        continue
    counts = np.array(mesh.GetFaceVertexCountsAttr().Get(first))
    indices = np.array(mesh.GetFaceVertexIndicesAttr().Get(first))
    npoints = len(mesh.GetPointsAttr().Get(first))
    st = UsdGeom.PrimvarsAPI(prim).GetPrimvar("st")
    uvs = np.array(st.Get(first), dtype=np.float64)
    uv_index = np.array(st.GetIndices(first)) if st.IsIndexed() else np.arange(len(uvs))
    assert st.GetInterpolation() == "faceVarying" and len(uv_index) == len(indices)
    face_material = np.full(len(counts), material_of(prim))
    for subset in UsdGeom.Subset.GetAllGeomSubsets(mesh):
        if subset.GetFamilyNameAttr().Get() in (None, "materialBind"):
            face_material[np.array(subset.GetIndicesAttr().Get())] = material_of(subset.GetPrim())
    starts = np.concatenate([[0], np.cumsum(counts)[:-1]])
    for face, (start, n) in enumerate(zip(starts, counts)):
        g = groups.setdefault(int(face_material[face]), {"key": {}, "source": [], "uv": [], "tri": []})
        corner = []
        for k in range(start, start + n):
            key =(offset + int(indices[k]), tuple(uvs[uv_index[k]]))
            r = g["key"].get(key)
            if r is None:
                r = g["key"][key] = len(g["source"])
                g["source"].append(key[0]); g["uv"].append(key[1])
            corner.append(r)
        for k in range(1, n - 1):  # fan; faces are quads and a few convex n-gons
            g["tri"] += [corner[0], corner[k], corner[k + 1]]
    pieces[prim.GetName().replace("knight_rig_", "")] = (offset, npoints)
    mesh._offset = offset
    offset += npoints
total = offset
print(f"{len(pieces)} pieces, {total} points, {len(materials)} materials, topology in {time.time() - t0:.1f}s")

# ------------------------------------------------------------------ animation
positions = np.zeros((len(frames), total, 3), dtype=np.float32)
for mesh in meshes:
    if not hasattr(mesh, "_offset"):
        continue
    attr = mesh.GetPointsAttr()
    n = len(attr.Get(first))
    world = np.array(UsdGeom.Xformable(mesh.GetPrim()).ComputeLocalToWorldTransform(first), dtype=np.float64)
    for i, f in enumerate(frames):
        p = np.array(attr.Get(float(f)), dtype=np.float64)
        p = p @ world[:3, :3] + world[3, :3]  # USD matrices use row vectors
        positions[i, mesh._offset:mesh._offset + n] = (p * scale).astype(np.float32)
print(f"positions read in {time.time() - t0:.1f}s")

def centroid(name):
    o, n = pieces[name]
    return positions[:, o:o + n].mean(axis=1)

sword_o, sword_n = pieces["Sword"]
pelvis = centroid("Skirtplate_top")
grip = centroid("Hand_R")
sword = positions[:, sword_o:sword_o + sword_n]
tip = sword[np.arange(len(frames)), np.linalg.norm(sword - grip[:, None, :], axis=2).argmax(axis=1)]
markers = {"pelvis": pelvis, "head": centroid("helmet_top"), "chestFront": centroid("Chestplate_outer_front"),
           "chestBack": centroid("Chestplate_outer_back"), "swordTip": tip}
marker_array = np.stack(list(markers.values()), axis=1).astype(np.float32)

lo, hi = positions.min(axis=(0, 1)), positions.max(axis=(0, 1))
lo1, hi1 = positions[0].min(axis=0), positions[0].max(axis=0)
print("frame 1 bounds", lo1.round(3), hi1.round(3), "all frames", lo.round(3), hi.round(3))
for f in (0, 120, 200, 250, 299):
    print("frame", frames[f], "pelvis", pelvis[f].round(3), "head", markers["head"][f].round(3), "tip", tip[f].round(3))

group_list = [groups[k] for k in sorted(groups)]
header = {
    "source": os.path.basename(source),
    "units": "metres", "up": "+Y",
    "fps": fps, "firstFrame": first, "frames": len(frames), "points": total,
    "materials": materials,
    "groups": [{"material": k, "vertices": len(groups[k]["source"]), "triangles": len(groups[k]["tri"]) // 3} for k in sorted(groups)],
    "pieces": {name: [o, n] for name, (o, n) in pieces.items()},
    "markers": list(markers.keys()),
}
blob = json.dumps(header, indent=1).encode()
blob += b" " * (-len(blob) % 4)
with open(target + ".tmp", "wb") as out:
    out.write(b"KNCACHE1")
    out.write(struct.pack("<i", len(blob)))
    out.write(blob)
    for g in group_list:
        out.write(np.array(g["source"], dtype="<i4").tobytes())
        out.write(np.array(g["uv"], dtype="<f4").tobytes())
        out.write(np.array(g["tri"], dtype="<i4").tobytes())
    out.write(positions.astype("<f4").tobytes())
    out.write(marker_array.astype("<f4").tobytes())
os.replace(target + ".tmp", target)
print(f"wrote {target} ({os.path.getsize(target) / 1e6:.0f} MB) in {time.time() - t0:.1f}s")
for g, k in zip(group_list, sorted(groups)):
    print(f"  {materials[k]['name']}: {len(g['source'])} vertices, {len(g['tri']) // 3} triangles")
