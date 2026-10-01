# Realistic dragon candidates (workstream F)

Scouted 2026-10-01. Nothing here is fetched by `scripts/fetch-dragon-assets.sh`, and no binaries are committed.
The film currently uses `dragon_evolved.glb` (Quaternius, CC0, chibi, flat colours).

Bottom line: no free (CC0/CC-BY) glTF dragon that is both realistic (PBR texture set) and downloadable without login
turned up. The realistic ones are either paid (3DDisco, Cassana XR), behind a Sketchfab login (CC-BY, but unverified
rig and clips), or Blender-only. The best no-login option that imports cleanly is low-poly with a single colour texture.

## Ranked candidates

| # | Model | Licence | Tris | Rig / clips | Textures | Format, access | Fit |
|---|-------|---------|------|-------------|----------|----------------|-----|
| 1 | [Simple 3D Dragon Model](https://opengameart.org/content/simple-3d-dragon-model), MattBas (OpenGameArt) | CC-BY-SA 4.0 | 2,676 (2,309 verts) | 59 joints; `Flying-loop` 1.04 s, `Idle-loop` 1.46 s, `Walk-loop` 1.67 s | One baked colour PNG (`glax.png`); material metallic 0, roughness 0.5 (KHR_materials_specular). No normal/roughness maps | GLB in `dragon.tar.gz` (168 KB), direct download, no login: `https://opengameart.org/sites/default/files/dragon.tar.gz` | Fast flight = Flying-loop; hover = Idle-loop (or slowed Flying-loop); no roar/lunge |
| 2 | [Cethiel's Dragon 3D](https://opengameart.org/content/cethiels-dragon-3d), Drummyfish and Cethiel (OpenGameArt) | CC0 (credit optional) | Low-poly, not measured | Rigged; separate Collada files `dragon_idle`, `dragon_walk`, `dragon_attack`, `dragon_die` | Hand-painted colour PNGs (`dragon.png`, `dragon_black.png`) | `dragon_oga.zip` (1.4 MB), DAE/OBJ/.blend, no login. Needs conversion to glTF in Blender | Idle = hover; attack = roar/lunge; no flight clip |
| 3 | [Flying Dragon](https://opengameart.org/content/flying-dragon), peaznchips (OpenGameArt) | CC0 | Not stated | Curve-driven flight path baked to armature keys by a Blender script | Textured, custom normal maps, modified roughness map (the only free one found with PBR-ish maps) | `fantasydragon_animation_free.zip` (20.5 MB), .blend only, no login. Needs Blender work to export glb | Fast flight and hover possible via the path script; Chinese-style dragon; most work |
| 4 | Sketchfab CC-BY group: [Animated Dragon Three Motion Loops](https://sketchfab.com/3d-models/animated-dragon-three-motion-loops-eca98cf6cd084c1596cecf716e110c29) (LasquetiSpice, CC-BY 4.0, 19.5k tris, 3 loops); [Dragon flying](https://sketchfab.com/3d-models/dragon-flying-78f809b98bbe426e94d4024dc894b206) (NORBERTO-3D, CC-BY, 7.5k tris); [Realistic Dragon Textures](https://sketchfab.com/3d-models/realistic-dragon-textures-66803f2e8461486987d72d116fe2f714) (Jazz Vincent, CC-BY, 19.6k tris, rig not stated, original modeller uncredited) | CC-BY (4.0 for the first) | 7.5k to 19.6k | Clips and rig unverified from the listing pages | Unverified; the Jazz Vincent one is called realistic | Sketchfab glb download needs a free login, so not scriptable and not tested | Most realistic of the free ones, but needs the user to log in and check clips by hand |
| 5 | [Flying Dragon](https://www.renderhub.com/pig-scales-studio/flying-dragon), Pig Scales Studio (RenderHub) | RenderHub "extended use" free licence (not CC) | 1,092 polys | 25 bones; takeoff and flying clips, flight loops | Materials in the .blend only | FBX/DAE/.blend; account likely needed | Fast flight; low-poly; licence terms need review |

Also looked at, rejected:

- [Dragon Rigged](https://poly.pizza/m/WIOTISRjeX), na3ee1 (Poly Pizza, CC-BY): downloadable without login, 2,482 tris, 79 joints, but the glb has no animation clips and uses flat material colours. Skin imports; there is nothing to play.
- Paid, genuinely realistic with 4k PBR sets and flying plus roar clips: 3DDisco "Realistic Black Wyvern" and "Realistic Gold Dragon" (itch.io / Sketchfab Store), Cassana XR "Animated Realistic Lowpoly Chinese Dragon" (26.3k tris, PBR, GLB). Licences are commercial, so not usable without the user buying one.
- [Dragon Rigged](https://sketchfab.com/3d-models/dragon-rigged-23d63b15bdb448108888a3985ccbc0a0) (noname001, CC-BY, 15k tris, armature, no clips listed) and [Dragon](https://sketchfab.com/3d-models/dragon-aefe420039b64b488b59cf7d34ca7457) (simoslay, CC-BY 4.0, 38k tris, detailed scales but no rig or animation mentioned): Sketchfab login needed.
- Others on OpenGameArt: Dragon (3D), CC-BY-SA, 2,250 tris (walk/attack/idle/sleep/die, no flight); Flying Dragon Rework is 2D sprites.

## Attribution text

- Candidate 1: `Dragon model by MattBas, https://opengameart.org/content/simple-3d-dragon-model, CC BY-SA 4.0 (https://creativecommons.org/licenses/by-sa/4.0/)`. Share-alike: adaptations of the model (including a retextured or re-rigged copy distributed in the repo) must stay CC-BY-SA. Rendered frames and the film are arguably not adaptations of the model, but the repo's SOURCES.md would need a CC-BY-SA row, and the user should decide whether that is acceptable.
- Candidate 2: none required; credit Cethiel and Drummyfish if wished.
- Candidate 3: none (CC0).

## Smoke test: candidate 1 through `Tracer.Animation.Gltf.load`

Release build of `AnimationRunner`, then a scratch `dotnet fsi` script in `/tmp/wsf` (outside the repo) that calls
`Gltf.load path { ImportOptions.Default with Clip = None }` and prints the scene. File inspected as downloaded from OpenGameArt.

What worked:

- Loads with no exception. Two benign warnings (no camera, no lights; the loader adds defaults).
- Skinned mesh imported: 1 mesh node, 2,309 verts, 2,676 tris, 59 joints. One material, textured (colour PNG decoded into a `Texture`).
- All three clips imported, 177 channels each: `Flying-loop` 1.042 s, `Idle-loop` 1.458 s, `Walk-loop` 1.667 s. Names have no `CharacterArmature|` prefix, so `Film.fs` `clip` lookups would need changing.

What would break or need work for a swap in `Film.fs`:

- Clip names: `Film.dragon` looks up `"CharacterArmature|" + name` and needs `Fast_Flying`, `Flying_Idle` and `Headbutt`. Mapping would be Flying-loop for flight, Idle-loop for hover. There is no roar or lunge clip, so the roar shot would need a procedural head and jaw move or a different clip.
- Scale and orientation: bounding box in bind pose is x +/-6.19, y 0 to 2.83, z -5.18 to 4.37, so the wings span about 12 m in this unit and the body is about 2.8 m tall. `dragonScale` and the `Translation = (0, -3.5, 0)` offset in `Film.fs` are tuned for `dragon_evolved` and would need retuning. The model sits on y = 0 with wings spread in x (T-pose style).
- Loops are short (about 1 s) so the arrangement in `Clip.arrange` (segment start times at 12.8 s, 18.6 s and so on) works with Loop = true but wing-beat speed would need to be re-tuned.
- Realism: single baked colour map, roughness 0.5, metallic 0, no normal map; it will not look more realistic than the current model in a path-traced render. It is arguably less appealing.
- Not rendered; no still produced.

Rejected na3ee1 (Poly Pizza) as a second sample: loads, but with no clips; the mesh is split in 6 skinned nodes, 79 joints, and the bind-pose bounding box is only about 0.2 units wide (needs about x60 scaling).

## Recommendation

Do not swap yet. If the user wants to try, the pragmatic path is:

1. Try candidate 4 (Sketchfab CC-BY, e.g. LasquetiSpice three loops or Jazz Vincent) by having the user download the glb after logging in; the loader handles skinned glb with PBR images, and clip names can be mapped in `Film.fs`. The licence is CC-BY, so only credit is needed.
2. Otherwise, buy or accept a commercial licence for one of the paid realistic wyverns (4k PBR, flying and roar clips), which is the only route to a true roar clip.
3. Candidate 1 is the only free, no-login, directly importable option, but it is a quality step down and CC-BY-SA.

Importer gaps seen for any of these: none for skins, clips or textures; the work is in `Film.fs` clip names, scale and the roar.
