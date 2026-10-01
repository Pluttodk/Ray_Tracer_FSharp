# Sponza assets

Fetched by `scripts/fetch-sponza-assets.sh` from the [Intel Graphics Research Samples Library](https://www.intel.com/content/www/us/en/developer/topic-technology/graphics-processing-research/samples.html); the binaries are not committed. The script fetches only the glTF scenes, textures, HDRIs and the knight's USD, skipping the 3ds Max, FBX, USDA, Alembic and Maya files, and then bakes the knight into `knight/knight.cache` with `scripts/sponza/convert-knight.py`.

**Sponza 2022 Scene commissioned by Frank Meinl, sponsored by Anton Kaplanyan.**

Licence: [Creative Commons Attribution 4.0 International](https://creativecommons.org/licenses/by/4.0/) (the full text is in each package's `credits_license.txt`). The licensor provides the material as-is, without warranties of any kind (CC-BY 4.0, Section 5). Intel's credits file also says: "For personal use and educational use. Limited commercial use for marketing and print purposes." Contact for issues: sponza.feedback@intel.com.

Citation requested by Intel for publications:

```bibtex
@misc{sponza22,
  Author = {Frank Meinl and Anton Kaplanyan},
  Year = {2022},
  Note = {https://www.intel.com/content/www/us/en/developer/topic-technology/graphics-processing-research/samples.html},
  Title = {Intel Sample Library}
}
```

Sponza Addon Package Crew: Katica Putica, Cristiano Siqueira, Timothy Heath, Justin Prazen, Sebastian Herholz, Bruce Cherniak, Anton Kaplanyan.
Reference photos: Katica Putica and Princino.photo, Dubrovnik, Croatia ([princinophoto.com](https://www.princinophoto.com)).

| Directory | Package (Intel download ID) | Used files | Licence |
|-----------|-----------------------------|------------|---------|
| `main_sponza/` | Sponza 2022 base scene (830833) | `NewSponza_Main_glTF_003.gltf/.bin`, `textures/` | CC-BY 4.0 |
| `pkg_a_curtains/` | Add-on: colourful curtains (726650) | `NewSponza_Curtains_glTF.gltf/.bin`, `textures/` | CC-BY 4.0 |
| `pkg_b_ivy/` | Add-on: ivy growth (726656) | `NewSponza_IvyGrowth_glTF.gltf/.bin`, `textures/` | CC-BY 4.0 |
| `pkg_c_trees/` | Add-on: cypress trees (726662) | `NewSponza_CypressTree_glTF.gltf/.bin`, `textures/` | CC-BY 4.0 |
| `pkg_d_10k_candles/` | Add-on: 10k emissive candles (726676) | `NewSponza_4_Combined_glTF.gltf/.bin` | CC-BY 4.0 |
| `pkg_e_knight_anim/` | Add-on: animated knight (763175) | `knight_USD_PREVIEW_SURFACE_ANIM_002_1.usd`, `Textures/` | CC-BY 4.0 |
| `knight/knight.cache` | Derived: baked from the knight USD (see below) | | CC-BY 4.0 (adapted) |

## The knight

Animated knight from the Sponza 2022 add-on packages (Intel, Sponza Addon Package Crew), authored in Autodesk Maya 2022.3; CC-BY 4.0. **Modified**: `scripts/sponza/convert-knight.py` converts the baked per-vertex animation of the USD (135 armour pieces, 300 frames at 24 fps) from centimetres to metres, triangulates it and stores it as a vertex cache with per-material topology; the renderer recomputes normals per pose and rebuilds the UsdPreviewSurface materials. Texture maps are downsampled at load time.

## HDRIs

| File | Source | Author | Licence |
|------|--------|--------|---------|
| `main_sponza/textures/kloppenheim_05_4k.hdr` | [Kloppenheim 05, Poly Haven](https://polyhaven.com/a/kloppenheim_05) | Greg Zaal | CC0 1.0 (public domain) |
| `pkg_b_ivy/textures/Fortress-Kaštel-4K.hdr` | Shipped in Intel's ivy add-on package | Not stated in the package | Distributed by Intel under the package's CC-BY 4.0 licence; no separate source or licence is given, and we could not trace an upstream original |
