# Plan: make `dragon-flight` look realistic

Start a fresh session in `/home/movj/code/Ray_Tracer_FSharp` with:

> Execute the plan in `docs/plans/dragon-realism.md`. You are the orchestrator.

## Goal

The current film (`artifacts/anim/dragon-flight/dragon-flight.mp4`, commit `858eb4b`) has five problems:

- flat, front-lit lighting
- no bounce light
- no atmospheric depth
- faceted terrain with black ridge artefacts
- a plastic, cartoonish dragon

The target is a golden-hour shot that reads as real: rim-lit dragon, hazy layered mountains, a bright sun with bloom, and tone-mapped highlights.

## Findings this plan is based on

All paths are relative to the repo root.

**Render settings**
- The film renders with the **classic (Whitted) integrator**, `--spp 9`, the `srgb` hard clip and no denoise (README.md:208). Matte surfaces get no indirect light.
- The path tracer (`RayTracer/PathTracing/PathIntegrator.fs`) already has next-event estimation, MIS, Russian roulette, a GGX BSDF and an OIDN denoiser. It is unused by the film.
- `--transfer aces` exists (Narkowicz fit, `Core/Colour.fs:63-84`).
- There is no bloom, vignette, colour grading or depth of field on the film cameras.

**Lighting**
- Sun: `DirectionalLight(Colour(1,0.78,0.55), 1.15, (-0.85,0.2,0.35))` (Film.fs:55, :223).
- Sky: a procedural gradient `EnvironmentLight` with 4 samples, cosine-sampled (Film.fs:58-73). It has no sun disc and is not importance sampled.
- `MaxBounces = 2`.

**Atmosphere**
- There is no fog. "Haze" is faked in the far-ring material (Terrain.fs:101-109).

**Terrain**
- Flat-shaded facets, and black lines along the ridges, most likely the shadow terminator or self-shadowing on coarse triangles.
- Colour comes from a 64×64 matte lookup, giving uniform bands.

**Dragon**
- `dragon_evolved.glb` (Quaternius) is a chibi model with flat colour factors.
- The glTF importer maps it to Phong (exponent around 65, `Gltf.fs:115-127`).
- The importer ignores metallic/roughness textures, normal maps and occlusion maps, and samples textures nearest-neighbour.
- Skinned deformation is posed at mid-shutter, so wings get no motion blur.

## Rules for the orchestrator

1. **Branching.**
   - `animation-v1` has not been pushed yet. First run `git push -u origin animation-v1`.
   - Create the integration branch `dragon-realism` from it and `git push -u origin dragon-realism`.
   - Each workstream runs in its own worktree (`isolation: "worktree"`) on the branch `dragon-realism/<ws-id>`.
2. **Models.**
   - Use `model: "sonnet"` (Sonnet 5.5) for small, well-specified work: workstreams A, D, F, and all review-still or reporting chores.
   - Use the default (Opus) model for B, C and E, which touch the integrators or the BSDF and importer.
3. **Committing and pushing.**
   - Every subagent commits each coherent step (build plus relevant tests green) and runs `git push -u origin dragon-realism/<ws-id>` after each commit.
   - The orchestrator merges a finished workstream into `dragon-realism` only after `dotnet build -c Release` and `dotnet test UnitTests` (or the repo's test runner) pass on the merge, then pushes immediately.
   - Never let more than about 30 minutes of finished work sit unpushed.
   - Commit messages follow the repo style and end with the `Co-Authored-By` line from the session's attribution rule. Never put a model name anywhere else in a commit.
4. **Render budget.**
   - The machine is shared by all agents, and while subagents run, the orchestrator's own renders count against the same budget.
   - Subagents render only stills, at most 480×270 and 4 frames, with `--no-video --no-audio --start N --end N`.
   - Only the orchestrator renders full-resolution comparisons or the film.
5. **File ownership.** Each workstream lists the files it owns. Touching another workstream's files requires asking the orchestrator first, through the agent's final report or a reply to a check-in.
   - `Film.fs` is shared. Each workstream may only add its own clearly marked block or parameter.
   - The orchestrator resolves `Film.fs` conflicts during merges.
6. **No asset or licence decisions without the user.** Swapping the dragon model, or adding any CC-BY or other attributed asset, needs the user's explicit OK.

## Check-ins every 4 minutes (mandatory)

As soon as the first background subagent is launched, create a session-scoped cron with `CronCreate` and the schedule `*/4 * * * *`. Its prompt:

> Dragon-realism check-in: for every running workstream agent, verify it is on-task and not derailed. Follow the "Check-in procedure" in docs/plans/dragon-realism.md.

Delete it with `CronDelete` when every workstream is merged or stopped. If cron is unavailable, use `ScheduleWakeup` with 240 s instead.

**Check-in procedure**, for each running agent:

1. Inspect its worktree. Run `git -C <worktree> log --oneline dragon-realism..HEAD` and `git -C <worktree> status --short`, then `git diff --stat` against the workstream's file list.
2. Look at the agent's latest activity and transcript output, if visible.
3. Classify the agent:
   - **On track:** working in its own files, on its current milestone, making progress. Do nothing.
   - **Scope drift:** editing files it doesn't own, refactoring unrelated code, or building features outside the spec. `SendMessage` a short redirect quoting the spec line.
   - **Stuck:** the same failing build or test, or no new diff or commit, across 3 consecutive check-ins (12 min). `SendMessage` with a hint or a narrower next step.
     - If it is still stuck at the next check-in, stop it (`TaskStop`) and relaunch it with the improved prompt and its branch as the starting point.
   - **Rendering too much:** renders over budget or full films. Tell it to stop.
4. Append one line per agent to `docs/plans/dragon-realism-status.md`, in the form `HH:MM <ws-id> <state> <one-line note>`. Commit this file only during merges, not on every check-in.
5. If a workstream has reported done, review its diff and stills, then merge, test and push (rule 3).

Keep check-ins short. They exist to catch derailment, not to micromanage.

## Phase 0 — baseline (orchestrator, solo, about 15 min)

1. Branch and push as in rule 1. Make sure the assets are present (`scripts/fetch-dragon-assets.sh`).
2. Choose 5 review frames, one per shot. Find the shot boundaries in `Film.fs` (camera `Cuts`). Suggested starting points are about 2 s, 8 s, 14 s, 19 s and 26 s, i.e. frames 48, 192, 336, 456 and 624.
3. Render the baseline stills at 960×540 with the current settings into `artifacts/review/00-baseline/`.
4. Render the same frames with `--integrator path --denoise --transfer aces --spp 64` into `artifacts/review/01-path-aces/`. This shows how much the existing features alone give.
5. Write `scripts/review-stills.sh <label> [extra args]`, which renders the 5 frames into `artifacts/review/<label>/` and builds a 5-up contact sheet with ffmpeg `tile`. Commit and push.

## Phase 1 — parallel workstreams (all launched together, in the background)

Every workstream prompt must include:

- the workstream's section of this plan
- the rules above (branching, committing and pushing, render budget, file ownership)
- the `scripts/review-stills.sh` usage
- a deliverable: a final report listing commits, before/after still paths, open issues, and any files it needed outside its scope

### A — Film render defaults and post-processing (Sonnet)

**Owns:** `RayTracer/Core/Colour.fs`, a new `RayTracer/Post/*.fs` (post-processing on the float framebuffer), `RayTracer/Render.fs` (post hook only), `AnimationRunner/Program.fs` (flags), and the README film command.

- Let a demo declare recommended render settings (integrator, transfer, MaxBounces, spp, denoise) so `dragon-flight` uses path + denoise + ACES by default. CLI flags still override them.
- **Bloom:** threshold, then a multi-scale Gaussian or dual-filter blur, added before tone mapping. Expose it as `--bloom <strength>` plus a per-demo default.
- Add a vignette and a simple grade (exposure, white balance or lift/gamma/gain, saturation), applied in linear light before the transfer.
- Write tests for the bloom kernel normalisation (energy is conserved when the threshold is 0) and for grade identity at default parameters.

### B — Sky, sun and shot lighting (Opus)

**Owns:** a new `RayTracer/Lights/Sky.fs`, `RayTracer/Lights/GlobalLight.fs` (importance sampling only), and the sky and light block in `Film.fs` (around :55-73 and :223-232).

- Implement an analytic sky: Preetham, or Hosek–Wilkie if it is feasible, driven by sun elevation and turbidity.
- Add a visible sun disc (about 0.53°) with limb darkening and physically scaled radiance. The sun should be about 5–10× the brightness of the sky's diffuse contribution.
- **Importance-sample the environment light** with a 2D luminance CDF over a lat-long table, for both integrators with MIS in the path tracer. Without this the bright sun disc becomes fireflies.
- **Relight the film for golden hour:**
  - Lower the sun (about 4–8°).
  - Per shot, put the sun behind or beside the dragon for rim and back light. Changing the per-shot sun direction slightly is acceptable if continuity holds; otherwise adjust camera framing in coordination with the orchestrator.
  - Check that the dragon's shadow is visible on the terrain in the close-pass shot.
- Write tests: the sky's luminance is finite and positive above the horizon, and the CDF sampling pdf integrates to about 1.

### C — Aerial perspective and haze (Opus)

**Owns:** a new `RayTracer/Atmosphere.fs`, `RayTracer/Scene.fs` (one optional `Atmosphere` field), the hit-shading tail of both integrators (`ClassicIntegrator.fs`, `PathIntegrator.fs`), and the removal of the fake haze in `Terrain.fs:101-109` (coordinate with D through the orchestrator).

- Add analytic height fog with aerial perspective: transmittance `exp(-∫σ(h))` for exponential density along the ray, and in-scattered colour taken from the sky (B's sky, or the scene's environment if B isn't merged yet).
- Add forward-scattering towards the sun with a Henyey–Greenstein phase, g ≈ 0.7, so haze glows around the sun.
- Apply it to camera rays and to secondary rays in the path tracer. Shadow rays may ignore it.
- Optionally add cheap god rays: a single-scatter march with shadow tests, behind a flag, off by default.
- Write tests: no atmosphere gives an identical result, and transmittance is monotonic in distance.

### D — Terrain quality (Sonnet)

**Owns:** `RayTracer.Animation/Terrain.fs` (except C's haze removal) and the terrain settings in `Film.fs:28-52`.

- Add smooth per-vertex normals, plus higher resolution near the camera paths if the BVH memory allows.
- **Fix the black ridge lines.**
  - Reproduce them on a still.
  - Determine whether the cause is shadow-ray self-intersection, shading-normal versus geometric-normal mismatch, or mesh cracks.
  - Fix them with an offset along the geometric normal and a shadow-terminator fix (for example, the Hanika 2021 "hacking the shadow terminator" offset). If the fix belongs in the core, report the needed core change to the orchestrator instead of editing integrators.
- **Material variety:**
  - Noise-modulated rock strata and colour variation.
  - Snow driven by slope, height and a noise mask, with a sharper but irregular boundary.
  - Darker, desaturated forest at distance.
  - Slight specular on snow.
- Write tests: normals are normalised and continuous across shared vertices, and the terrain still builds deterministically from its seed.

### E — Physically based materials and glTF import (Opus)

**Owns:** `RayTracer.Animation/Gltf.fs` (materials and textures), `RayTracer/PathTracing/MaterialAdapter.fs`, a new PBR material in `RayTracer/Materials*.fs` / `PathTracing/Surface.fs`, and a new texture filtering module.

- Add a real `PbrMaterial` (base colour, metallic, roughness, normal map, occlusion, emissive, transmission) that the path tracer uses directly instead of going through the Phong-exponent round-trip. Classic-integrator fallback via the existing adapter is fine.
- In the glTF importer, read the metallicRoughness texture, the normal texture (tangents from `TANGENT`, or MikkTSpace-like generation from UVs), occlusion, and `KHR_texture_transform`. Use bilinear filtering, with mipmaps if time allows.
- **Wing membranes:** add a thin-surface diffuse transmission lobe (back-light shows through), selectable per material.
- **Dragon look while the model is unchanged:**
  - Material overrides by glTF material name in `Film.fs`: darker, desaturated scales, roughness about 0.55, slight sheen, a procedural scale bump (Voronoi), translucent wings and a wet glossy eye.
  - Add these as a marked block in `Film.fs`.
- Write tests: GGX white-furnace for the new material, normal-map identity with a flat map, and bilinear sampling at texel centres.

### F — Realistic dragon asset scouting (Sonnet, research only, no code merges)

**Owns:** `assets/dragon/CANDIDATES.md` only.

- Find 3–5 realistic, rigged, animated dragon models with flight, hover or glide cycles and physically based texture maps. Licence must be CC0, CC-BY or a similar compatible licence, and the asset downloadable without login if possible.
- For each candidate, record:
  - URL and licence
  - triangle count
  - bones and clips
  - texture set
  - glTF availability
  - which `Film.fs` clips it could map to
  - risks
- Do a smoke test: download the top candidate to `/tmp`, import it with the existing glTF loader in a scratch test or script, and report what breaks.
- Do not change the fetch script and do not commit binaries. The orchestrator asks the user before any swap.

## Phase 2 — integration (orchestrator, sequential)

1. Merge in this order, testing and pushing after each: A, D, C, B, E. B and C both touch lighting colour, so merge C first and let B adapt the sky colour.
2. Re-render the review stills, then tune exposure, sun strength, haze density, bloom and grade on the integrated branch. Do this at 480×270 first, then confirm at 960×540.
3. Show the user the contact sheets (`SendUserFile`) for baseline, path-aces and integrated. Present F's candidate list and **ask whether to swap the dragon model**.
4. If the user approves the swap, run a follow-up workstream G (Opus): update the fetch script, `SOURCES.md` and attribution, remap clips in `Film.fs`, and re-tune the materials.
5. Stretch goals, each a small Sonnet workstream with the same check-in rules:
   - depth of field on the close-up shots (thin lens, focus on the dragon)
   - motion blur on skinned deformation (pose per motion step)
   - wing-tip vortex or dust particles

## Phase 3 — final render and wrap-up

1. Render the full film at 960×540 with the new defaults, keeping resume on. Check the time per frame first and tell the user the estimate before committing to a long render.
2. Update the README's dragon-flight section to cover the new features and command.
3. Make the final commit and push. Optionally send the MP4 with `--telegram` if the user wants.
4. Delete the check-in cron. Summarise for the user what changed, with before/after frames and what remains.

## Definition of done

- [ ] The dragon reads as a creature, not a toy: rim light, a visible shadow, translucent wings, no Phong plastic highlight.
- [ ] Distance is legible through haze layers; nothing in the mid-ground has foreground saturation.
- [ ] The sun and sky have a disc, bloom and tone-mapped highlights, with no hard clipping.
- [ ] There are no black ridge lines or visible terrain facets at 960×540.
- [ ] The denoised path-traced frames show no fireflies or noise at the default spp.
- [ ] All tests pass. Everything is merged into `dragon-realism` and pushed.
