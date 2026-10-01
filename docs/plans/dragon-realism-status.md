# Dragon-realism status log

Branch names are `dragon-realism-<ws>`, not `dragon-realism/<ws>`. Git refuses a slash-namespaced branch while a branch named `dragon-realism` exists.

14:28 f done CANDIDATES.md (d4ac351): no free, realistic, login-free glTF dragon found; recommends no swap; merged
14:28 a on-track first commit 9d95440 post-processing + render defaults
14:28 b on-track launched
14:28 c on-track launched
14:28 d on-track launched
14:28 e on-track launched
14:29 a done 9d95440 reviewed; merged, build+1430 tests green
14:31 d done 7fe1a16 reviewed; merged (test-registration conflicts resolved), 1436 tests green. Ridge cause = shadow terminator; core fix -> follow-up d2
14:34 c done d2f29af,e75aaca reviewed; merged, 1447 tests green. No god rays. B must retune SkyWeight/SunWeight
14:37 d2 done 4fa101f,ed58243 reviewed; merged, tests green. Open: Transform.intersectLocal drops ShadowPoint (falls back safely; not used by film meshes)
14:40 b done 89c543f,f131177 reviewed; merged, 1514 tests green; applied B's tracking-camera flip
14:56 e done fdbe28d..fdc1c6b reviewed; merged, 1577 tests green. Edited PathIntegrator NEE for translucency (reviewed OK). Open: no real tangents for normal maps, no mipmaps
15:25 s done dad5ccd reviewed; merged; summit YFov 0.8->0.6 by orchestrator; tests green
15:44 dof done reviewed; merged, tests green, ~0 cost
