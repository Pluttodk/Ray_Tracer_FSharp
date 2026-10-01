# Sponza-knight status log

23:35 orch Intel Sponza 2022 + add-ons + knight downloaded (CC-BY 4.0); Blender 4.2 installed; sponza demo skeleton dd7c028
23:35 k launched (knight import; USD is a per-vertex mesh cache, 300 frames)
23:35 r1 launched (alpha mask + tangents + load perf)
23:35 r2 launched; reported SphereLight + HDRI + light BVH pushed on sponza-r2, verifying
23:35 v launched (shadowed single-scatter sun shafts)
23:35 g launched (Cycles ground truth + compare)
23:35 p launched (profiling / speed)
00:06 r2 done 1663785..1448a83; merged; lamps offset -0.2 m, HDRI rot 155 by orch; 1638 tests
00:08 k done a9a750c..0c8956f; merged; 1652 tests. Knight = 12.46 s vertex cache: idle guard 0-4.5, crouch 4.5-5.5, sword overhead 5.5-8, swing 8-8.5, spin+advance -X 8.5-12
00:15 r1 done b1b7f50..692402a; merged (test reg conflicts); 1697 tests; orch: shadow point through transforms
01:05 p done (7b88cf8..fccdc38 + r1patch 323d63b): 7x less CPU/frame, bit-identical; merged; 1698 tests. Use DOTNET_GCgen0size=0x10000000 for the final render
