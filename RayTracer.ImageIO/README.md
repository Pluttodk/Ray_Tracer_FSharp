# Managed image I/O

`Tracer.Imaging.RgbImage` owns a packed `byte[]` of **RGB8, top-left-first**
pixels, with no row padding. `new RgbImage(width, height)` allocates black pixels;
`FromPixels(width, height, bytes)` copies its input. `Pixels` exposes the owned
array for output accumulation; concurrent writers must use disjoint pixels.
`GetPixel`/`SetPixel` adapt `System.Drawing.Color`, which is a portable value type.

`Load(path)` and `Load(stream)` decode PNG/JPEG. `SavePng(path)` and
`SavePng(stream)` encode PNG. File overloads own and dispose their streams;
stream overloads leave the caller's stream open. Dispose an image when finished.
Texture closures should copy the decoded pixels before disposing the loader,
then retain that managed array for the closure's lifetime. No native bitmap,
graphics context, windowing dependency, or per-pixel lock is involved.

The codec does not flip rows or apply gamma, exposure, or tone mapping.
`FlipVertical()` is explicit. The legacy renderer continues to call the frozen
`Colour.ToColor` and flip vertically; modern transfer settings belong to the
renderer, not this library.

## Dependency/license audit

Versions were selected using `dotnet package search <id> --exact-match` and are
pinned in the project plus NuGet lock files. The repository remains GPLv2.

| Package | Pinned version | License/source |
| --- | --- | --- |
| StbImageSharp | 2.30.16 | `Unlicense OR MIT` in the [exact package metadata](https://api.nuget.org/v3-flatcontainer/stbimagesharp/2.30.16/stbimagesharp.nuspec); use the MIT option. |
| StbImageWriteSharp | 1.16.7 | Public domain in the [release README](https://github.com/StbSharp/StbImageWriteSharp/blob/276e0a0e389f8bcc79aa6fb4446ff825d0fdb2b4/README.md#license). |
| FParsec (renderer dependency) | 1.1.1 | [Simplified/2-clause BSD](https://www.quanttec.com/fparsec/license.html); Unicode data has the separately noted Unicode terms. Its documentation's CC-BY-NC license does **not** apply to the library. |

These permissive/public-domain code licenses are compatible with the GPLv2
repository. The two Stb packages are managed C# ports, not native wrappers.
Do not redistribute their binaries without preserving applicable upstream
copyright/license notices.
