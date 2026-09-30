namespace Tracer.Gpu

open System.Runtime.InteropServices
open ILGPU

[<Struct; StructLayout(LayoutKind.Sequential); NoEquality; NoComparison>]
type V3 =
    { X: float32
      Y: float32
      Z: float32 }

[<Struct; StructLayout(LayoutKind.Sequential); NoEquality; NoComparison>]
type V2 =
    { X: float32
      Y: float32 }

[<Struct; StructLayout(LayoutKind.Sequential); NoEquality; NoComparison>]
type DeviceTransform =
    { RowX: V3
      RowY: V3
      RowZ: V3
      Translation: V3 }

[<Struct; StructLayout(LayoutKind.Sequential); NoEquality; NoComparison>]
type DeviceTriangle =
    { A: V3
      B: V3
      C: V3
      GeometricNormal: V3
      NormalA: V3
      NormalB: V3
      NormalC: V3
      UvA: V2
      UvB: V2
      UvC: V2
      Material: int
      Object: int }

[<Struct; StructLayout(LayoutKind.Sequential); NoEquality; NoComparison>]
type DeviceNode =
    { Low: V3
      High: V3
      Left: int
      Right: int
      Start: int
      Count: int }

[<Struct; StructLayout(LayoutKind.Sequential); NoEquality; NoComparison>]
type DeviceMaterial =
    { Colour: V3
      AmbientColour: V3
      SpecularColour: V3
      ReflectionColour: V3
      Filter: V3
      Ambient: float32
      Diffuse: float32
      Specular: float32
      Reflectivity: float32
      Ior: float32
      Emission: float32
      Kind: int
      Exponent: int
      GlossExponent: int
      GlossySampleOffset: int
      GlossySampleSets: int
      TextureOffset: int
      TextureWidth: int
      TextureHeight: int }

[<Struct; StructLayout(LayoutKind.Sequential); NoEquality; NoComparison>]
type DeviceLight =
    { Position: V3
      Direction: V3
      EdgeU: V3
      EdgeV: V3
      Radiance: V3
      Area: float32
      Kind: int
      SampleOffset: int
      SampleCount: int
      SampleSets: int }

[<Struct; StructLayout(LayoutKind.Sequential); NoEquality; NoComparison>]
type DeviceCamera =
    { Position: V3
      U: V3
      V: V3
      W: V3
      ViewDistance: float32
      PixelWidth: float32
      PixelHeight: float32 }

[<Struct; StructLayout(LayoutKind.Sequential); NoEquality; NoComparison>]
type DeviceSettings =
    { Camera: DeviceCamera
      Ambient: V3
      Background: V3
      Width: int
      Height: int
      CameraSamples: int
      CameraSampleSets: int
      CameraSampleOffset: int
      GlossySamples: int
      MaxBounces: int
      Seed: int
      TriangleCount: int
      NodeCount: int
      LightCount: int
      ObjectCount: int
      AllOpaque: int
      RayCapacity: int
      MediumCapacity: int
      TraversalCapacity: int
      MaximumRayWork: int
      BatchStart: int
      BatchCount: int }

[<Struct; StructLayout(LayoutKind.Sequential); NoEquality; NoComparison>]
type DeviceScene =
    { Triangles: ArrayView<DeviceTriangle>
      Nodes: ArrayView<DeviceNode>
      PrimitiveIndices: ArrayView<int>
      Materials: ArrayView<DeviceMaterial>
      ObjectMaterials: ArrayView<int>
      WorldToObject: ArrayView<DeviceTransform>
      Lights: ArrayView<DeviceLight>
      TexturePixels: ArrayView<V3>
      Samples: ArrayView<V2>
      InitialMedia: ArrayView<int> }

[<Struct; StructLayout(LayoutKind.Sequential); NoEquality; NoComparison>]
type DeviceRay =
    { Origin: V3
      Direction: V3 }

[<Struct; StructLayout(LayoutKind.Sequential); NoEquality; NoComparison>]
type DeviceWork =
    { Ray: DeviceRay
      Weight: V3
      Key: uint64
      Depth: int
      MediumCount: int }

[<Struct; StructLayout(LayoutKind.Sequential); NoEquality; NoComparison>]
type DeviceWorkspace =
    { Rays: ArrayView<DeviceWork>
      Media: ArrayView<int>
      Traversal: ArrayView<int>
      Errors: ArrayView<int>
      SeenObjects: ArrayView<int>
      InitialMediumCount: ArrayView<int> }

[<Struct; StructLayout(LayoutKind.Sequential); NoEquality; NoComparison>]
type DeviceHit =
    { Time: float32
      Beta: float32
      Gamma: float32
      Triangle: int }

[<Struct; StructLayout(LayoutKind.Sequential); NoEquality; NoComparison>]
type DeviceSurface =
    { Point: V3
      Geometric: V3
      Normal: V3
      Uv: V2
      OffsetDistance: float32
      Material: int
      Object: int
      FrontFace: int }

[<Struct; StructLayout(LayoutKind.Sequential); NoEquality; NoComparison>]
type DeviceLightSample =
    { Direction: V3
      Radiance: V3
      Distance: float32
      Weight: float32 }

[<Struct; StructLayout(LayoutKind.Sequential); NoEquality; NoComparison>]
type DeviceBoundary =
    { EtaI: float32
      EtaT: float32
      Count: int }

[<Struct; StructLayout(LayoutKind.Sequential); NoEquality; NoComparison>]
type DeviceDielectric =
    { Fresnel: float32
      Direction: V3 }
