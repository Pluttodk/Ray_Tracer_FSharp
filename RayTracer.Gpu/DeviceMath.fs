namespace Tracer.Gpu

open System
open ILGPU
open ILGPU.Algorithms

module DeviceMath =
    let v3 x y z : V3 = { X = x; Y = y; Z = z }
    let v2 x y : V2 = { X = x; Y = y }
    let zero () = v3 0.f 0.f 0.f
    let one () = v3 1.f 1.f 1.f
    let add (a: V3) (b: V3) = v3 (a.X + b.X) (a.Y + b.Y) (a.Z + b.Z)
    let sub (a: V3) (b: V3) = v3 (a.X - b.X) (a.Y - b.Y) (a.Z - b.Z)
    let scale (a: V3) factor = v3 (a.X * factor) (a.Y * factor) (a.Z * factor)
    let mul (a: V3) (b: V3) = v3 (a.X * b.X) (a.Y * b.Y) (a.Z * b.Z)
    let neg (a: V3) = v3 -a.X -a.Y -a.Z
    let dot (a: V3) (b: V3) = a.X * b.X + a.Y * b.Y + a.Z * b.Z
    let identityTransform () : DeviceTransform =
        { RowX = v3 1.f 0.f 0.f; RowY = v3 0.f 1.f 0.f; RowZ = v3 0.f 0.f 1.f; Translation = zero () }
    let transformPoint (matrix: DeviceTransform) (point: V3) =
        v3 (dot matrix.RowX point + matrix.Translation.X)
           (dot matrix.RowY point + matrix.Translation.Y)
           (dot matrix.RowZ point + matrix.Translation.Z)
    let transformVector (matrix: DeviceTransform) (vector: V3) =
        v3 (dot matrix.RowX vector) (dot matrix.RowY vector) (dot matrix.RowZ vector)
    let cross (a: V3) (b: V3) =
        v3 (a.Y * b.Z - a.Z * b.Y) (a.Z * b.X - a.X * b.Z) (a.X * b.Y - a.Y * b.X)
    /// Machine epsilon for the device's scalar type, 2^-23.
    ///
    /// Every surface offset in the renderer is expressed in multiples of this.
    /// It must track the device precision: the FP64 value is about 2e-16, and
    /// using that at FP32 makes offsets vanish into rounding, which shows up as
    /// shadow acne and self-intersection rather than as an obvious failure.
    [<Literal>]
    let Epsilon = 1.1920928955078125e-7f

    let maxComponentAbs (a: V3) = XMath.Max(XMath.Abs a.X, XMath.Max(XMath.Abs a.Y, XMath.Abs a.Z))
    let magnitude (a: V3) =
        let largest = maxComponentAbs a
        if largest = 0.f then 0.f
        elif largest = Single.PositiveInfinity then Single.PositiveInfinity
        else
            let x = a.X / largest
            let y = a.Y / largest
            let z = a.Z / largest
            largest * XMath.Sqrt(x * x + y * y + z * z)
    let normalize (a: V3) =
        let largest = maxComponentAbs a
        if largest = 0.f then a
        else
            let x = a.X / largest
            let y = a.Y / largest
            let z = a.Z / largest
            let length = XMath.Sqrt(x * x + y * y + z * z)
            v3 (x / length) (y / length) (z / length)
    let isBlack (a: V3) = a.X = 0.f && a.Y = 0.f && a.Z = 0.f
    let finite (value: float32) = value = value && XMath.Abs value <> Single.PositiveInfinity
    let finite3 (a: V3) = finite a.X && finite a.Y && finite a.Z
    let axisValue (a: V3) axis = if axis = 0 then a.X elif axis = 1 then a.Y else a.Z
    let clamp01 value = XMath.Max(0.f, XMath.Min(1.f, value))

    let interpolateUv (a: V2) (b: V2) (c: V2) beta gamma =
        v2 (a.X + beta * (b.X - a.X) + gamma * (c.X - a.X))
           (a.Y + beta * (b.Y - a.Y) + gamma * (c.Y - a.Y))

    let powi value exponent =
        let mutable remaining = exponent
        let mutable factor = value
        let mutable result = 1.f
        while remaining > 0 do
            if (remaining &&& 1) <> 0 then result <- result * factor
            remaining <- remaining >>> 1
            if remaining > 0 then factor <- factor * factor
        result

    let mixKey (key: uint64) =
        let mutable value = key + 0x9e3779b97f4a7c15UL
        value <- (value ^^^ (value >>> 30)) * 0xbf58476d1ce4e5b9UL
        value <- (value ^^^ (value >>> 27)) * 0x94d049bb133111ebUL
        value ^^^ (value >>> 31)

    let sampleKey seed pixel sample =
        let address = (uint64 (uint32 pixel) <<< 32) ||| uint64 (uint32 sample)
        mixKey (uint64 (uint32 seed) ^^^ mixKey address)

    let sample (scene: DeviceScene) offset count sets key index =
        scene.Samples.[offset + int (key % uint64 sets) * count + index % count]

    let frameV (normal: V3) =
        let up = if XMath.Abs normal.Y > 0.999f then v3 1.f 0.f 0.f else v3 0.f 1.f 0.f
        normalize (cross up normal)

    let hemisphere (sample: V2) exponent =
        let phi = 2.f * float32 Math.PI * sample.X
        let cosine = XMath.Pow(1.f - sample.Y, 1.f / (exponent + 1.f))
        let sine = XMath.Sqrt(XMath.Max(0.f, 1.f - cosine * cosine))
        v3 (sine * XMath.Cos phi) (sine * XMath.Sin phi) cosine

    let reflect (incoming: V3) (normal: V3) =
        normalize (sub incoming (scale normal (2.f * dot incoming normal)))

    let dielectric (incoming: V3) (normal: V3) etaI etaT =
        let cosine = clamp01 (-dot incoming normal)
        let eta = etaI / etaT
        let discriminant = 1.f - eta * eta * XMath.Max(0.f, 1.f - cosine * cosine)
        if etaI = etaT then { Fresnel = 0.f; Direction = incoming }
        elif discriminant <= 0.f then { Fresnel = 1.f; Direction = zero () }
        else
            let transmittedCosine = XMath.Sqrt discriminant
            let parallelTerm = (etaT * cosine - etaI * transmittedCosine) / (etaT * cosine + etaI * transmittedCosine)
            let perpendicular = (etaI * cosine - etaT * transmittedCosine) / (etaI * cosine + etaT * transmittedCosine)
            { Fresnel = 0.5f * (parallelTerm * parallelTerm + perpendicular * perpendicular)
              Direction = normalize (add (scale incoming eta) (scale normal (eta * cosine - transmittedCosine))) }

    let attenuationChannel filter distance =
        if filter = 1.f then 1.f
        elif distance = Single.PositiveInfinity then 0.f
        elif distance = 0.f then 1.f
        elif filter = 0.f then 0.f
        else XMath.Pow(filter, distance)

    let attenuation (filter: V3) distance =
        v3 (attenuationChannel filter.X distance)
           (attenuationChannel filter.Y distance)
           (attenuationChannel filter.Z distance)

    /// Nudge a coordinate one ULP further along the surface normal.
    ///
    /// The bit patterns are FP32: 0x00000001 is the smallest positive
    /// subnormal and 0x80000001 its negative counterpart. Under FP64 these were
    /// 64-bit constants; carrying those over unchanged would step by a
    /// completely wrong magnitude and reintroduce shadow acne.
    let moveOffset value distance normal =
        let shifted = value + distance * normal
        if normal = 0.f then value
        elif shifted = 0.f then
            Interop.IntAsFloat(if normal > 0.f then 1u else 0x80000001u)
        else
            let bits = Interop.FloatAsInt shifted
            Interop.IntAsFloat(if (shifted > 0.f) = (normal > 0.f) then bits + 1u else bits - 1u)

    let offset (surface: DeviceSurface) (outgoing: V3) =
        let normal = if dot outgoing surface.Geometric >= 0.f then surface.Geometric else neg surface.Geometric
        v3 (moveOffset surface.Point.X surface.OffsetDistance normal.X)
           (moveOffset surface.Point.Y surface.OffsetDistance normal.Y)
           (moveOffset surface.Point.Z surface.OffsetDistance normal.Z)

    let spawn (surface: DeviceSurface) direction : DeviceRay =
        { Origin = offset surface direction; Direction = normalize direction }
