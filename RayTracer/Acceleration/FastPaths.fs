namespace Tracer.Basics

/// Allocation-free intersection for primitives an accelerator tests in bulk, such as mesh triangles.
///
/// A leaf test through `hitFunction` builds a HitPoint for every candidate, misses included, and at
/// millions of candidates per second that allocation rate keeps the garbage collector stopping every
/// render thread. An accelerator that sees this interface finds the closest primitive by its hit time
/// alone and builds the HitPoint only for the winner, by calling the primitive's own intersection again,
/// so the hit it returns is the one `hitFunction` would have returned.
[<AllowNullLiteral>]
type IHitTime =
    /// The ray parameter of the hit `hitFunction ray` reports, or NaN when it reports a miss. Must not
    /// allocate, and must agree exactly with `hitFunction`. Only for single-intersection primitives
    /// (triangles), whose interval hit is their `hitFunction` hit filtered to the interval.
    abstract member HitTime: Ray -> float

/// Visibility without shading: shadow rays only need to know that something blocks the segment.
[<AllowNullLiteral>]
type IOccluder =
    /// True exactly when `HitWithin(ray, minimum, maximum)` (IIntervalShape) would report a hit.
    abstract member Occludes: Ray * minimum: float * maximum: float -> bool
