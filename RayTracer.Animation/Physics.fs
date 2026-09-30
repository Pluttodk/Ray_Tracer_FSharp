namespace Tracer.Animation

open System
open Tracer.Basics

/// Rigid-sphere dynamics: gravity, restitution, Coulomb friction and rolling, solved with impulses at a fixed
/// time step. Friction acts at the contact point, so rolling without slipping emerges from the solver rather
/// than being imposed: a ball on a slope accelerates at 5/7 g sin(theta), and its texture turns accordingly.
module Physics =
    /// Static geometry the spheres collide with.
    type Collider =
        /// The half-space below the plane n . x = offset, with `normal` pointing out of the solid.
        | Plane of normal: Vector * offset: float
        /// A box with the given centre, half extents along its local axes, and orientation.
        | OrientedBox of centre: Point * halfExtents: Vector * rotation: Quaternion

    type Body =
        { Name: string
          Radius: float
          Mass: float
          Position: Point
          Velocity: Vector
          AngularVelocity: Vector
          Orientation: Quaternion
          /// Coefficient of restitution for normal impacts (0 = dead, 1 = perfectly elastic).
          Restitution: float
          /// Coulomb friction coefficient at contacts.
          Friction: float
          /// Fractional speed lost per second while in contact, so rolling balls eventually stop.
          RollingResistance: float }

    let body name radius (position: Point) =
        { Name = name; Radius = radius; Mass = 1.; Position = position; Velocity = Vector.Zero
          AngularVelocity = Vector.Zero; Orientation = Quaternion.identity; Restitution = 0.7
          Friction = 0.6; RollingResistance = 0.15 }

    type World =
        { Gravity: Vector
          Colliders: Collider list
          Bodies: Body list
          /// Simulation step in seconds.
          Step: float }

    let world bodies colliders =
        { Gravity = Vector(0., -9.81, 0.); Colliders = colliders; Bodies = bodies; Step = 1. / 480. }

    /// One recorded body state.
    type Sample =
        { Time: float
          Position: Point
          Velocity: Vector
          Orientation: Quaternion
          AngularVelocity: Vector
          /// Contact normal (out of the collider) if the body touches something at this instant.
          Contact: Vector option
          /// Normal speed of the strongest impact since the previous sample (0 if none).
          ImpactSpeed: float
          ImpactNormal: Vector }

    type Trajectory = { Body: Body; Samples: Sample[] }

    /// Normal (out of the collider) and penetration depth of a sphere against a collider, if touching.
    let contact (centre: Point) radius (collider: Collider) =
        match collider with
        | Plane (normal, offset) ->
            let n = normal.Normalise
            let distance = Vector(centre.X, centre.Y, centre.Z) * n - offset
            if distance < radius then Some (n, radius - distance) else None
        | OrientedBox (boxCentre, half, rotation) ->
            let inverse = Quaternion.conjugate rotation
            let local = Quaternion.rotate inverse (centre - boxCentre)
            let clamp (v: float) h = max -h (min h v)
            let closest = Vector(clamp local.X half.X, clamp local.Y half.Y, clamp local.Z half.Z)
            let offset = local - closest
            let distance = offset.Magnitude
            if distance > 0. then
                if distance < radius then Some (Quaternion.rotate rotation (offset / distance), radius - distance) else None
            else
                // Centre inside the box: push out through the nearest face.
                let depths = [| half.X - abs local.X, Vector(float (sign local.X), 0., 0.)
                                half.Y - abs local.Y, Vector(0., float (sign local.Y), 0.)
                                half.Z - abs local.Z, Vector(0., 0., float (sign local.Z)) |]
                let depth, axis = depths |> Array.minBy fst
                let axis = if axis.IsZero then Vector(0., 1., 0.) else axis
                Some (Quaternion.rotate rotation axis, radius + depth)

    type private State =
        { mutable Position: Point
          mutable Velocity: Vector
          mutable AngularVelocity: Vector
          mutable Orientation: Quaternion
          mutable Contact: Vector option
          mutable ImpactSpeed: float
          mutable ImpactNormal: Vector
          /// How long the body has been nearly still while touching something.
          mutable SlowTime: float
          Body: Body
          InverseMass: float
          InverseInertia: float }

    /// Below this approach speed an impact does not bounce, so resting contacts settle instead of jittering.
    let private restingSpeed = 0.25

    /// Applies an impulse at offset `r` from the body's centre.
    let private applyImpulse (s: State) (impulse: Vector) (r: Vector) =
        s.Velocity <- s.Velocity + s.InverseMass * impulse
        s.AngularVelocity <- s.AngularVelocity + s.InverseInertia * (r % impulse)

    let private recordImpact (s: State) speed (normal: Vector) =
        if speed > s.ImpactSpeed then
            s.ImpactSpeed <- speed
            s.ImpactNormal <- normal

    /// Resolves a contact against static geometry with normal `n` (pointing into the body) and depth `depth`.
    let private resolveStatic (s: State) (n: Vector) depth =
        let b = s.Body
        s.Position <- s.Position + depth * n
        let r = -b.Radius * n
        let contactVelocity = s.Velocity + (s.AngularVelocity % r)
        let vn = contactVelocity * n
        if vn < 0. then
            recordImpact s -vn n
            let e = if -vn < restingSpeed then 0. else b.Restitution
            // r is parallel to n, so the angular term vanishes from the normal effective mass.
            let jn = -(1. + e) * vn / s.InverseMass
            applyImpulse s (jn * n) r
            // Friction: the impulse that would stop tangential slip, capped by the Coulomb cone.
            let contactVelocity = s.Velocity + (s.AngularVelocity % r)
            let tangential = contactVelocity - (contactVelocity * n) * n
            let slip = tangential.Magnitude
            if slip > 1e-12 then
                let direction = tangential / slip
                // Effective mass along the tangent for a sphere: 1/m + r^2/I.
                let effective = s.InverseMass + b.Radius * b.Radius * s.InverseInertia
                let jt = min (slip / effective) (b.Friction * jn)
                applyImpulse s (-jt * direction) r
        s.Contact <- Some n

    let private resolvePair (a: State) (b: State) =
        let delta = b.Position - a.Position
        let distance = delta.Magnitude
        let reach = a.Body.Radius + b.Body.Radius
        if distance < reach && distance > 0. then
            let n = delta / distance
            let depth = reach - distance
            let total = a.InverseMass + b.InverseMass
            a.Position <- a.Position - (depth * a.InverseMass / total) * n
            b.Position <- b.Position + (depth * b.InverseMass / total) * n
            let ra, rb = a.Body.Radius * n, -b.Body.Radius * n
            let va = a.Velocity + (a.AngularVelocity % ra)
            let vb = b.Velocity + (b.AngularVelocity % rb)
            let vn = (vb - va) * n
            if vn < 0. then
                recordImpact a -vn -n
                recordImpact b -vn n
                let e = if -vn < restingSpeed then 0. else 0.5 * (a.Body.Restitution + b.Body.Restitution)
                let jn = -(1. + e) * vn / total
                applyImpulse a (-jn * n) ra
                applyImpulse b (jn * n) rb
                let va = a.Velocity + (a.AngularVelocity % ra)
                let vb = b.Velocity + (b.AngularVelocity % rb)
                let relative = vb - va
                let tangential = relative - (relative * n) * n
                let slip = tangential.Magnitude
                if slip > 1e-12 then
                    let direction = tangential / slip
                    let effective =
                        total + a.Body.Radius * a.Body.Radius * a.InverseInertia + b.Body.Radius * b.Body.Radius * b.InverseInertia
                    let mu = sqrt (a.Body.Friction * b.Body.Friction)
                    let jt = min (slip / effective) (mu * jn)
                    applyImpulse a (jt * direction) ra
                    applyImpulse b (-jt * direction) rb

    let private integrateOrientation (q: Quaternion) (w: Vector) dt =
        // dq/dt = 1/2 (0, w) q, with w in world space.
        let dq = Quaternion.multiply { X = w.X; Y = w.Y; Z = w.Z; W = 0. } q
        Quaternion.normalise
            { X = q.X + 0.5 * dt * dq.X; Y = q.Y + 0.5 * dt * dq.Y
              Z = q.Z + 0.5 * dt * dq.Z; W = q.W + 0.5 * dt * dq.W }

    /// Runs the world for `duration` seconds and records every body `sampleRate` times per second.
    let simulate (world: World) (duration: float) (sampleRate: float) =
        if not (world.Step > 0.) then invalidArg (nameof world) "The simulation step must be positive."
        if not (sampleRate > 0.) || not (duration >= 0.) then invalidArg (nameof sampleRate) "Sample rate and duration must be positive."
        let names = world.Bodies |> List.map (fun b -> b.Name)
        if List.distinct names <> names then invalidArg (nameof world) "Body names must be unique."
        let states =
            world.Bodies |> List.map (fun b ->
                if not (b.Radius > 0. && b.Mass > 0.) then invalidArg (nameof world) $"Body {b.Name} needs a positive radius and mass."
                { Position = b.Position; Velocity = b.Velocity; AngularVelocity = b.AngularVelocity
                  Orientation = Quaternion.normalise b.Orientation; Contact = None; ImpactSpeed = 0.; ImpactNormal = Vector.Zero
                  SlowTime = 0.; Body = b; InverseMass = 1. / b.Mass; InverseInertia = 1. / (0.4 * b.Mass * b.Radius * b.Radius) })
            |> Array.ofList
        let sampleCount = int (floor (duration * sampleRate + 1e-9)) + 1
        let samples = states |> Array.map (fun _ -> ResizeArray<Sample>(sampleCount))
        let record time =
            for i in 0 .. states.Length - 1 do
                let s = states.[i]
                samples.[i].Add
                    { Time = time; Position = s.Position; Velocity = s.Velocity; Orientation = s.Orientation
                      AngularVelocity = s.AngularVelocity; Contact = s.Contact; ImpactSpeed = s.ImpactSpeed; ImpactNormal = s.ImpactNormal }
                s.ImpactSpeed <- 0.
        let dt = world.Step
        let mutable time = 0.
        record 0.
        for sample in 1 .. sampleCount - 1 do
            let target = float sample / sampleRate
            while time < target - 1e-12 do
                let h = min dt (target - time)
                for s in states do
                    s.Velocity <- s.Velocity + h * world.Gravity
                    s.Position <- s.Position + h * s.Velocity
                    s.Orientation <- integrateOrientation s.Orientation s.AngularVelocity h
                    s.Contact <- None
                for i in 0 .. states.Length - 1 do
                    for j in i + 1 .. states.Length - 1 do
                        resolvePair states.[i] states.[j]
                for s in states do
                    for collider in world.Colliders do
                        match contact s.Position s.Body.Radius collider with
                        | Some (n, depth) -> resolveStatic s n depth
                        | None -> ()
                    if s.Contact.IsSome then
                        let keep = max 0. (1. - s.Body.RollingResistance * h)
                        s.Velocity <- keep * s.Velocity
                        s.AngularVelocity <- keep * s.AngularVelocity
                        // Put a body that has stayed nearly still to sleep so it does not creep.
                        if s.Velocity.Magnitude < 0.02 && s.AngularVelocity.Magnitude * s.Body.Radius < 0.02 then
                            s.SlowTime <- s.SlowTime + h
                            if s.SlowTime > 0.3 then
                                s.Velocity <- Vector.Zero
                                s.AngularVelocity <- Vector.Zero
                        else s.SlowTime <- 0.
                    else s.SlowTime <- 0.
                time <- time + h
            record target
        Array.map2 (fun (s: State) (recorded: ResizeArray<Sample>) -> { Body = s.Body; Samples = recorded.ToArray() }) states samples

    /// Kinetic (linear and rotational) plus potential energy of a recorded sample.
    let energy (world: World) (body: Body) (sample: Sample) =
        let inertia = 0.4 * body.Mass * body.Radius * body.Radius
        let height = -(Vector(sample.Position.X, sample.Position.Y, sample.Position.Z) * world.Gravity)
        0.5 * body.Mass * sample.Velocity.MagnitudeSquared + 0.5 * inertia * sample.AngularVelocity.MagnitudeSquared + body.Mass * height
