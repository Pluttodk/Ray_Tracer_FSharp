module ParticleTests

open Assert
open Tracer.Basics
open Tracer.Animation

// A flier skimming a flat snowfield at 40 units, moving along +x at 50 units/s.
let private source : Particles.Source =
    { Position = fun t -> Vector(50. * t, 40., 0.)
      Ground = fun _ _ -> 0.
      Snow = fun _ _ -> 1. }

let private settings = { Particles.defaults with Start = 1.; Stop = 3.; PerEvent = 20 }

let private same (a: Particles.Particle[]) (b: Particles.Particle[]) =
    a.Length = b.Length && Array.forall2 (fun (p: Particles.Particle) (q: Particles.Particle) -> p.Centre = q.Centre && p.Radius = q.Radius) a b

let deterministicTests () =
    let a = Particles.at settings source 2.2
    Assert.True (a.Length > 0, "the plume has particles while the flier skims the snow")
    Assert.True (same a (Particles.at settings source 2.2), "the simulation is deterministic for a seed")
    Assert.True (not (same a (Particles.at { settings with Seed = 6 } source 2.2)), "a different seed gives a different plume")
    // Pure function of t: asking for other frames first changes nothing.
    Particles.at settings source 1.5 |> ignore
    Assert.True (same a (Particles.at settings source 2.2), "a frame does not depend on which frames were built before it")

let emissionTests () =
    Assert.True ((Particles.at settings source 0.99).Length = 0, "no particles exist before emission starts")
    let high = { source with Position = fun t -> Vector(50. * t, 400., 0.) }
    Assert.True ((Particles.at settings high 2.).Length = 0, "no particles when the flier is too high to lift snow")
    let bare = { source with Snow = fun _ _ -> 0. }
    Assert.True ((Particles.at settings bare 2.).Length = 0, "no particles without snow")

let boundedTests () =
    let mutable worst = 0
    for i in 0 .. 200 do
        worst <- max worst (Particles.at settings source (0.05 * float i)).Length
    Assert.True (worst <= Particles.maxLive settings, "the live particle count is bounded")
    let ps = Particles.at settings source 2.5
    Assert.True (ps |> Array.forall (fun p -> p.Centre.Y > 0. && p.Age >= 0. && p.Age < p.Life), "particles stay above the ground within their life")

let fadeTests () =
    Assert.True ((Particles.at settings source (settings.Stop + settings.MaxLife + 0.01)).Length = 0, "the plume has fully gone after the last life span")
    // Each particle's radius reaches zero at the end of its life, so nothing pops out.
    let early = Particles.at settings source 2.0
    Assert.True (early |> Array.forall (fun p -> p.Radius > 0.), "live particles have positive size")
    let nearEnd = Particles.at settings source 5.9
    Assert.True (nearEnd |> Array.forall (fun p -> p.Radius < 0.2 * settings.Radius), "particles are nearly gone shortly before the end")

let allTest () =
    deterministicTests ()
    emissionTests ()
    boundedTests ()
    fadeTests ()
