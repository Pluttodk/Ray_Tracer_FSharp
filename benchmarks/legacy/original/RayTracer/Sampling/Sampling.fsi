module Tracer.Basics.Sampling

type Sampler =
    new: samples:(float*float)[][] -> Sampler
    member Current: float * float
    member NextSet: unit -> (float * float) []
    member Next: unit -> (float * float)
    member SampleCount: int
    member SetCount : int

val setRandomSeed : int -> unit
val regular : int -> Sampler
val random : int -> int -> Sampler
val jittered : int -> int -> Sampler
val nRooks : int -> int -> Sampler
val multiJittered : int -> int -> Sampler
val mapToDisc : (float * float) -> (float * float)
val mapToHemisphere : (float * float) -> float -> (float * float * float)