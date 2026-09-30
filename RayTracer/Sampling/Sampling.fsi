module Tracer.Basics.Sampling

type Sampler =
    new: samples:(float*float)[][] -> Sampler
    member Current: float * float
    member NextSet: unit -> (float * float) []
    member Next: unit -> (float * float)
    member SampleCount: int
    member SetCount : int
    member SampleSetAt: key:uint64 -> (float * float) []
    member SampleAt: key:uint64 * index:int -> float * float

val setRandomSeed : int -> unit
val mixKey : uint64 -> uint64
val sampleKey : seed:int -> pixel:int -> sample:int -> uint64
val sample2D : key:uint64 -> dimension:int -> float * float
val regular : int -> Sampler
val random : int -> int -> Sampler
val jittered : int -> int -> Sampler
val nRooks : int -> int -> Sampler
val multiJittered : int -> int -> Sampler
val mapToDisc : (float * float) -> (float * float)
val mapToHemisphere : (float * float) -> float -> (float * float * float)