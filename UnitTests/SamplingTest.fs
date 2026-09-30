module SamplingTest

open Tracer.Basics.Sampling
open System
open Assert

let getGrid (v:float) max = int(v*(float max))

let isRookThreatened (samples:(float*float) []) i =
    let n = samples.Length
    let valX, valY = samples.[i]
    let gridX, gridY = getGrid valX n, getGrid valY n
    samples |> Array.mapi (fun index (x, y) ->
        index <> i && (gridX = getGrid x n || gridY = getGrid y n))
    |> Array.exists id

// Returns actual jittered array vs. expected array
let getJitteredEvaluation (samples:(float*float) []) =
    let n = int(Math.Sqrt (float samples.Length))
    let gridExpected = [|for x in 0..n-1 do for y in 0..n-1 do yield (x, y)|]
    let gridValues = [|for (x, y) in samples do yield (getGrid x n, getGrid y n)|]
    let gridExpected = Array.sort gridExpected
    let gridValues = Array.sort gridValues
    (gridExpected, gridValues)

let allTest () =
    let jittered_JitteredPropertyIsMaintained =
        let sampler = jittered 4 1
        let samples = [|for i in 0..sampler.SampleCount-1 do yield sampler.Next()|]
        let expected, actual = getJitteredEvaluation samples
        Assert.Equal (expected, actual, "jittered_JitteredPropertyIsMaintained")

    let nRooks_NRooksPropertyIsMaintained =
        setRandomSeed 42 // With this seed, nRooks property will be challenged.
        let sampler = nRooks 8 1
        let samples = [|for i in 0..sampler.SampleCount-1 do yield sampler.Next()|]
        let mutable result = true
        for i in 0..sampler.SampleCount-1 do
            if isRookThreatened samples i then result <- false
        Assert.True (result, "nRooks_NRooksPropertyIsMaintained")

    let multiJittered_JitteredPropertyIsMaintained =
        let sampler = multiJittered 4 1
        let samples = [|for i in 0..sampler.SampleCount-1 do yield sampler.Next()|]
        let expected, actual = getJitteredEvaluation samples
        Assert.Equal (expected, actual, "multiJittered_JitteredPropertyIsMaintained")

    let multiJittered_NRooksPropertyIsMaintained =
        setRandomSeed 42 // With this seed, nRooks property will be challenged.
        let sampler = multiJittered 4 1
        let samples = [|for i in 0..sampler.SampleCount-1 do yield sampler.Next()|]
        let mutable result = true
        for i in 0..sampler.SampleCount-1 do
            if isRookThreatened samples i then result <- false
        Assert.True (result, "multiJittered_NRooksPropertyIsMaintained")

    let sampleSets_SeedRepeatsSetOrder =
        setRandomSeed 19
        let sampler = multiJittered 2 2
        let sets1 = [|for j in 0..sampler.SetCount-1 do yield [|for i in 0..sampler.SampleCount-1 do yield sampler.Next()|]|]
        let sample1_1 = Array.sort sets1.[0]
        let sample1_2 = Array.sort sets1.[1]
        
        setRandomSeed 19
        let sampler = multiJittered 2 2
        let sets2 = [|for j in 0..sampler.SetCount-1 do yield [|for i in 0..sampler.SampleCount-1 do yield sampler.Next()|]|]
        let sample2_1 = Array.sort sets2.[0]
        let sample2_2 = Array.sort sets2.[1]

        Assert.Equal (sample1_1, sample2_1, "sampleSets_SeedRepeatsSetOrder1")
        Assert.Equal (sample1_2, sample2_2, "sampleSets_SeedRepeatsSetOrder2")
        Assert.Equal (sets1, sets2, "sampleSets_SeedRepeatsExactOrder")

    let sampleSets_SetsAreCorrectSize = 
        let sampler = multiJittered 2 4
        Assert.Equal (4, sampler.SetCount, "sampleSets_SetsAreCorrectSize")

    let sampleSets_SeedControlsSamples =
        setRandomSeed 19
        let sampler = multiJittered 4 2
        let samples1 = [|for i in 0..sampler.SampleCount-1 do yield sampler.Next()|]

        setRandomSeed 19
        let sampler = multiJittered 4 2
        let samples2 = [|for i in 0..sampler.SampleCount-1 do yield sampler.Next()|]

        Assert.Equal (samples1, samples2, "sampleSets_SeedControlsExactSampleOrder")
        setRandomSeed 20
        let changed = multiJittered 4 2
        let samples3 = Array.init changed.SampleCount (fun _ -> changed.Next())
        Assert.True (samples1 <> samples3, "sampleSets_DifferentSeedsChangeSamples")
    
    let sampleSets_SamplesAreCorrectSize =
        let sampler = multiJittered 4 1
        Assert.Equal (16, sampler.SampleCount, "sampleSets_SamplesAreCorrectSize")

    let mapToDisc_SamplesAreInCorrectQuadrants =
        let sampler = multiJittered 16 1
        let samples = [|for i in 0..sampler.SampleCount-1 do yield sampler.Next()|]
        let toDisc = Array.map mapToDisc samples
        let validArr = [|(true, true);(false, false);(true, false);(false, true)|]
        let result = Array.forall (fun (b1, b2) -> 
            Array.exists (fun (x, y) -> b1 = (x < 0.0) && b2 = (y < 0.0)) toDisc) validArr
        Assert.True (result, "mapToDisc_SamplesAreInCorrectQuadrants")

    let mapToDisc_SamplesAreInValidRange =
        let sampler = multiJittered 16 1
        let samples = [|for i in 0..sampler.SampleCount-1 do yield sampler.Next()|]
        let toDisc = Array.map mapToDisc samples
        let result = Array.forall (fun (x, y) ->
            Double.IsFinite x && Double.IsFinite y && x * x + y * y <= 1. + 1e-12) toDisc
        Assert.True (result, "mapToDisc_SamplesAreInValidRange")

    let Sampler_SamplerReturnsNextSample =
        let sampler = multiJittered 8 1

        let next = sampler.Next()
        Assert.True (next > (0., 0.), "Sampler_SamplerReturnsNextSample")

    let Sampler_SamplerReturnsCurrentSample =
        let sampler = multiJittered 8 1

        let next = sampler.Next()
        let current = sampler.Current
        Assert.Equal (next, current, "Sampler_SamplerReturnsCurrentSample")

    ()