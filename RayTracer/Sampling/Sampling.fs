module Tracer.Basics.Sampling

open System
open System.Drawing
open System.Threading
open Tracer.Imaging

let mutable rand = new Random()

let setRandomSeed seed = rand <- new Random(seed)

type Sampler(samples : (float*float)[][]) =
    do
        if isNull samples || samples.Length = 0 || isNull samples.[0] || samples.[0].Length = 0 then
            invalidArg (nameof samples) "A sampler requires at least one nonempty sample set."
        let count = samples.[0].Length
        if samples |> Array.exists (fun set -> isNull set || set.Length <> count) then
            invalidArg (nameof samples) "All sample sets must have the same length."
        if samples |> Array.exists (Array.exists (fun (x, y) ->
            not (Double.IsFinite x && Double.IsFinite y) || x < 0. || x > 1. || y < 0. || y > 1.)) then
            invalidArg (nameof samples) "Sample coordinates must be finite and in [0,1]."
    let samples = samples |> Array.map Array.copy
    let sampleSetCount = samples.Length
    let sampleCount = samples.[0].Length
    let state = new ThreadLocal<struct (int64 * int64 * (float * float))>(fun () -> struct (0L, 0L, (0., 0.)))

    member this.NextSet() =
        let struct (sampleIndex, setIndex, current) = state.Value
        state.Value <- struct (sampleIndex, setIndex + 1L, current)
        samples.[int (setIndex % int64 sampleSetCount)]

    member this.Next() =
        let struct (index, setIndex, _) = state.Value
        let sample = samples.[int ((index / int64 sampleCount) % int64 sampleSetCount)].[int (index % int64 sampleCount)]
        state.Value <- struct (index + 1L, setIndex, sample)
        sample

    member this.Current =
        let struct (_, _, current) = state.Value
        current

    member this.SampleSetAt(key: uint64) =
        samples.[int (key % uint64 sampleSetCount)]

    member this.SampleAt(key: uint64, index: int) =
        if index < 0 then invalidArg (nameof index) "Sample index must be nonnegative."
        samples.[int (key % uint64 sampleSetCount)].[index % sampleCount]

    member this.SampleCount = sampleCount
    member this.SetCount = sampleSetCount

let mixKey (key: uint64) =
    let mutable value = key + 0x9e3779b97f4a7c15UL
    value <- (value ^^^ (value >>> 30)) * 0xbf58476d1ce4e5b9UL
    value <- (value ^^^ (value >>> 27)) * 0x94d049bb133111ebUL
    value ^^^ (value >>> 31)

let sampleKey (seed: int) (pixel: int) (sample: int) =
    let address = (uint64 (uint32 pixel) <<< 32) ||| uint64 (uint32 sample)
    mixKey (uint64 (uint32 seed) ^^^ mixKey address)

let sample2D (key: uint64) (dimension: int) =
    let toUnit (bits: uint64) = float (bits >>> 11) * (1. / 9007199254740992.)
    toUnit (mixKey (key ^^^ uint64 (dimension * 2))),
    toUnit (mixKey (key ^^^ uint64 (dimension * 2 + 1)))

let private validateCounts samples sets =
    if samples <= 0 then invalidArg (nameof samples) "Sample count must be positive."
    if sets <= 0 then invalidArg (nameof sets) "Sample set count must be positive."

let regular (ni:int) =
    validateCounts ni 1
    if int64 ni * int64 ni > int64 Int32.MaxValue then invalidArg (nameof ni) "The sample grid is too large."
    let n = float ni
    let samples = Array.create (ni*ni) (0.0, 0.0)
    let rec innerX x = 
        let rec innerY = function
            | 1 -> 
                samples.[ni*(x-1)] <- (float x/(n+1.0), 1.0/(n+1.0))
            | y -> 
                samples.[(ni*(x-1))+(y-1)] <- (float x/(n+1.0), float y/(n+1.0))
                innerY (y-1)
        match x with
        | 1 -> innerY ni
        | c ->
            innerY ni
            innerX (c-1)
    innerX ni
    new Sampler([|samples|])

// This method is called after every sample method is finished.
// It shuffles the sample sets and the samples within each set.
let createSampler (set:(float * float)[][]) =
    for i in 0..set.Length-1 do
        let samples = set.[i]
        for j in 1..samples.Length-1 do
            let r = rand.Next(j + 1)
            let temp = samples.[r]
            samples.[r] <- samples.[j]
            samples.[j] <- temp
        set.[i] <- samples
    for i in 1..set.Length-1 do
            let r = rand.Next(i + 1)
            let temp = set.[r]
            set.[r] <- set.[i]
            set.[i] <- temp
    new Sampler(set)

let random n sn =
    validateCounts n sn
    let sets = Array.create sn [|(0.0, 0.0)|]
    let rec loop k =
        let samples = Array.create n (0.0, 0.0)
        let rec inner = function
            | 0 -> samples.[0] <- (rand.NextDouble(), rand.NextDouble())
            | c -> 
                samples.[c] <- (rand.NextDouble(), rand.NextDouble())
                inner (c-1)
        inner (n-1)
        sets.[k] <- samples
        if k > 0 then loop (k-1)
    loop (sn-1)
    createSampler sets

// Returns a jittered sample point, that lies within a given grid cell, in relation to the max grid value.
let getJitteredValue (cell:int) (max:int) = (rand.NextDouble()/float max) + ((1.0/float max) * float cell)

let jittered n sn =
    validateCounts n sn
    if int64 n * int64 n > int64 Int32.MaxValue then invalidArg (nameof n) "The sample grid is too large."
    let sets = Array.create sn [|(0.0, 0.0)|]
    let pn = int (float n**2.0)
    let rec loop k =
        let samples = Array.create pn (0.0 ,0.0)
        let rec innerX x = 
            let rec innerY = function
                | 0 -> samples.[x*n] <- (getJitteredValue x n, getJitteredValue 0 n)
                | y -> 
                    samples.[(x*n) + y] <- (getJitteredValue x n, getJitteredValue y n)
                    (innerY (y-1))
            match x with
            | 0 -> innerY (n-1)
            | c ->  
                innerY (n-1)
                (innerX (c-1))
        innerX (n-1)
        sets.[k] <- samples
        if k > 0 then loop (k-1)
    loop (sn-1)
    createSampler sets

let nRooks n sn =
    validateCounts n sn
    let sets =
        Array.init sn (fun _ ->
            let samples = Array.init n (fun i -> getJitteredValue i n, getJitteredValue i n)
            for i = n - 1 downto 1 do
                let j = rand.Next(i + 1)
                let x, y = samples.[i]
                let otherX, otherY = samples.[j]
                samples.[i] <- x, otherY
                samples.[j] <- otherX, y
            samples)
    createSampler sets

(*
    This method lives up to the lecture notes PDF's version of multi-jittered sampling, 
    but according to what I could read on the web, this is not the optimal multi-jittered
    method, because samples are not as evenly distributed. The method below this one 
    (that we use in our program) is supposedly more effective.
*)
let shuffleMultiPDF (samples:(float * float) []) n =
    for j in 0..n-1 do
        for i in 0..n-1 do
            let shuf = (j + rand.Next(n-j)) * n + i
            let current = j * n + i
            let replX, replY = samples.[shuf]
            let currentX, currentY = samples.[current]
            samples.[shuf] <- (currentX, replY)
            samples.[current] <- (replX, currentY)

    for i in 0..n-1 do
        for j in 0..n-1 do
            let shuf = j * n + (i + rand.Next(n-i))
            let current = j * n + i
            let replX, replY = samples.[shuf]
            let currentX, currentY = samples.[current]
            samples.[shuf] <- (replX, currentY)
            samples.[current] <- (currentX, replY)

    samples

(* 
    This method uses correlated shuffling to improve upon the basic idea of multi-jittered
    sampling. This ensures a more evenly distributed set of samples. The change from above is
    that this method shuffles using the same randomly chosen value for an entire row/column, 
    instead of shuffling with a new random value for every sample point.

    Correlated extension is found here: https://graphics.pixar.com/library/MultiJitteredSampling/paper.pdf.
 *)
let shuffleMulti (samples:(float * float) []) n =
    for j in 0..n-1 do
        let k = (j + rand.Next(n-j))
        for i in 0..n-1 do
            let shuf = k * n + i
            let current = j * n + i
            let replX, replY = samples.[shuf]
            let currentX, currentY = samples.[current]
            samples.[shuf] <- (currentX, replY)
            samples.[current] <- (replX, currentY)

    for i in 0..n-1 do
        let k = (i + rand.Next(n-i))
        for j in 0..n-1 do
            let shuf = j * n + k
            let current = j * n + i
            let replX, replY = samples.[shuf]
            let currentX, currentY = samples.[current]
            samples.[shuf] <- (replX, currentY)
            samples.[current] <- (currentX, replY)
    samples

(* 
    This method sets up the initial diagonal distribution of samples, and calls
    shuffleMulti to perform the multiJittered shuffling.
*)
let multiJittered n sn =
    validateCounts n sn
    if int64 n * int64 n > int64 Int32.MaxValue then invalidArg (nameof n) "The sample grid is too large."
    let sets = Array.create sn [|(0.0, 0.0)|]
    let ns = int (float (n)**2.0)
    let rec loop k = 
        let samples = Array.create ns (0.0, 0.0)
        let rec placeDiag = function
            | 0 -> samples.[0] <- (getJitteredValue 0 ns, getJitteredValue 0 ns)
            | c -> 
                samples.[c] <- (getJitteredValue (((c%n)*n)+(c/n)) ns, getJitteredValue c ns)
                placeDiag (c-1)
        placeDiag (ns-1)
        sets.[k] <- shuffleMulti samples n
        if k > 0 then loop (k-1)
    loop (sn-1)
    createSampler sets

let mapToDisc (x, y) =
    if not (Double.IsFinite x && Double.IsFinite y) || x < 0. || x > 1. || y < 0. || y > 1. then
        invalidArg "sample" "Disk samples must be finite and in [0,1]."
    let x, y = (2.0*x-1.0, 2.0*y-1.0)
    if x = 0. && y = 0. then 0., 0.
    else
        let r, theta =
            if abs x > abs y then x, (Math.PI / 4.) * (y / x)
            else y, (Math.PI / 2.) - (Math.PI / 4.) * (x / y)
        r * Math.Cos theta, r * Math.Sin theta
    
let mapToHemisphere (x, y) e =
    if not (Double.IsFinite x && Double.IsFinite y && Double.IsFinite e) || x < 0. || x > 1. || y < 0. || y > 1. || e < 0. then
        invalidArg "sample" "Hemisphere samples must be in [0,1] with a finite nonnegative exponent."
    let phi = 2.0*Math.PI*x
    let cosine = (1.0 - y) ** (1.0 / (e + 1.0))
    let sine = sqrt (max 0. (1. - cosine * cosine))
    sine * Math.Cos phi, sine * Math.Sin phi, cosine

// A series of helper methods to visualize the sampling as points on square/disc/sphere.
let drawSamples (sampler:Sampler) sampleMethod (fileName:string) =
    let size = 400
    let dotSize = 4
    use img = new RgbImage(size, size)
    for i in [0..size-1] do
            for j in [0..size-1] do
                img.SetPixel(i, j, Color.White)
    let drawGrid n thickness =
        for i in [1..n-1] do
            for j in [0..size-1] do
                for k in [(-thickness/2)..(thickness/2)] do
                    img.SetPixel(((size/n)*i)+k, j, Color.Black)
                    img.SetPixel(j, ((size/n)*i)+k, Color.Black)
    if sampler.SampleCount < 64 then
        if sampleMethod = "jittered" then drawGrid (int (Math.Sqrt(float sampler.SampleCount))) 1
        else if sampleMethod = "nrooks" then drawGrid sampler.SampleCount 1
        else if sampleMethod = "multi" then
            drawGrid (int (Math.Sqrt(float sampler.SampleCount))) 2
            drawGrid sampler.SampleCount 1
    let samples = sampler.NextSet()
    for (sx, sy) in samples do
        let x = int (float size*sx)
        let y = int (float size*sy)
        for i in [x-(dotSize/2)..x+(dotSize/2)] do
            for j in [y-(dotSize/2)..y+(dotSize/2)] do
                if i >= 0 && j >= 0 && i < size && j < size then img.SetPixel(i, j, Color.Red)
    img.SavePng(fileName)

let drawCircle (img:RgbImage) size =
    let SIZE_HALVED = float size/2.0
    for i in 1..int 360 do
        let theta = float i
        let x  =  int(SIZE_HALVED + (SIZE_HALVED-2.0) * Math.Cos(theta))
        let y  =  int(SIZE_HALVED + (SIZE_HALVED-2.0) * Math.Sin(theta))
        for i in [x-1..x+1] do
            for j in [y-1..y+1] do
                img.SetPixel(i, j, Color.Black)

let drawDiscSamples (sampler:Sampler) (fileName:string) =
    let size = 400
    let dotSize = 4
    use img = new RgbImage(size, size)
    for i in 0..size-1 do
            for j in [0..size-1] do
                img.SetPixel(i, j, Color.White)
    drawCircle img size
    let samples = sampler.NextSet()
    for (sx, sy) in samples do
        let sx, sy = mapToDisc (sx, sy)
        let x = int (float (size)*((sx+1.0)/2.0))
        let y = int (float (size)*((sy+1.0)/2.0))
        for i in [x-(dotSize/2)..x+(dotSize/2)] do
            for j in [y-(dotSize/2)..y+(dotSize/2)] do
                if i >= 0 && j >= 0 && i < size && j < size then img.SetPixel(i, j, Color.Red)
    img.SavePng(fileName)

let drawSphereSamples (sampler:Sampler) e (fileName:string) above =
    let size = 400
    let dotSize = 4
    use img = new RgbImage(size, size)
    for i in 0..size-1 do
            for j in [0..size-1] do
                img.SetPixel(i, j, Color.White)
    drawCircle img size
    let samples = sampler.NextSet()
    for (sx, sy) in samples do
        let sx, sy, sz = mapToHemisphere (sx, sy) e
        let x = int (float (size)*((sx+1.0)/2.0))
        let sv = if above then sy else sz
        let y = int (float (size)*((sv+1.0)/2.0))
        for i in [x-(dotSize/2)..x+(dotSize/2)] do
            for j in [y-(dotSize/2)..y+(dotSize/2)] do
                if i >= 0 && j >= 0 && i < size && j < size then img.SetPixel(i, j, Color.Red)
    img.SavePng(fileName)

let main argsv =
    if Array.isEmpty argsv then 
        printfn "Error: No arguments given! Expected: [sampleMethod] [sampleAmount]."
        0
    else
    let method = argsv.[0]
    let amount = Int32.Parse(argsv.[1])
    let sets = if argsv.Length > 2 then Int32.Parse(argsv.[2]) else 1
    let fileName = "sampletest.png"
    let samples = 
        match method with
            | "regular" -> regular amount
            | "random"  -> random amount sets
            | "jittered" -> jittered amount sets
            | "nrooks"  -> nRooks amount sets
            | "multi"   -> multiJittered amount sets
            | _ -> regular 4
    if argsv.Length > 3 then
        if argsv.[3] = "disc" then drawDiscSamples samples fileName
        else if argsv.[3] = "sphere" then
            let e = if argsv.Length = 5 then float (Int32.Parse argsv.[4]) else 0.0
            drawSphereSamples samples e fileName true
    else drawSamples samples method fileName
    0