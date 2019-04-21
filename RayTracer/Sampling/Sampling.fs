module Tracer.Basics.Sampling

open System
open System.Drawing
open System.Threading

let mutable rand = new Random()

let setRandomSeed seed = rand <- new Random(seed)

type Sampler(samples : (float*float)[][]) =
    let sampleSetCount = samples.Length
    let mutable sampleIndices = Array.create 40 0 // Set an intial thread indexing array of 40.
    let mutable currentSet = Array.create 40 0
    let mutable currentSample: (float * float) = (0., 0.)
    let sampleCount = samples.[0].Length
    let mutex = new Mutex()

    member this.NextSet() =
        let threadIndex = Thread.CurrentThread.GetHashCode()

        if threadIndex >= currentSet.Length then
            // If threads happen to have a hash code larger than the length of our indexing aray, 
            // we expand the array using Mutex for safety.
            mutex.WaitOne() |> ignore
            currentSet <- Array.append currentSet (Array.create (threadIndex*2-currentSet.Length) 0)
            mutex.ReleaseMutex()

        // Use the thread's hash code as index in an array where we keep track of the current sample set for each thread.
        let setIndex = currentSet.[threadIndex]
        // We perform modulo here in order to avoid OutOfIndex exceptions when running with several threads.
        let samples = samples.[setIndex % this.SetCount]
        currentSet.[threadIndex] <- setIndex + 1
        samples

    member this.Next() = 
        let threadIndex = Thread.CurrentThread.GetHashCode()
        if threadIndex >= sampleIndices.Length then
            // If threads happen to have a hash code larger than the length of our indexing aray, 
            // we expand the array using Mutex for safety.
            mutex.WaitOne() |> ignore
            sampleIndices <- Array.append sampleIndices (Array.create (threadIndex*2-sampleIndices.Length) 0)
            mutex.ReleaseMutex()

        // Use the thread's hash code as index in an array where we keep track of the current sample for each thread.
        let currentSampleIndex = sampleIndices.[threadIndex]
        // We perform modulo here in order to avoid OutOfIndex exceptions when running with several threads.
        let setIndex = (currentSampleIndex / sampleCount) % sampleSetCount
        let sampleIndex = currentSampleIndex % sampleCount
        let sample = samples.[setIndex].[sampleIndex]
        sampleIndices.[threadIndex] <- currentSampleIndex + 1
        currentSample <- sample
        sample

    member this.Current = 
        currentSample

    member this.SampleCount = sampleCount
    member this.SetCount = sampleSetCount

let regular (ni:int) =
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
    let rand = new Random() // We create a new Random here, for help with testing.
    for i in 0..set.Length-1 do
        let samples = set.[i]
        for j in 1..samples.Length-1 do
            let r = rand.Next(j)
            let temp = samples.[r]
            samples.[r] <- samples.[j]
            samples.[j] <- temp
        set.[i] <- samples
    for i in 1..set.Length-1 do
            let r = rand.Next(i)
            let temp = set.[r]
            set.[r] <- set.[i]
            set.[i] <- temp
    new Sampler(set)

let random n sn =
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

// Returns the grid that a sample point lies within (for jittered/nRooks).
let getGridCell (v:float) max = int(v*(float max))

// After shuffling a sample point in nRooks, we check that no other samples lie within these spots.
let illegalSpots = [|(2, 1);(-2, 1);(2, -1);(-2, -1);(1, 2);(1, -2);(-1, 2);(-1, -2)|]

// Returns an array of valid spots that a 'rook' can move to in nRooks sampling.
let rec getLegalSpots i (samples:(float * float) array) =
    let n = samples.Length
    
    let mutable result = [||]
    for j in 0..i-1 do
        let xVal, _ = samples.[j]
        let xGrid = getGridCell xVal n
        let checkValidity k =
            if k < n && k > 0 then
                let (x2, y2) = samples.[k]
                let xGrid2 = getGridCell x2 n
                let yGrid2 = getGridCell y2 n
                not (Array.exists (fun (xi, yi) ->
                    xGrid2 = (xGrid+xi) && yGrid2 = (i+yi)) illegalSpots)
            else true
        result <- if checkValidity (i-1) && checkValidity (i-2) && checkValidity (i+1) && checkValidity (i+2) then Array.append result [|j|] else result
    result

// Initial diagonal shuffling for nRooks.
let shuffleDiagonals (samples:(float * float) []) =
    let n = samples.Length
    
    for i in n-1..-1..0 do
        let shufX = rand.Next(i)
        let _, replY = samples.[shufX]
        let  _, currentY = samples.[i]
        samples.[shufX] <- (getJitteredValue shufX n, currentY)
        samples.[i] <- (getJitteredValue i n, replY)
    for i in n-1..-1..0 do
        let legalSpots = getLegalSpots i samples
        if not (Array.isEmpty legalSpots) then
            let shufY = legalSpots.[rand.Next(legalSpots.Length)]
            let replX, _ = samples.[shufY]
            let currentX, _ = samples.[i]
            samples.[shufY] <- (currentX, getJitteredValue shufY n)
            samples.[i] <- (replX, getJitteredValue i n)
    samples

let nRooks n sn =
    let sets = Array.create sn [|(0.0, 0.0)|]
    let rec loop k =
        let samples = Array.create n (0.0,0.0)
        let rec placeDiagonals = function
            | 0 -> samples.[0] <- (getJitteredValue 0 n, getJitteredValue 0 n)
            | c -> 
                samples.[c] <- (getJitteredValue c n, getJitteredValue c n)
                (placeDiagonals (c-1))
        placeDiagonals (n-1)
        sets.[k] <- shuffleDiagonals samples
        if k > 0 then loop (k-1)
    loop (sn-1)
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
    let x, y = (2.0*x-1.0, 2.0*y-1.0)
    let PI_QUART = Math.PI/4.0
    let (r, theta) = 
        match (x > -y, x > y) with
            | true, true    -> (x, PI_QUART * (y/x))
            | true, false   -> (y, PI_QUART * (2.0-x/y))
            | false, false  -> (-x, PI_QUART * (4.0+y/x))
            | false, true   -> (-y, PI_QUART * (6.0-x/y))
    (r * Math.Cos(theta), r*Math.Sin(theta))
    
let mapToHemisphere (x, y) e =
    let E_VAL = 1.0/(e+1.0)
    let phi = 2.0*Math.PI*x
    let theta = Math.Acos((1.0-y)**E_VAL)
    (Math.Sin(theta) * Math.Cos(phi), Math.Sin(theta) * Math.Sin(phi), Math.Cos(theta))

// A series of helper methods to visualize the sampling as points on square/disc/sphere.
let drawSamples (sampler:Sampler) sampleMethod fileName =
    let size = 400
    let dotSize = 4
    let img = new Bitmap(size, size)
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
    img.Save(fileName)  

let drawCircle (img:Bitmap) size = 
    let SIZE_HALVED = float size/2.0
    for i in 1..int 360 do
        let theta = float i
        let x  =  int(SIZE_HALVED + (SIZE_HALVED-2.0) * Math.Cos(theta))
        let y  =  int(SIZE_HALVED + (SIZE_HALVED-2.0) * Math.Sin(theta))
        for i in [x-1..x+1] do
            for j in [y-1..y+1] do
                img.SetPixel(i, j, Color.Black)

let drawDiscSamples (sampler:Sampler) fileName =
    let size = 400
    let dotSize = 4
    let img = new Bitmap(size, size)
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
    img.Save(fileName)  

let drawSphereSamples (sampler:Sampler) e fileName above =
    let size = 400
    let dotSize = 4
    let img = new Bitmap(size, size)
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
    img.Save(fileName)

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