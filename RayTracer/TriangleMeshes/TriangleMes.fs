module TriangleMes

open Tracer.Basics
open Tracer.Basics.Acceleration
open System.Threading.Tasks
open PLYParser
open Tracer.BaseShape

type TriPoint (v : Vertex) = 
    inherit Point(v.x,v.y,v.z)
    member this.v : Vertex = v

type PLYTriangle (a: Point, b: Point, c: Point, t: Texture, smoothen, hasNormalWithin, hasTextureCoords) = 
    inherit Triangle(a,b,c, Material.None)
    override this.hitFunction r = 
        let oldHit = base.hitFunction r
        let alpha =  1. - base.beta - base.gamma
        let getNormal = 
            if(smoothen) then
                let vertexNormal = 
                    if (hasNormalWithin) then 
                        let na = 
                            let triangle = (a :?> TriPoint).v
                            Vector(triangle.nx.Value,triangle.ny.Value,triangle.nz.Value)
                        let nb = 
                            let triangle = (b :?> TriPoint).v
                            Vector(triangle.nx.Value,triangle.ny.Value,triangle.nz.Value)
                        let nc =
                            let triangle = (c :?> TriPoint).v
                            Vector(triangle.nx.Value,triangle.ny.Value,triangle.nz.Value)
                        [|na.Normalise;nb.Normalise;nc.Normalise|]
                    else
                        let na = (a :?> TriPoint).v.normal.Normalise
                        let nb = (b :?> TriPoint).v.normal.Normalise
                        let nc = (c :?> TriPoint).v.normal.Normalise
                        [|na;nb;nc|]
                let V = (( * ) alpha vertexNormal.[0] |> ( + ) (( * ) base.beta vertexNormal.[1]) |> ( + ) (( * ) base.gamma vertexNormal.[2]))
                (V).Normalise
            else oldHit.Normal
        let u,v = 
            if (hasTextureCoords) then 
                let triA = (a :?> TriPoint).v
                let triB = (b :?> TriPoint).v
                let triC = (c :?> TriPoint).v
                let alpha = 1. - base.beta - base.gamma
                let v = (alpha * triA.v.Value) + (base.beta * triB.v.Value) + (base.gamma * triC.v.Value)
                let u = (alpha * triA.u.Value) + (base.beta * triB.u.Value) + (base.gamma * triC.u.Value)
                v,u
            else 0.,0.
        if(oldHit.DidHit) then
            let material = (Textures.getFunc t) u v
            let hitpoint = HitPoint(r, oldHit.Time, getNormal, material, oldHit.Shape)
            hitpoint
        else HitPoint(r)

type BasePLYTriangle (a: Point, b: Point, c: Point,smoothen, hasNormalWithin, hasTextureCoords) = 
    inherit BaseTriangle(a,b,c)
    override this.toShape(tex:Texture) = 
        PLYTriangle(a,b,c,tex, smoothen, hasNormalWithin, hasTextureCoords) :> Shape

let createTriangles (triangleArray : Vertex array) (faceArray : int list array) (smooth:bool) (hasNormalWithin : bool) (hasTextureCoords : bool)= 
    let ar = Array.zeroCreate(faceArray.Length)
    Parallel.For(0, ar.Length, fun i ->
        let v1 = triangleArray.[faceArray.[i].[1]]
        let p1 = new TriPoint(v1)
        let v2 = triangleArray.[faceArray.[i].[2]]
        let p2 = new TriPoint(v2)
        let v3 = triangleArray.[faceArray.[i].[3]]
        let p3 = new TriPoint(v3)
        let triangle = new BasePLYTriangle((p1 :> Point) ,(p2 :> Point) ,(p3 :> Point), smooth, hasNormalWithin, hasTextureCoords)
        if (smooth && not hasNormalWithin) then
            v1.normal <- triangle.n
            v2.normal <- triangle.n
            v3.normal <- triangle.n
        ar.[i] <- (triangle)) |> ignore
    ar
        

let drawTriangles (filepath:string) (smoothen:bool) = 
    let drawTiming = true
    let drawTimer = new System.Diagnostics.Stopwatch()
    // Start timer for acceleration traverse measurement
    if drawTiming then 
        drawTimer.Start()
        printfn "# TriangleMes drawTriangles timing start"

    let test = parsePLY filepath
    let triangleArray = fst test
    let faceArray = snd test
    let hasNormalWithin = triangleArray.[0].nx.IsSome
    let hasTexture = triangleArray.[0].u.IsSome

    let ar = createTriangles triangleArray faceArray smoothen hasNormalWithin hasTexture
    let idOfShape = Acceleration.listOfKDTree.Length + 1


    let baseShape = {new BaseShape() with
        member this.toShape(tex) = 
            let newTriangle = Array.zeroCreate(ar.Length)
            let firstTriangle = (ar.[0]).toShape(tex)
            newTriangle.[0] <- firstTriangle
            let newPoints = Array.zeroCreate(2)
            newPoints.[0] <- firstTriangle.getBoundingBox().lowPoint
            newPoints.[1] <- firstTriangle.getBoundingBox().highPoint

            for i in 0..ar.Length-1 do
            //Parallel.For(0, ar.Length, fun i ->
                let triangle = (ar.[i]).toShape(tex)
                let triangleLowPoint = triangle.getBoundingBox().lowPoint 
                let triangleHightPoint = triangle.getBoundingBox().highPoint
                newPoints.[0] <- newPoints.[0].Lowest triangleLowPoint
                newPoints.[1] <- newPoints.[1].Highest triangleHightPoint
                newTriangle.[i] <- triangle(* ) |> ignore*)
            let sA = shapeArray(idOfShape, newTriangle, None)
            let accel = Acceleration.createAcceleration(sA)
            let shape = {new Shape() with
                member this.hitFunction r = 
                    traverseIAcceleration accel r newTriangle
                member this.getBoundingBox () = BBox(newPoints.[0],newPoints.[1])
                member this.isInside p = false
            }
            shape
    }
    // Stop timer for acceleration traverse measurement and print elapsed time
    if drawTiming then
        drawTimer.Stop()
        printfn "## TriangleMes drawTriangles in %f seconds" drawTimer.Elapsed.TotalSeconds
    baseShape