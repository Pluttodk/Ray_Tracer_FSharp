module Tracer.Basics.TriangleMesh

open Tracer.Basics
open Tracer.Basics.Acceleration
open System.Threading.Tasks
open PLYParser
open Tracer.BaseShape
open Tracer.Basics.Textures

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
                        let na = (a :?> TriPoint).v.normal
                        let nb = (b :?> TriPoint).v.normal
                        let nc = (c :?> TriPoint).v.normal
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
            let hitpoint = HitPoint(r, oldHit.Time, getNormal, material, this, u, v)
            hitpoint
        else HitPoint(r)

type BasePLYTriangle (a: Point, b: Point, c: Point,smoothen, hasNormalWithin, hasTextureCoords) = 
    inherit BaseTriangle(a,b,c)
    let e = 0.000001
    let lx = (min a.X (min b.X c.X)) - e
    let ly = (min a.Y (min b.Y c.Y)) - e
    let lz = (min a.Z (min b.Z c.Z)) - e 

    let hx = (max a.X (max b.X c.X)) + e
    let hy = (max a.Y (max b.Y c.Y)) + e
    let hz = (max a.Z (max b.Z c.Z)) + e 
    member this.BBox = 
        BBox(Point(lx, ly, lz), Point(hx, hy, hz))
    override this.toShape(tex:Texture) = 
        PLYTriangle(a,b,c,tex, smoothen, hasNormalWithin, hasTextureCoords) :> Shape

let createTriangles (triangleArray : Vertex array) (faceArray : int list array) (smooth:bool) (hasNormalWithin : bool) (hasTextureCoords : bool)= 
    let ar = Array.zeroCreate(faceArray.Length)
    let bBoxSize : Point array = Array.zeroCreate(2)
    bBoxSize.[0] <- Point(infinity, infinity, infinity)
    bBoxSize.[1] <- Point(-infinity, -infinity, -infinity)
    for i in 0..ar.Length-1 do
        //CALCULATES THE NEW POINT
        let v1 = triangleArray.[faceArray.[i].[1]]
        let p1 = new TriPoint(v1)
        let v2 = triangleArray.[faceArray.[i].[2]]
        let p2 = new TriPoint(v2)
        let v3 = triangleArray.[faceArray.[i].[3]]
        let p3 = new TriPoint(v3)
        //CONVERTS TO A TRIANGLE
        let triangle = new BasePLYTriangle((p1 :> Point) ,(p2 :> Point) ,(p3 :> Point), smooth, hasNormalWithin, hasTextureCoords)
        //CALCULATES NORMAL IF NEEDED
        if (smooth && not hasNormalWithin) then
            v1.normal <- triangle.n
            v2.normal <- triangle.n
            v3.normal <- triangle.n
        //INCREASE THE NEW BBOX
        let triangleLowPoint = triangle.BBox.lowPoint 
        let triangleHightPoint = triangle.BBox.highPoint
        bBoxSize.[0] <- bBoxSize.[0].Lowest triangleLowPoint
        bBoxSize.[1] <- bBoxSize.[1].Highest triangleHightPoint
        ar.[i] <- (triangle)
    ar,BBox(bBoxSize.[0],bBoxSize.[1])
        

let drawTriangles (filepath:string) (smoothen:bool) = 
    let test = parsePLY filepath
    let triangleArray = fst test
    let faceArray = snd test

    let hasNormalWithin = triangleArray.[0].nx.IsSome
    let hasTexture = triangleArray.[0].u.IsSome

    let triangleInfo = createTriangles triangleArray faceArray smoothen hasNormalWithin hasTexture
    let ar = fst triangleInfo
    let bBox = snd triangleInfo

    let idOfShape = Acceleration.listOfAccel.Length + 1

    let baseShape = {new BaseShape() with
        member this.toShape(tex) = 
            let triangles = ar |> Array.map (fun (i:BasePLYTriangle) -> (i.toShape(tex)))
            let sA = shapeArray(idOfShape, triangles, None)
            let accel = Acceleration.createAcceleration(sA)
            let shape = {new Shape() with
                member this.hitFunction r = traverseIAcceleration accel r triangles
                member this.getBoundingBox () = bBox
                member this.isInside p = false
            }
            shape
    }
    baseShape