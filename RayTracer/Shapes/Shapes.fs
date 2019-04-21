namespace Tracer.Basics
open System
open Transformation
open Textures

///////////////////////////////////
/////////////SHAPES!!!/////////////
///////////////////////////////////

////RECTANGLE////
type Rectangle(bottomLeft:Point, topLeft:Point, bottomRight:Point, tex:Texture)=
    inherit Shape()
    member this.bottomLeft = bottomLeft
    member this.topLeft = topLeft
    member this.bottomRight = bottomRight
    member this.tex = tex
    member this.width = bottomRight.X - bottomLeft.X
    member this.height = topLeft.Y - bottomLeft.Y
    member this.normal:Vector = new Vector(0.0, 0.0, 1.0)
    member this.bBox = 
        let e = 0.000001
        let lx = (min bottomLeft.X (min topLeft.X bottomRight.X)) - e
        let ly = (min bottomLeft.Y (min topLeft.Y bottomRight.Y)) - e
        let lz = - e

        let hx = (max bottomLeft.X (max topLeft.X bottomRight.X)) + e
        let hy = (max bottomLeft.Y (max topLeft.Y bottomRight.Y)) + e
        let hz = e

        BBox(Point(lx, ly, lz), Point(hx, hy, hz))

    override this.isInside (p:Point) = failwith "Cannot be inside 2D shapes"

    override this.getBoundingBox () = this.bBox

    override this.hitFunction (r:Ray) = 
        match (r.GetDirection.Z = 0.0) with //This method checks if dz = 0.0, which would make the ray, parrallel to the plane
        |true -> HitPoint(r)
        |false -> 
            let t = (-((r.GetOrigin.Z) / (r.GetDirection.Z))) //gets the t-value
            match t <= 0.0 with //This checks if t is 0 or smaller, in which case there is no hit
            |true -> HitPoint(r)
            |false ->
                let px = (r.GetOrigin.X)+t*(r.GetDirection.X)
                let py = (r.GetOrigin.Y)+t*(r.GetDirection.Y)
                if (px >= 0.0 && px <= this.width) && (py >= 0.0 && py <= this.height) then 
                    let func = Textures.getFunc tex
                    let u = (px / this.width)
                    let v = (py / this.height)
                    let mat = func u v
                    HitPoint(r, t, this.normal, mat, this, u, v) 
                else HitPoint(r)
        
                      
////DISC////
type Disc(center:Point, radius:float, tex:Texture)=
    inherit Shape()
    member this.center = center
    member this.radius = Math.Abs radius // must not be a negative number
    member this.tex = tex
    member this.bBox = 
        let e = 0.000001
        let lx = -radius - e
        let ly = -radius - e
        let lz = -e

        let hx = radius + e
        let hy = radius + e
        let hz = e

        BBox(Point(lx, ly, lz), Point(hx, hy, hz))

    override this.isInside (p:Point) = failwith "Cannot be inside 2D shapes"

    override this.getBoundingBox () = this.bBox

    member this.normal: Vector = new Vector(0.0, 0.0, 1.0)

    override this.hitFunction (r:Ray) = 
        match r with
            |(r) when (r.GetDirection.Z) = 0.0 -> HitPoint(r) //This checks if dz = 0.0, which would make the ray, parrallel to the plane 
            |(r) when (-((r.GetOrigin.Z) / (r.GetDirection.Z))) <= 0.0 -> HitPoint(r) //This checks if t is 0 or smaller, in which case there is no hit
            |(r) -> let t = (-((r.GetOrigin.Z) / (r.GetDirection.Z)))
                    let px = (r.GetOrigin.X)+t*(r.GetDirection.X)
                    let py = (r.GetOrigin.Y)+t*(r.GetDirection.Y)
                    if (((px*px)+(py*py)) <= radius*radius) 
                        then 
                            let func = Textures.getFunc tex
                            let u = (px + radius)/(2.*radius)
                            let v = (py + radius)/(2.*radius)
                            let mat = func u v
                            HitPoint(r, t, this.normal, mat, this, u, v) 
                    else HitPoint(r)


////TRIANGLE////
and Triangle(a:Point, b:Point, c:Point, mat:Material)=
    inherit Shape()
    let mutable be : float = 0.0
    let mutable ga : float = 0.0
    member this.a = a
    member this.b = b
    member this.c = c
    member this.mat = mat
    member this.u = a-b
    member this.v = a-c

    member this.n = this.u.CrossProduct this.v

    //the many members are for simplifying cramers rule and hit function
    member this.pa = ((a.X)-(b.X))
    member this.pb = ((a.X)-(c.X))
    member this.e = ((a.Y)-(b.Y))
    member this.f = ((a.Y)-(c.Y))
    member this.i = ((a.Z)-(b.Z))
    member this.j = ((a.Z)-(c.Z))

    member this.bBox =
        let e = 0.000001
        let lx = (min a.X (min b.X c.X)) - e
        let ly = (min a.Y (min b.Y c.Y)) - e
        let lz = (min a.Z (min b.Z c.Z)) - e 

        let hx = (max a.X (max b.X c.X)) + e
        let hy = (max a.Y (max b.Y c.Y)) + e
        let hz = (max a.Z (max b.Z c.Z)) + e 

        BBox(Point(lx, ly, lz), Point(hx, hy, hz))
        
    member this.beta with get() = be and set(value) = be <- value
    member this.gamma with get() = ga and set(value) = ga <- value

    override this.isInside (p:Point) = failwith "Cannot be inside 2D shapes" 

    override this.getBoundingBox () = this.bBox
    

    override this.hitFunction (r:Ray) = 
        let pc = (r.GetDirection.X)
        let g = (r.GetDirection.Y)
        let k = (r.GetDirection.Z)


        match r with
            |(r) when (this.pa*((this.f*(k))-((g)*this.j)) + this.pb*(((g)*this.i)-(this.e*(k))) + (pc)*((this.e*this.j)-(this.f*this.i))) = 0.0 -> HitPoint(r)
            |(r) -> let d = ((a.X)-(r.GetOrigin.X)) 
                    let h = ((a.Y)-(r.GetOrigin.Y)) 
                    let l = ((a.Z)-(r.GetOrigin.Z))
                    let D = (this.pa*((this.f*k)-(g*this.j)) + this.pb*((g*this.i)-(this.e*k)) + pc*((this.e*this.j)-(this.f*this.i)))
                    if (not (D = 0.0)) then
                        let x = (d*((this.f*k)-(g*this.j)) + this.pb*((g*l)-(h*k)) + pc*((h*this.j)-(this.f*l)))/D
                        let y = (this.pa*((h*k)-(g*l)) + d*((g*this.i)-(this.e*k)) + pc*((this.e*l)-(h*this.i)))/D
                        let z = (this.pa*((this.f*l)-(h*this.j)) + this.pb*((h*this.i)-(this.e*l)) + d*((this.e*this.j)-(this.f*this.i)))/D
                        //x=beta, y=gamma, z=t
                        //alpha is gained from 1-x-y, this is used for texturing (alpha, beta, gamma that is)
                        if (x <= 1.0 && x >= 0.0) && (y <= 1.0 && y >= 0.0) && (x+y <= 1.0 && x+y >= 0.0) && (z>0.0) then
                                this.beta <- x
                                this.gamma <- y
                                HitPoint(r, z, this.n, mat, this) else HitPoint(r)
                    else HitPoint(r)
                            


////SPHERE////
type SphereShape(origin: Point, radius: float, tex: Texture) = 
    inherit Shape()

    let pimult2 = 2. * Math.PI

    member this.origin = origin 
    member this.radius = radius
    member this.tex = tex
    member this.bBox = //no point on the sphere should be larger than the radius, as origin is always 0,0,0
        let e = 0.000001
        let lx = - radius - e
        let ly = - radius - e
        let lz = - radius - e

        let hx = radius + e
        let hy = radius + e
        let hz = radius + e

        BBox(Point(lx, ly, lz), Point(hx, hy, hz))

    override this.isInside (p:Point) =
        let x = (p.X)**2. + (p.Y)**2. + (p.Z)**2.
        (x < (radius**2.))

    override this.getBoundingBox () = this.bBox    

    member this.NormalAtPoint (p:Point) = 
        Vector((p.X/radius),(p.Y/radius),(p.Z/radius)).Normalise
    
    member this.getTextureCoords (p:Point) =
        let n = this.NormalAtPoint p
        let u = Math.Atan2(n.X, n.Z) / pimult2
        let v = 1. - (Math.Acos(n.Y) / Math.PI)

        let uF = if u < 0. then 1. + u else u
        let vF = if v < 0. then 1. + v else v
        (uF, vF)


    member this.determineHitPoint (r:Ray) (t:float) = //determines the hipoint and the texture
        let p = r.PointAtTime t
        let (u,v) = this.getTextureCoords (p)
        let func = Textures.getFunc tex
        let mat = func u v 
        HitPoint(r, t, this.NormalAtPoint p, mat, this, u, v)

    override this.hitFunction (r:Ray) = 
        match (this.bBox.intersect r).IsSome with
        |true ->
            let a = (r.GetDirection.X**2.) + (r.GetDirection.Y**2.) + (r.GetDirection.Z**2.) //Determines a in the quadratic equation
            let b = 2. * ((r.GetOrigin.X * r.GetDirection.X) + (r.GetOrigin.Y * r.GetDirection.Y) + (r.GetOrigin.Z * r.GetDirection.Z))//Determines b in the quadratic equation
            let c = (r.GetOrigin.X**2.) + (r.GetOrigin.Y**2.) + (r.GetOrigin.Z**2.) - (radius**2.) //Determines c in the quadratic equation
            let D = (b**2.)-4.*a*c 
            let sqrtD = Math.Sqrt(D)
            let t1 = (-b + sqrtD)/(2.0 * a)
            let t2 = (-b - sqrtD)/(2.0 * a)
            match D with
            |(0.0) -> match t1 > 0.0 with //when D = 0.0, there should only be one hit, and t1 should be equal to t2
                      |true -> this.determineHitPoint r t1 
                      |false -> HitPoint(r)
            |(D) when D < 0.0 -> HitPoint(r) //when D < 0.0 there are no hits
            |(D) -> match (t1,t2) with //when D > 0.0, and there are two valid values for t
                      |(t1,t2) when t1 <= 0.0 && t2 <= 0.0 -> HitPoint(r)
                      |(t1,t2) -> match (t1 < t2 && t1 > 0.0) with
                                  |true -> this.determineHitPoint r t1 
                                  |false when t2 > 0.0 -> this.determineHitPoint r t2    
                                  |false -> this.determineHitPoint r t1
        |false -> HitPoint(r)


////HOLLOWCYLINDER////
type HollowCylinder(center:Point, radius:float, height:float, tex:Texture) = //change back to texture
    inherit Shape()
    member this.center = center
    member this.radius = radius
    member this.height = height
    member this.tex = tex
    member this.bBox = 
        let e = 0.000001
        let lxz = - radius - e
        let ly = - (height/2.) - e //height instead of radius for the Y coord

        let hxz = radius + e
        let hy = (height/2.) + e //height instead of radius for the Y coord

        BBox(Point(lxz, ly, lxz), Point(hxz, hy, hxz))

    override this.isInside (p:Point) = failwith "Cannot be inside 2D shapes"

    override this.getBoundingBox () = this.bBox

    member this.NormalAtPoint (p:Point):Vector =
        new Vector(p.X/radius, 0.0, p.Z/radius)
    
    member this.getTextureCoords (p:Point) = //determines the the texture coords u and v
        let n = this.NormalAtPoint p 
        let phiNot = Math.Atan2(n.X, n.Z)
        let phi = match phiNot < 0. with
                  |true -> phiNot + (2. * Math.PI) 
                  |false -> phiNot
        let u = phi / (2. * Math.PI)
        let v = (p.Y / height) + (1. / 2.)
        (u, v)

    member this.determineHitPoint (r:Ray) (p:Point) =  //creates the HitPoint, based on the given Point
        let (u,v) = this.getTextureCoords (p)
        let func = Textures.getFunc tex
        let mat = func u v 
        HitPoint(r, r.TimeAtPoint p, this.NormalAtPoint p, mat, this, u, v)

    member this.determineIfPointIsInsideHeight (p:Point) = //determines if a Point is within the height of the cylinder
        (p.Y > -(height/2.0) && p.Y < (height/2.0))

    override this.hitFunction (r:Ray) = 
        match (this.bBox.intersect r).IsSome with
        |true ->
            let a = ((r.GetDirection.X)**2.0) + ((r.GetDirection.Z)**2.0) //Determines a in the quadratic equation
            let b = 2.0*((r.GetOrigin.X * r.GetDirection.X)+(r.GetOrigin.Z * r.GetDirection.Z)) //Determines b in the quadratic equation
            let c = ((r.GetOrigin.X)**2.0) + ((r.GetOrigin.Z)**2.0) - (radius**2.0) //Determines c in the quadratic equation
            let D = (b**2.0) - 4.0*a*c
            let t1 = (-b + Math.Sqrt(D))/(2.0 * a)
            let t2 = (-b - Math.Sqrt(D))/(2.0 * a)
            match D with
            |(0.0) -> match t1 <= 0.0 with //when D = 0 then t1 = t2
                      |true -> HitPoint(r) 
                      |false -> let p = r.PointAtTime t1
                                match this.determineIfPointIsInsideHeight p with
                                |true -> this.determineHitPoint r p 
                                |false -> HitPoint(r)
            |(D) when D < 0.0 -> HitPoint(r) //when D < 0.0 there are no hits
            |(_) -> match (t1,t2) with //when D > 0.0, and there are two valid values for t
                    |(t1,t2) when t1 <= 0.0 && t2 <= 0.0 -> HitPoint(r) //if both t's are a miss
                    |(t1,t2) -> 
                        let p1 = r.PointAtTime t1
                        let p2 = r.PointAtTime t2
                        match t2 < t1 && t2 > 0.0 with
                        |true ->  match this.determineIfPointIsInsideHeight p2 with
                                  |true -> this.determineHitPoint r p2
                                  |false -> match (t1 > 0.0 && (this.determineIfPointIsInsideHeight p1)) with
                                            |true -> this.determineHitPoint r p1
                                            |false -> HitPoint(r)
                        |false -> match t1 > 0.0 with
                                  |true ->  match this.determineIfPointIsInsideHeight p1 with
                                            |true -> this.determineHitPoint r p1
                                            |false -> HitPoint(r)
                                  |false -> match (t2 > 0.0 && (this.determineIfPointIsInsideHeight p2)) with
                                            |true -> this.determineHitPoint r p2
                                            |false -> HitPoint(r)                                                           
        |false -> HitPoint(r)


////TRANSFORM////                                                                                     
module Transform =
    let transformRay (r : Ray) t = 
        let o = pointToMatrix r.GetOrigin
        let d = vectorToMatrix r.GetDirection
        let invT = getInvMatrix t
        let originMatrix = QuickMatrix.multi (invT, o)
        let directionMatrix = QuickMatrix.multi (invT, d)
        let origin = matrixToPoint originMatrix
        let direction = matrixToVector directionMatrix
        new Ray(origin, direction)

    let transformNormal (v:Vector) (t: Transformation.Transformation)= 
        let tVector = matrixToVector (QuickMatrix.multi ((getInvMatrix (t)).transpose,(vectorToMatrix v)))
        tVector

    let transform (s : Shape) (t:Transformation) =
        {new Shape() with
            member this.hitFunction r = 
                let transformedRay = transformRay r t
                let hitsOriginal = s.hitFunction transformedRay
                if (hitsOriginal.DidHit) then
                    let normal = transformNormal (hitsOriginal.Normal) t
                    new HitPoint(r, hitsOriginal.Time, normal.Normalise, hitsOriginal.Material, hitsOriginal.Shape, hitsOriginal.U, hitsOriginal.V)
                else 
                    new HitPoint(r)
            member this.getBoundingBox () = 
                let bbH = s.getBoundingBox().highPoint
                let bbL = s.getBoundingBox().lowPoint
                let vertex = 
                    [|bbH; 
                    Point(bbL.X, bbH.Y, bbH.Z);
                    Point(bbL.X, bbH.Y, bbL.Z);
                    Point(bbH.X, bbH.Y, bbL.Z);
                    Point(bbH.X, bbL.Y, bbH.Z);
                    Point(bbL.X, bbL.Y, bbH.Z);
                    bbL;
                    Point(bbH.X, bbL.Y, bbL.Z)|]
                let newPos = Array.zeroCreate(2)
                let firstPoint = matrixToPoint (QuickMatrix.multi ((getMatrix t),pointToMatrix vertex.[0]))
                newPos.[0] <- firstPoint
                newPos.[1] <- firstPoint
                for i in 1..7 do 
                    let newPoint = matrixToPoint (QuickMatrix.multi ((getMatrix t),pointToMatrix vertex.[i]))
                    newPos.[0] <- (newPos.[0]).Lowest newPoint
                    let pos1 = newPos.[1]
                    newPos.[1] <- pos1.Highest newPoint
                BBox(newPos.[0],newPos.[1])
            member this.isInside p = 
                let oldP = matrixToPoint (QuickMatrix.multi(getInvMatrix t, pointToMatrix p))
                s.isInside(oldP)
        }
        

////SOLIDCYLINDER////
type SolidCylinder(center:Point, radius:float, height:float, cylinder:Texture, top:Texture, bottom:Texture) =
    inherit Shape()
    member this.center = center
    member this.radius = radius
    member this.height = height
    member this.cylinder = cylinder
    member this.top = top
    member this.bottom = bottom
    //builds the transformed discs at the top and bottom of the solid cylinder
    member this.topDisc = 
        let rotate = rotateX (Math.PI/2.)
        let move = translate 0. (height/2.) 0.
        let mergeTrans = mergeTransformations [rotate; move]
        Transform.transform (Disc(Point(0.,0.,0.), radius, top)) mergeTrans
    member this.bottomDisc = 
        let rotate = rotateX (Math.PI/2.)
        let move = translate 0. -(height/2.) 0.
        let mergeTrans = mergeTransformations [rotate; move]
        Transform.transform (Disc(Point(0.,0.,0.), radius, bottom)) mergeTrans
    //builds the hollow cylinder
    member this.hollowCylinder = HollowCylinder(center, radius, height, cylinder)
    member this.bBox = this.hollowCylinder.bBox

    override this.isInside (p:Point) = 
        match ((p.X**2. + p.Z**2.) <= radius**2.) with //checks if the point lies within the bounds of the cylinders radius (similar to checking for discs)
        |true -> (-(height/2.) <= p.Y && p.Y <= (height/2.)) //checks if the point lies between the 2 discs, so not above or below the cylinder
        |false -> false

    override this.getBoundingBox () = this.bBox

    override this.hitFunction (r:Ray) = 
        // Check the component shapes, for HitPoints
        let hpTop = this.topDisc.hitFunction r
        let hpBottom = this.bottomDisc.hitFunction r
        let hpCylinder = this.hollowCylinder.hitFunction r

        match ((hpBottom.DidHit || hpTop.DidHit) || hpCylinder.DidHit) with //checks if all shapes were missed
        |true ->
            //extract t from hitPoints
            let tTop = match hpTop.DidHit with
                            |true -> hpTop.Time
                            |false -> infinity
            let tBottom = match hpBottom.DidHit with
                            |true -> hpBottom.Time
                            |false -> infinity
            let tCylinder = match hpCylinder.DidHit with
                            |true -> hpCylinder.Time
                            |false -> infinity

            //Compare t values
            match (tTop, tBottom, tCylinder) with
            |(top, bottom, cylinder) when top = bottom && bottom = cylinder -> HitPoint(r)
            |(top, bottom, cylinder) when cylinder < bottom && cylinder < top ->  hpCylinder
            |(top, bottom, cylinder) when top < bottom && top < cylinder ->  hpTop
            |(top, bottom, cylinder) when bottom < top && bottom < cylinder ->  hpBottom
            |(_,_,_) -> HitPoint(r)
        |false -> HitPoint(r)


////BOX////
type Box(low:Point, high:Point, front:Texture, back:Texture, top:Texture, bottom:Texture, left:Texture, right:Texture) = 
    inherit Shape()
    member this.low = low
    member this.high = high
    member this.front = front
    member this.back = back
    member this.top = top
    member this.bottom = bottom
    member this.left = left
    member this.right = right
    member this.width = high.X - low.X
    member this.height = high.Y - low.Y
    member this.depth = high.Z - low.Z

    override this.isInside (p:Point) =
        match (low.X <= p.X && p.X <= high.X) with
        |true ->
            match (low.Y <= p.Y && p.Y <= high.Y) with
            |true -> (low.Z <= p.Z && p.Z <= high.Z) 
            |false -> false
        |false -> false

    override this.getBoundingBox () = 
        let e = 0.000001
        BBox(Point(low.X-e, low.Y-e, low.Z-e), Point(high.X+e, high.Y+e, high.Z+e))
    
    member this.getMatFromTex (tex:Texture) (u:float) (v:float) = //gets the material, based on the texture and the texture coords
        let func = Textures.getFunc tex
        let mat = func u v 
        mat

    override this.hitFunction (r:Ray) = 

        let boolX = r.GetDirection.X >= 0.0
        let boolY = r.GetDirection.Y >= 0.0
        let boolZ = r.GetDirection.Z >= 0.0
        
        //deermines the values, at which the ray either enters or exits certain dimensions of the box
        let tx =  match boolX with
                  |true -> (low.X - r.GetOrigin.X)/r.GetDirection.X 
                  |false -> (high.X - r.GetOrigin.X)/r.GetDirection.X
        let tx' = match boolX with
                  |true -> (high.X - r.GetOrigin.X)/r.GetDirection.X 
                  |false -> (low.X - r.GetOrigin.X)/r.GetDirection.X
        let ty =  match boolY with
                  |true -> (low.Y - r.GetOrigin.Y)/r.GetDirection.Y 
                  |false -> (high.Y - r.GetOrigin.Y)/r.GetDirection.Y
        let ty' = match boolY with
                  |true -> (high.Y - r.GetOrigin.Y)/r.GetDirection.Y 
                  |false -> (low.Y - r.GetOrigin.Y)/r.GetDirection.Y
        let tz =  match boolZ with
                  |true -> (low.Z - r.GetOrigin.Z)/r.GetDirection.Z 
                  |false -> (high.Z - r.GetOrigin.Z)/r.GetDirection.Z
        let tz' = match boolZ with
                  |true -> (high.Z - r.GetOrigin.Z)/r.GetDirection.Z 
                  |false -> (low.Z - r.GetOrigin.Z)/r.GetDirection.Z
        
        //t is the distace where the ray enters the box, t' is the distance where the box exits the box
        let t = max tx (max ty tz)

        let t' = min tx' (min ty' tz')

        match (t < t' && t' > 0.0) with
        |true ->
            match t > 0.0 with
            |true -> 
                match (tx, ty, tz) with
                |(tx,ty,tz) when tx >= ty && tx >= tz -> match r.GetDirection.X > 0.0 with
                                                         |true -> 
                                                            let u = (r.PointAtTime(t).Y - low.Y) / this.height
                                                            let v = (r.PointAtTime(t).Z - low.Z) / this.depth
                                                            HitPoint(r, t, Vector(-1.0, 0.0, 0.0), (this.getMatFromTex left u v), this, u, v) //when tx is the biggest and t > 0.0
                                                         |false -> 
                                                            let u = (r.PointAtTime(t).Y - low.Y) / this.height
                                                            let v = (r.PointAtTime(t).Z - low.Z) / this.depth
                                                            HitPoint(r, t, Vector(1.0,0.0,0.0), (this.getMatFromTex right u v), this, u, v)
                |(tx,ty,tz) when ty >= tx && ty >= tz -> match r.GetDirection.Y > 0.0 with
                                                         |true ->
                                                            let u = (r.PointAtTime(t).X - low.X) / this.width
                                                            let v = (r.PointAtTime(t).Z - low.Z) / this.depth
                                                            HitPoint(r, t, Vector(0.0, -1.0, 0.0), (this.getMatFromTex bottom u v), this, u, v) //when ty is the biggest and t > 0.0
                                                         |false -> 
                                                            let u = (r.PointAtTime(t).X - low.X) / this.width
                                                            let v = (r.PointAtTime(t).Z - low.Z) / this.depth
                                                            HitPoint(r, t, Vector(0.0, 1.0, 0.0), (this.getMatFromTex top u v), this, u, v)
                |(tx,ty,tz) when tz >= tx && tz >= ty -> match r.GetDirection.Z > 0.0 with
                                                         |true ->
                                                            let u = (r.PointAtTime(t).X - low.X) / this.width
                                                            let v = (r.PointAtTime(t).Y - low.Y) / this.height
                                                            HitPoint(r, t, Vector(0.0, 0.0, -1.0), (this.getMatFromTex back u v), this, u, v) //when tz is the biggest and t > 0.0
                                                         |false ->
                                                            let u = (r.PointAtTime(t).X - low.X) / this.width
                                                            let v = (r.PointAtTime(t).Y - low.Y) / this.height
                                                            HitPoint(r, t, Vector(0.0, 0.0, 1.0), (this.getMatFromTex front u v), this, u, v)
                |(_,_,_) -> failwith "shouldn't reach this point"
            |false ->
                match (tx', ty', tz') with
                |(tx',ty',tz') when tx' <= ty' && tx' <= tz' -> match r.GetDirection.X > 0.0 with
                                                                |true ->
                                                                    let u = (r.PointAtTime(t').Y - low.Y) / this.height
                                                                    let v = (r.PointAtTime(t').Z - low.Z) / this.depth
                                                                    HitPoint(r, t', Vector(-1.0, 0.0, 0.0), (this.getMatFromTex right u v), this, u, v) //when tx' is the smallest and t > 0.0
                                                                |false ->
                                                                    let u = (r.PointAtTime(t').Y - low.Y) / this.height
                                                                    let v = (r.PointAtTime(t').Z - low.Z) / this.depth
                                                                    HitPoint(r, t', Vector(1.0, 0.0, 0.0), (this.getMatFromTex left u v), this, u, v)
                |(tx',ty',tz') when ty' <= tx' && ty' <= tz' -> match r.GetDirection.Y > 0.0 with
                                                                |true ->
                                                                    let u = (r.PointAtTime(t').X - low.X) / this.width
                                                                    let v = (r.PointAtTime(t').Z - low.Z) / this.depth
                                                                    HitPoint(r, t', Vector(0.0, -1.0, 0.0), (this.getMatFromTex top u v), this, u, v) //when ty' is the smallest and t > 0.0
                                                                |false ->
                                                                    let u = (r.PointAtTime(t').X - low.X) / this.width
                                                                    let v = (r.PointAtTime(t').Z - low.Z) / this.depth
                                                                    HitPoint(r, t', Vector(0.0, 1.0, 0.0), (this.getMatFromTex bottom u v), this, u, v)
                |(tx',ty',tz') when tz' <= tx' && tz' <= ty' -> match r.GetDirection.Z > 0.0 with
                                                                |true ->
                                                                    let u = (r.PointAtTime(t').X - low.X) / this.width
                                                                    let v = (r.PointAtTime(t').Y - low.Y) / this.height
                                                                    HitPoint(r, t', Vector(0.0, 0.0, -1.0), (this.getMatFromTex front u v), this, u, v) //when tz' is the smallest and t > 0.0
                                                                |false ->
                                                                    let u = (r.PointAtTime(t').X - low.X) / this.width
                                                                    let v = (r.PointAtTime(t').Y - low.Y) / this.height 
                                                                    HitPoint(r, t', Vector(0.0, 0.0, 1.0), (this.getMatFromTex back u v), this, u, v)
                |(_,_,_) -> failwith "shouldn't reach this point"
        |false -> HitPoint(r)
        

////INFINITEPLANE////
type InfinitePlane(tex:Texture) = 
    inherit Shape()
    member this.tex = tex
    override this.isInside (p:Point) = failwith "Cannot be inside 2D shapes" 
    override this.getBoundingBox () = failwith "Infinite Plane cannot have a Bounding Box"
    override this.hitFunction (r:Ray) = 
        let t = -(r.GetOrigin.Z / r.GetDirection.Z)
        match (r.GetDirection.Z <> 0.0 && t > 0.0) with
        |true ->
            let func = Textures.getFunc tex
            let u = (r.PointAtTime t).X
            let v = (r.PointAtTime t).Y
            let mat = func u v
            HitPoint(r, t, Vector(0.0, 0.0, -1.0), mat, this, u, v)
        |false -> HitPoint(r)