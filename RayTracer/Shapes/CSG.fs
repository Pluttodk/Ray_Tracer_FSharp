namespace Tracer.Basics
open System



///////////////////////////////////
////CONSTRUCTIVE SOLID GEOMETRY////
///////////////////////////////////

type CSGOperator = Union | Intersection | Subtraction | Grouping

type CSG(s1:Shape, s2:Shape, op:CSGOperator) =
    inherit Shape()
    member this.s1 = s1
    member this.s2 = s2
    member this.op = op
    member this.epsilon = 0.00005
    member this.bBox = match op with
                       |Union|Grouping -> //merges the two BBoxes, by combining the highest high coords, and the lowest low coords, to form a new bounding box
                            let bBox1 = s1.getBoundingBox ()
                            let bBox2 = s2.getBoundingBox ()
                            let newLow = Point((min bBox1.lowPoint.X bBox2.lowPoint.X), (min bBox1.lowPoint.Y bBox2.lowPoint.Y), (min bBox1.lowPoint.Z bBox2.lowPoint.Z))
                            let newHigh = Point((max bBox1.highPoint.X bBox2.highPoint.X), (max bBox1.highPoint.Y bBox2.highPoint.Y), (max bBox1.highPoint.Z bBox2.highPoint.Z))
                            BBox(newLow, newHigh)
                       |Intersection -> //chooses the highest of the low point coords, and the lowest of the highpoint coords, to approximate the intersection
                            let bBox1 = s1.getBoundingBox ()
                            let bBox2 = s2.getBoundingBox ()
                            let newLow = Point((max bBox1.lowPoint.X bBox2.lowPoint.X), (max bBox1.lowPoint.Y bBox2.lowPoint.Y), (max bBox1.lowPoint.Z bBox2.lowPoint.Z))
                            let newHigh = Point((min bBox1.highPoint.X bBox2.highPoint.X), (min bBox1.highPoint.Y bBox2.highPoint.Y), (min bBox1.highPoint.Z bBox2.highPoint.Z))
                            BBox(newLow, newHigh)
                       |Subtraction -> s1.getBoundingBox () //just returns the bounding box for s1

    override this.isInside (p:Point) = match op with
                                        |Union|Grouping -> (s1.isInside p || s2.isInside p)
                                        |Intersection -> (s1.isInside p && s2.isInside p)
                                        |Subtraction -> (s1.isInside p && (not (s2.isInside p)))

    override this.getBoundingBox () = this.bBox

                                        

    ////UNION////
    member this.unionHitFunctionInside (originalRay:Ray) (r:Ray) =
        //fire ray at both shapes
        let s1Hit = s1.hitFunction r 
        let s2Hit = s2.hitFunction r

        let s1Time = match s1Hit.DidHit with
                     |true -> s1Hit.Time 
                     |false -> infinity
        let s2Time = match s2Hit.DidHit with
                     |true -> s2Hit.Time 
                     |false -> infinity

        //compare the two times, and continue to work with the closest one (shouldnt be possible for both to miss)
        match (s1Time <= (s2Time + this.epsilon)) with 
        |true -> 
            let p1 = r.PointAtTime s1Time
            match (s2.isInside p1) with
            |true ->  
                let newOrigin = p1.Move (r.GetDirection.MultScalar (this.epsilon))
                this.unionHitFunctionInside originalRay (new Ray(newOrigin, r.GetDirection.Normalise))//keep firing the ray, as we have hit an internal edge (so the hit is inside s1)
            |false -> HitPoint(originalRay, originalRay.TimeAtPoint(r.PointAtTime(s1Hit.Time)), s1Hit.Normal, s1Hit.Material, this) //if the hit, is not inside s2, we have found the hitpoint
        |false -> 
            let p2 = r.PointAtTime s2Time
            match (s1.isInside p2) with
            |true ->
                let newnewOrigin = p2.Move (r.GetDirection.MultScalar (this.epsilon))
                this.unionHitFunctionInside originalRay (new Ray(newnewOrigin, r.GetDirection.Normalise))//keep firing the ray, as we have hit an internal edge (so the hit is inside s1)
            |false ->
                HitPoint(originalRay, originalRay.TimeAtPoint(r.PointAtTime(s2Hit.Time)), s2Hit.Normal, s2Hit.Material, this) //if the hit, is not inside s1, we have found the hitpoint

    member this.unionHitFunction (r:Ray) = match this.isInside r.GetOrigin with
                                           |false -> 
                                                let s1Hit = s1.hitFunction r
                                                let s2Hit = s2.hitFunction r
                                                match ((s1Hit.DidHit) || (s2Hit.DidHit)) with // checks if any of the shapes was hit, before proceeding
                                                |true ->
                                                    let s1Time = match s1Hit.DidHit with
                                                                 |true -> s1Hit.Time 
                                                                 |false -> infinity
                                                    let s2Time = match s2Hit.DidHit with
                                                                 |true -> s2Hit.Time 
                                                                 |false -> infinity
                                                    match (s1Time <= (s2Time + this.epsilon)) with
                                                    |true -> HitPoint(r, s1Hit.Time, s1Hit.Normal, s1Hit.Material, this, s1Hit.U, s1Hit.V, s1Hit.DidHit)
                                                    |false -> HitPoint(r, s2Hit.Time, s2Hit.Normal, s2Hit.Material, this, s2Hit.U, s2Hit.V, s2Hit.DidHit)
                                                |false -> HitPoint(r)
                                           |true -> this.unionHitFunctionInside r r
                                               

    ////INTERSECTION////
    member this.intersectionHitFunction (originalRay:Ray) (r:Ray) = 
        let s1Hit = s1.hitFunction r //fire ray at both shapes
        let s2Hit = s2.hitFunction r
        match (s1Hit.DidHit || s2Hit.DidHit) with  // checks if any of the shapes was hit, before proceeding
        |true ->
            let s1Time = match s1Hit.DidHit with
                            |true -> s1Hit.Time
                            |false -> infinity
            let s2Time = match s2Hit.DidHit with
                            |true -> s2Hit.Time
                            |false -> infinity
        
        
            match (s1Time, s2Time) with 
            |(s1T, s2T) when s1T = infinity ->  //if only s2 was a hit
                let p2 = r.PointAtTime s2T
                match (s1.isInside p2) with
                |true -> 
                    HitPoint(originalRay, originalRay.TimeAtPoint p2, s2Hit.Normal, s2Hit.Material, this, s2Hit.U, s2Hit.V, s2Hit.DidHit)
                |false -> 
                    let newOrigin = p2.Move (r.GetDirection.MultScalar (this.epsilon))
                    this.intersectionHitFunction originalRay (new Ray(newOrigin, r.GetDirection.Normalise))

            |(s1T, s2T) when s2T = infinity -> //if only s1 was a hit
                let p1 = r.PointAtTime s1T
                match (s2.isInside p1) with
                |true -> 
                    HitPoint(originalRay, originalRay.TimeAtPoint p1, s1Hit.Normal, s1Hit.Material, this, s1Hit.U, s1Hit.V, s1Hit.DidHit)
                |false -> 
                    let newOrigin = p1.Move (r.GetDirection.MultScalar (this.epsilon))
                    this.intersectionHitFunction originalRay (new Ray(newOrigin, r.GetDirection))
                    
            |(s1T, s2T) when (s2T - this.epsilon) < s1T && s1T < (s2T + this.epsilon) -> //if both shapes are hit, and they overlap
                HitPoint(originalRay, originalRay.TimeAtPoint (r.PointAtTime s1T), s1Hit.Normal, s1Hit.Material, this, s1Hit.U, s1Hit.V, s1Hit.DidHit)

            |(s1T, s2T) -> //both shapes are hit, and they dont overlap
                        //refires rays from the furthest valid hit
                        let s1T = s1T
                        let p1 = r.PointAtTime s1T //find the points, so they don't have to be calculated twice
                        let p2 = r.PointAtTime s2T
                        match (s1T <= s2T) with //checks which t-value is smallest, and should therefore be checked first
                        |true -> 
                            match (s2.isInside p1) with //checks if the s1 hit, is inside s2
                            |true ->  //if it is, return s1 hit
                                HitPoint(originalRay, originalRay.TimeAtPoint p1, s1Hit.Normal, s1Hit.Material, this, s1Hit.U, s1Hit.V, s1Hit.DidHit)
                            |false -> //if it is not, check if s2Hit lies inside s1
                                match (s1.isInside p2) with //if s2Hit lies inside s1, return s2Hit, else refire the ray, to check for more of the shape 
                                |true -> HitPoint(originalRay, originalRay.TimeAtPoint p2, s2Hit.Normal, s2Hit.Material, this, s2Hit.U, s2Hit.V, s2Hit.DidHit)
                                |false ->
                                    let newOrigin = p2.Move (r.GetDirection.MultScalar (this.epsilon))
                                    this.intersectionHitFunction originalRay (new Ray(newOrigin, r.GetDirection.Normalise))
                        |false -> 
                            match (s1.isInside p2) with //checks if the s2 hit, is inside s1
                            |true -> //if it is, return s2 hit
                                HitPoint(originalRay, originalRay.TimeAtPoint p2, s2Hit.Normal, s2Hit.Material, this, s2Hit.U, s2Hit.V, s2Hit.DidHit)
                            |false -> //if it is not, check if s1Hit lies inside s2
                                match (s2.isInside p1) with //if s1Hit lies inside s2, return s1Hit, else refire the ray, to check for more of the shape 
                                |true -> HitPoint(originalRay, originalRay.TimeAtPoint p1, s1Hit.Normal, s1Hit.Material, this, s1Hit.U, s1Hit.V, s1Hit.DidHit)
                                |false -> 
                                    let newOrigin = p1.Move (r.GetDirection.MultScalar (this.epsilon))
                                    this.intersectionHitFunction originalRay (new Ray(newOrigin, r.GetDirection.Normalise))
        |false -> HitPoint(r)


    ////SUBTRACTION////
    member this.subtractionHitFunction (originalRay:Ray) (r:Ray) =
        let s1Hit = s1.hitFunction r //fire ray at first shape
   
        match s1Hit.DidHit with 
        |true -> 
            let p1 = r.PointAtTime s1Hit.Time
            match (s2.isInside (p1.Move (originalRay.GetDirection.MultScalar (this.epsilon)).Invert)) with //checks if p1 lies inside s2 (plus epsilon)
            |true -> //refire Ray
                let newOrigin = p1.Move (r.GetDirection.MultScalar (this.epsilon))
                let r2 = new Ray(newOrigin, r.GetDirection.Normalise) //make new ray, so we dont repeat hits
                let s2Hit = s2.hitFunction r2 //fire new ray at second shape

                let p2 = r2.PointAtTime s2Hit.Time
                //it should always hit s2, when fired from inside s2.
                match (s1.isInside p2) with //checks if p2 lies inside s1
                |true ->  //inside hit on s2                     
                    HitPoint(originalRay, originalRay.TimeAtPoint p2, s2Hit.Normal.Invert, s2Hit.Material, this, s2Hit.U, s2Hit.V, s2Hit.DidHit)
                |false -> //s2 creates a hole through s1, so the ray needs to refired, to check for mor hits
                    let newnewOrigin = p2.Move (r2.GetDirection.MultScalar (this.epsilon))
                    this.subtractionHitFunction originalRay (new Ray(newnewOrigin, r2.GetDirection.Normalise)) //the direction vector should be the same for r and r2
            |false -> HitPoint(originalRay, originalRay.TimeAtPoint p1, s1Hit.Normal, s1Hit.Material, this, s1Hit.U, s1Hit.V, s1Hit.DidHit)
        |false -> HitPoint(r)
    
    
    ////GROUPING////
    member this.groupingHitFunction (r:Ray) =
        let s1Hit = s1.hitFunction r //fire ray at both shapes
        let s2Hit = s2.hitFunction r

        match (s1Hit.DidHit || s2Hit.DidHit) with
        |true ->
            let s1Time = match s1Hit.DidHit with
                         |true -> s1Hit.Time 
                         |false -> infinity
            let s2Time = match s2Hit.DidHit with 
                         |true -> s2Hit.Time 
                         |false -> infinity

            match (s1Time <= (s2Time + this.epsilon)) with //just returns the closest hit
            |true -> HitPoint(r, s1Time, s1Hit.Normal, s1Hit.Material, this, s1Hit.U, s1Hit.V, s1Hit.DidHit)
            |false -> HitPoint(r, s2Time, s2Hit.Normal, s2Hit.Material, this, s2Hit.U, s2Hit.V, s2Hit.DidHit)
        |false -> HitPoint(r)

    
    ////GENERAL HIT-FUNCTION////
    override this.hitFunction (r:Ray) = match op with
                                        |Union -> this.unionHitFunction r
                                        |Intersection -> this.intersectionHitFunction r r //the extra ray is because they need the original ray, to calculate correct t-value
                                        |Subtraction -> this.subtractionHitFunction r r //the extra ray is because they need the original ray, to calculate correct t-value
                                        |Grouping -> this.groupingHitFunction r


