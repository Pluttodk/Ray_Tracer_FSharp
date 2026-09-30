namespace Tracer.Basics

module KD_tree = 

    type ShapeBBox (box:BBox, shape:int) =
        member this.box = box
        member this.shape = shape

    type KDTree = Node of int * float * BBox * KDTree * KDTree
                | Leaf of BBox * ShapeBBox list
                
    let findMaxMin (xs:list<ShapeBBox>) = 
        //Function to find highest point and lowest point from a list of bounding boxes
        match xs with
        | [] -> Point(0., 0., 0.), Point(0., 0., 0.)
        | _  -> 
            let rec find (maxX:float) (minX:float) (maxY:float) (minY:float) (maxZ:float) (minZ:float) (xs:list<ShapeBBox>) =
                match xs with
                | []    -> Point(maxX, maxY, maxZ), Point(minX, minY, minZ)
                | x::xs -> 
                    let maxX = if x.box.highPoint.X > maxX then x.box.highPoint.X else maxX
                    let maxY = if x.box.highPoint.Y > maxY then x.box.highPoint.Y else maxY
                    let maxZ = if x.box.highPoint.Z > maxZ then x.box.highPoint.Z else maxZ
                    let minX = if x.box.lowPoint.X  < minX then x.box.lowPoint.X  else minX
                    let minY = if x.box.lowPoint.Y  < minY then x.box.lowPoint.Y  else minY
                    let minZ = if x.box.lowPoint.Z  < minZ then x.box.lowPoint.Z  else minZ
                    find maxX minX maxY minY maxZ minZ xs
            find -infinity infinity -infinity infinity -infinity infinity xs

    let partitionAfterSelect (boxes:list<ShapeBBox>) (splitX:float) (splitY:float) (splitZ:float) =
        //Function to split boxes into 2 list on each axis.
        let rec inner leftX rightX leftY rightY leftZ rightZ (tosort:list<ShapeBBox>) = 
            match tosort with
            | [] -> leftX,rightX,leftY,rightY,leftZ,rightZ
            | c::cr -> 
                let lX = if c.box.lowPoint.X <= splitX then c::leftX
                         else leftX
                let rX = if c.box.highPoint.X > splitX then c::rightX
                         else rightX
                let lY = if c.box.lowPoint.Y <= splitY then c::leftY
                         else leftY
                let rY = if c.box.highPoint.Y > splitY then c::rightY
                         else rightY
                let lZ = if c.box.lowPoint.Z <= splitZ then c::leftZ
                         else leftZ
                let rZ = if c.box.highPoint.Z > splitZ then c::rightZ
                         else rightZ
                inner lX rX lY rY lZ rZ cr
        inner [] [] [] [] [] [] boxes

    let findSplitValues (boxes:list<ShapeBBox>) = 
        //Function to find split values for each axis
        let boxesLength = float boxes.Length
        let rec inner accX accY accZ minx maxx miny maxy minz maxz (boxes:list<ShapeBBox>) = 
            match boxes with
            | [] -> (accX/boxesLength), (accY/boxesLength), (accZ/boxesLength),
                     minx, maxx, miny, maxy, minz, maxz
            | x::xs->
                    inner 
                       (accX+x.box.lowPoint.X)
                       (accY+x.box.lowPoint.Y)
                       (accZ+x.box.lowPoint.Z)
                       (min minx x.box.lowPoint.X)
                       (max maxx x.box.highPoint.X)
                       (min miny x.box.lowPoint.Y)
                       (max maxy x.box.highPoint.Y)
                       (min minz x.box.lowPoint.Z)
                       (max maxz x.box.highPoint.Z)
                       xs
        inner 0. 0. 0. infinity -infinity infinity -infinity infinity -infinity boxes 
        
    let rec findPlane fxIntersect sxIntersect fyIntersect syIntersect fzIntersect szIntersect (max:Point) (min:Point) runX runY runZ =
        //Very ugly function - finds the correct plane to split on using the heuristics, split on longest axis,
        //buy only if that axis doesn't make more than 60% intersections. In that case, try the next best axis.
        //If no axis can be used, return axis 3, for a leaf creation in the creationFunction.
        if (max.X-min.X >= max.Y-min.Y || runY = false) && (max.X-min.X >= max.Z-min.Z || runZ = false) && runX = true then
            if fxIntersect < 1.6 && sxIntersect < 1.6 then
                0
            else
                findPlane fxIntersect sxIntersect fyIntersect syIntersect fzIntersect szIntersect max min false runY runZ
        else if (max.Y-min.Y > max.X-min.X || runX = false) && (max.Y-min.Y >= max.Z-min.Z || runZ = false) && runY = true then
            if fyIntersect < 1.6 && syIntersect < 1.6 then
                1
            else
                findPlane fxIntersect sxIntersect fyIntersect syIntersect fzIntersect szIntersect max min runX false runZ
        else if (max.Z-min.Z > max.X-min.X || runX = false) && (max.Z-min.Z > max.Y-min.Y || runY = false) && runZ = true then
            if fzIntersect < 1.6 && szIntersect < 1.6 then
                2
            else
                findPlane fxIntersect sxIntersect fyIntersect syIntersect fzIntersect szIntersect max min runX runY false
        else if runX = false && runY = false && runZ = false then
            3
        else 3


    let rec createKDTreeFromList currentDepth (hi:Point) (lo:Point) (boxes:list<ShapeBBox>) = 
        //Function to create the KD-tree
        match boxes with
        | []    -> failwith "There are no shapes to build a tree with!"
        | boxes -> 
            if boxes.Length <= 1 then Leaf(BBox(lo, hi), boxes) //If we only have 1 element left
            else
                let boxesLength = boxes.Length
            
                //Find the split values, and values for the empty space check
                let (splitX, splitY, splitZ, minx, maxx, miny, maxy, minz, maxz) = findSplitValues boxes

                //Find empty space
                let empty = (List.sortBy (fun (_,a,b) -> a/b) 
                              ([(1,(minx - lo.X),(hi.X-lo.X));
                                (2,(hi.X - maxx),(hi.X-lo.X));
                                (3,(miny - lo.Y),(hi.Y-lo.Y));
                                (4,(hi.Y - maxy),(hi.Y-lo.Y));
                                (5,(minz - lo.Z),(hi.Z-lo.Z));
                                (6,(hi.Z - maxz),(hi.Z-lo.Z))]))

                //Empty space threshold for depths of the tree
                let minSpace = match currentDepth with
                               | 0 -> 0.1
                               | 1 -> 0.15
                               | 2 -> 0.2
                               | 3 -> 0.25
                               | _ -> 0.3
                let (ax,em,len) = empty.[empty.Length - 1]

                //Empty space check
                if em/len >= minSpace then
                    match ax with
                    | 1 -> Node(0, minx, BBox(lo, hi), Leaf(BBox(lo, Point(minx, hi.Y, hi.Z)), []), createKDTreeFromList (currentDepth+1) hi (Point(minx, lo.Y, lo.Z)) boxes)
                    | 2 -> Node(0, maxx, BBox(lo, hi), createKDTreeFromList (currentDepth+1) (Point(maxx, hi.Y, hi.Z)) lo boxes, Leaf(BBox(Point(maxx, lo.Y, lo.Z), hi), []))
                    | 3 -> Node(1, miny, BBox(lo, hi), Leaf(BBox(lo, Point(hi.X, miny, hi.Z)), []), createKDTreeFromList (currentDepth+1) hi (Point(lo.X, miny, lo.Z)) boxes)
                    | 4 -> Node(1, maxy, BBox(lo, hi), createKDTreeFromList (currentDepth+1) (Point(hi.X, maxy, hi.Z)) lo boxes, Leaf(BBox(Point(lo.X, maxy, lo.Z), hi), []))
                    | 5 -> Node(2, minz, BBox(lo, hi), Leaf(BBox(lo, Point(hi.X, hi.Y, minz)), []), createKDTreeFromList (currentDepth+1) hi (Point(lo.X, lo.Y, minz)) boxes)
                    | 6 -> Node(2, maxz, BBox(lo, hi), createKDTreeFromList (currentDepth+1) (Point(hi.X, hi.Y, maxz)) lo boxes, Leaf(BBox(Point(lo.X, lo.Y, maxz), hi), []))
                else
            
                    //Split boxes into 2 lists for each axis.
                    let (firstX, secondX, firstY, secondY, firstZ, secondZ) = partitionAfterSelect boxes splitX splitY splitZ

                    //Intersection values for the heuristics check.
                    let (fxIntersect, sxIntersect, fyIntersect, syIntersect, fzIntersect, szIntersect) = 
                        (float firstX.Length)/((float boxesLength)/2.), (float secondX.Length)/((float boxesLength)/2.),
                        (float firstY.Length)/((float boxesLength)/2.), (float secondY.Length)/((float boxesLength)/2.),
                        (float firstZ.Length)/((float boxesLength)/2.), (float secondZ.Length)/((float boxesLength)/2.)

                    //Find the right plane
                    let axis = findPlane fxIntersect sxIntersect fyIntersect syIntersect fzIntersect szIntersect hi lo true true true

                    //If no plane is good enough, create leaf.
                    if axis = 3 then
                        Leaf(BBox(lo, hi), boxes)
                    else

                        //Set the lists to be used, the point to give to the recursive create, and the splitValue from the correct axis
                        let (first, second, firstHigh, 
                             secondLow, splitValue) = if axis = 0 then 
                                                                firstX, secondX, (Point(splitX, hi.Y, hi.Z)), 
                                                                Point(splitX, lo.Y, lo.Z), splitX
                                                            else if axis = 1 then
                                                                firstY, secondY, (Point(hi.X, splitY, hi.Z)), 
                                                                Point(lo.X, splitY, lo.Z), splitY
                                                            else if axis = 2 then
                                                                firstZ, secondZ, (Point(hi.X, hi.Y, splitZ)), 
                                                                Point(lo.X, lo.Y, splitZ), splitZ
                                                            else
                                                                [], [], hi, lo, 0.
            
                        let firstLength = first.Length
                        let secondLength = second.Length

                        //Last checks for creating the leafs/nodes.
                        if firstLength = boxesLength && secondLength = boxesLength then 
                            Leaf(BBox(lo, hi), boxes)
                        else if firstLength = boxesLength then 
                            Node(axis, splitValue, BBox(lo, hi), Leaf(BBox(lo, firstHigh), first), createKDTreeFromList (currentDepth+1) secondLow hi (second))
                        else if secondLength = boxesLength then 
                            Node(axis, splitValue, BBox(lo, hi), createKDTreeFromList (currentDepth+1) lo firstHigh (first), Leaf((BBox(secondLow, hi)), second))
                        else Node(axis, splitValue, BBox(lo, hi), createKDTreeFromList (currentDepth+1) lo firstHigh (first), createKDTreeFromList (currentDepth+1) secondLow hi (second))
                
    let buildKDTree (shapes:array<Shape>) = 
        //Function called from the rest of the program
        let shapeBoxArray = Array.zeroCreate(shapes.Length)
        for i in 0..(shapes.Length-1) do
            let id = i
            let shape = shapes.[i]
            let newShapeBox = ShapeBBox(shape.getBoundingBox (), id)
            shapeBoxArray.[i] <- newShapeBox
        let ShapeBoxList = shapeBoxArray |> Array.toList
        let (KDMaxXYZ, KDMinXYZ) = findMaxMin ShapeBoxList
        if shapeBoxArray.Length < 10 then 
            Leaf(BBox(KDMinXYZ, KDMaxXYZ), ShapeBoxList) //Check for less than 10 shapes. If that is the case, no KD-tree will be built
        else
            createKDTreeFromList 0 KDMaxXYZ KDMinXYZ ShapeBoxList

    let findRayDirectionFromA (a:int) (r:Ray) =
        //Finds the rays direction on a specific axis
        match a with
        | 0 -> r.GetDirection.X
        | 1 -> r.GetDirection.Y
        | 2 -> r.GetDirection.Z

    let findRayOriginFromA (a:int) (r:Ray) =
        //Finds the rays origin on a specific axis
        match a with
        | 0 -> r.GetOrigin.X
        | 1 -> r.GetOrigin.Y
        | 2 -> r.GetOrigin.Z


    let closestHit (shapeBoxes:list<ShapeBBox>) (ray:Ray) (shapes:array<Shape>) =
        // Get closest hitpoint
        let rec findClosestHit (h:HitPoint) t' (shapeBoxes:list<ShapeBBox>) (shapes:array<Shape>) = 
            match shapeBoxes with
            | []    -> h
            | (s:ShapeBBox)::sl -> 
                       let hit = shapes.[s.shape].hitFunction ray
                       if hit.DidHit && hit.Time < t' then findClosestHit hit hit.Time sl shapes
                       else findClosestHit h t' sl shapes
        let hit = findClosestHit (HitPoint(ray)) infinity shapeBoxes shapes
        // Check if the ray hit
        if hit.DidHit then
            // If the ray hit, then return the first hit point
            Some (hit)
        else
            // If not, return none
            None
    
    let order (d:float, left:KDTree, right:KDTree) =
        //Order the lists depending on d
        if d > 0. then (left, right)
        else (right, left)


    let rec searchKDTree (tree:KDTree) (ray:Ray) (t:float) (t':float) (shapes:array<Shape>):HitPoint = 
        //Search the KD-tree
        let SearchKDLeaf (tree:KDTree) (ray:Ray) (t':float) (shapes:array<Shape>) = 
            //When leaf is hit
            match tree with
            | Node(_) -> failwith "Should never be a node here..."
            | Leaf(_, shapeList) -> 
                let option = closestHit shapeList ray shapes
                match option with
                | Some(hit) -> if hit.Time < t' then hit
                               else HitPoint(ray)
                | None ->      HitPoint(ray)
        
        let searchKDNode (tree:KDTree) (ray:Ray) (t:float) (t':float) (shapes:array<Shape>) = 
            //When node is hit
            match tree with
            | Leaf(_) -> failwith "Should never be a leaf here..."
            | Node(axis, value, _, left, right) ->
                let a = axis
                if (findRayDirectionFromA a ray) = 0.0 then
                    if (findRayOriginFromA a ray) <= value then
                        searchKDTree left ray t t' shapes
                    else
                        searchKDTree right ray t t' shapes
                else
                    let tHit = (value - (findRayOriginFromA a ray)) / (findRayDirectionFromA a ray)
                    let (first, second) = order((findRayDirectionFromA a ray), left, right)
                    if tHit <= t || tHit <= 0.0 then
                        searchKDTree second ray t t' shapes
                    else
                        if tHit >= t' then
                            searchKDTree first ray t t' shapes
                        else
                            let hitPoint = searchKDTree first ray t tHit shapes
                            let returnPoint = hitPoint
                            if hitPoint.DidHit then
                                returnPoint
                            else
                                searchKDTree second ray tHit t' shapes
        match tree with 
        | Node(_) -> searchKDNode tree ray t t' shapes
        | Leaf(_) -> SearchKDLeaf tree ray t' shapes
                            

    let traverseKDTree (tree:KDTree) (ray:Ray) (shapes:array<Shape>) = 
        //Traverse function, if we intersect the KD-tree roots bounding box, run search, else return no-hit
        match tree with
        | Node(_, _, bBox, _, _) -> let intersect = bBox.intersect ray
                                    match intersect with
                                    | Some (t, t') -> searchKDTree tree ray t t' shapes
                                    | None -> HitPoint (ray)
        | Leaf(bBox, _)          -> let intersect = bBox.intersect ray
                                    match intersect with
                                    | Some (t, t') -> searchKDTree tree ray t t' shapes
                                    | None -> HitPoint (ray)