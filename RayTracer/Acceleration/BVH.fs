namespace Tracer.Basics

module BVH = 
    
    // Used for debug, will print to console etc. 
    let debugBuildCounts    = false

    // Type of the BVHTree, with Nodes and Leafs.
    type BVHStructure = | Leaf of List<int>*BBox
                        | Node of BVHStructure*BVHStructure*BBox*int
    
    // Function for sorting a int list, based on array of bounding boxes and axis value.
    let rec sortListByAxis (indexList:list<int>) (boxes:array<BBox>) (axis:int) =
        let sort ax = 
                match ax with
                | 0 -> (fun c -> boxes.[c].lowPoint.X)
                | 1 -> (fun c -> boxes.[c].lowPoint.Y)
                | 2 -> (fun c -> boxes.[c].lowPoint.Z)
                | _ -> invalidArg "sortListByAxis -> invalid axis value" "Axis value needs to be between 0-2."
        List.sortBy (sort axis) indexList

    // Function for getting combined outer low and high from a array og bounding boxes.
    let findOuterBoundingBoxLowHighPoints (boxes:array<BBox>) = 
        if boxes.Length = 0 then failwith "findOuterBoundingBoxLowHighPoints -> Empty array"
        let first = boxes.[0]
        let lowX = Array.fold (fun acc (box:BBox) -> if box.lowPoint.X < acc then box.lowPoint.X else acc) first.lowPoint.X boxes
        let lowY = Array.fold (fun acc (box:BBox) -> if box.lowPoint.Y < acc then box.lowPoint.Y else acc) first.lowPoint.Y boxes
        let lowZ = Array.fold (fun acc (box:BBox) -> if box.lowPoint.Z < acc then box.lowPoint.Z else acc) first.lowPoint.Z boxes
        let highX = Array.fold (fun acc (box:BBox) -> if box.highPoint.X > acc then box.highPoint.X else acc) first.highPoint.X boxes
        let highY = Array.fold (fun acc (box:BBox) -> if box.highPoint.Y > acc then box.highPoint.Y else acc) first.highPoint.Y boxes
        let highZ = Array.fold (fun acc (box:BBox) -> if box.highPoint.Z > acc then box.highPoint.Z else acc) first.highPoint.Z boxes
        Point(lowX, lowY, lowZ), Point(highX, highY, highZ)
    
    // Function for finding the longest axis lenght of a cube
    let findLargestBoundingBoxSideLengths (box:(Point*Point)) : int*float =
        let lowPoint, highPoint = box
        let x, y, z = (highPoint.X - lowPoint.X), (highPoint.Y - lowPoint.Y), (highPoint.Z - lowPoint.Z)

        match (x,y,z) with
        | x, y, z when x>=y && x>=z -> (0, x) // x is longest
        | x, y ,z when y>=x && y>=z -> (1, y) // y is longest
        | _ -> (2, z) // z is longest
        
    
    // Function to find the min-values from a bounding box based on the axis value.
    let findAxisMinMaxValues (box:BBox) axis =
        let lowPoint, highPoint = box.lowPoint, box.highPoint;
        match axis with
        | 0 -> (lowPoint.X, highPoint.X)
        | 1 -> (lowPoint.Y, highPoint.Y)
        | 2 -> (lowPoint.Z, highPoint.Z)
        | _ -> invalidArg "findAxisMinMaxValues invalid axis value" "Axis value needs to be between 0-2."
    
    // Converts from int index to box array
    let rec getBoxArrFromIndexes (indexes:list<int>) (boxes:array<BBox>) : array<BBox> =
        [|for i in 0..(indexes.Length-1) -> boxes.[indexes.[i]]|]
    
    // Function for converting a list of shapes to an array of their bounding boxes.
    let convertShapesToBBoxes (shapes:array<Shape>) : array<BBox> =
        let bboxArr : BBox[] = Array.zeroCreate (shapes.Length)
        for i in 0..bboxArr.Length-1 do
            bboxArr.[i] <- shapes.[i].getBoundingBox()
        bboxArr


 // ######################### BUILD BVH TREE #########################
    let mutable totalNodes = 0
    let mutable totalLeafs = 0
    let mutable totalAxisX = 0
    let mutable totalAxisY = 0
    let mutable totalAxisZ = 0
    let mutable totalSingleShape = 0
    let mutable totalDualShape = 0
    // Build BVH structure from a list of shapes.
    let buildStructure (shapes:array<Shape>) : BVHStructure = 
        if shapes.Length = 0 then failwith "buildStructure -> Unable to build BVH Tree, lists is empty."

        let boxes = convertShapesToBBoxes shapes // Get bounding boxes

        let boxIntList = [0..boxes.Length-1] // Initilize box indexes

        let rec innerNode (intIndexes:int list) : BVHStructure = 
            let boxArr = getBoxArrFromIndexes intIndexes boxes
            let lowPoint, highPoint = findOuterBoundingBoxLowHighPoints boxArr
            let axisToSplit, _ = findLargestBoundingBoxSideLengths (lowPoint, highPoint)

            if debugBuildCounts && axisToSplit=0 then totalAxisX <- totalAxisX+1
            if debugBuildCounts && axisToSplit=1 then totalAxisY <- totalAxisY+1
            if debugBuildCounts && axisToSplit=2 then totalAxisZ <- totalAxisZ+1

            let box = BBox (lowPoint, highPoint)
            
            let sortedList = sortListByAxis intIndexes boxes axisToSplit // Sort list min to max

            match sortedList with
            | [] -> failwith " innerNode -> Empty array"
            | b when b.Length > 2 ->
                let middle = b.Length/2
                let leftList = b.[0..middle-1]
                let rigthList = b.[middle..]

                if debugBuildCounts then totalNodes <- totalNodes+1
                Node (  innerNode leftList, 
                        innerNode rigthList, 
                        box, 
                        axisToSplit)
            | c when c.Length = 2 -> 
                    if boxArr.[0].boundingBoxIntersect boxArr.[1] then
                        if debugBuildCounts then totalDualShape <- totalDualShape+1
                        if debugBuildCounts then totalLeafs <- totalLeafs+1
                        Leaf (c, box)
                    else 
                        if debugBuildCounts then totalNodes <- totalNodes+1
                        Node (  innerNode [c.[0]], 
                                innerNode [c.[1]], 
                                box, 
                                axisToSplit)
            | d when d.Length = 1 ->
                    if debugBuildCounts then totalSingleShape <- totalSingleShape+1
                    if debugBuildCounts then totalLeafs <- totalLeafs+1
                    Leaf (d, box)
            | _ -> failwith "buildBVHTree -> innerNodeTree: Not caught by matching."
        
        innerNode boxIntList
 
    let build (shapes:array<Shape>) : BVHStructure = 
        let structure = buildStructure shapes
        if debugBuildCounts then 
            printfn "totalShapes: %i" shapes.Length 
            printfn "totalNodes: %i" totalNodes 
            printfn "totalLeafs: %i" totalLeafs
            printfn "totalAxisX: %i" totalAxisX
            printfn "totalAxisY: %i" totalAxisY
            printfn "totalAxisZ: %i" totalAxisZ
            printfn "totalSingleShape: %i" totalSingleShape
            printfn "totalDualShape: %i" totalDualShape
        structure
    

 // ######################### TRAVERSAL BVH STRUCTURE #########################
    // Function swaps the order if d is not positive
    let order d (left:BVHStructure) (right:BVHStructure) = 
        match d with
        | d when d > 0 -> (left, right)
        | _            -> (right, left)

    // Function checking for leaf.
    let isLeaf = function
    | Leaf (_,_) -> true
    | _ -> false

    // Function get ray direction based on the axis.
    let getRayDirectionValue (ray:Ray) (axis:int) : int =
        match axis with
        | 0 -> (int ray.GetDirection.X)
        | 1 -> (int ray.GetDirection.Y)
        | 2 -> (int ray.GetDirection.Z)
        | _ -> invalidArg "Input out of bound" "Axis (x, y , z) paramter must 0, 1 or 2"

    // Get bounding box from structure element.
    let getBbox (structure:BVHStructure) : BBox = 
        match structure with
        | Node (_,_,bbox,_) ->  bbox
        | Leaf (_,bbox) ->      bbox

    // Functions finds closest hit of a ray in structure.
    let closestHit (structure:BVHStructure) (ray:Ray) (shapes:array<Shape>) : HitPoint option =
        match structure with
        |   Leaf (shapesRef, _) ->  let mutable closestHit = None
                                    let mutable closestDist = infinity
                                    for shapeRef in shapesRef do
                                        let hitPoint = shapes.[shapeRef].hitFunction ray
                                        let dist = hitPoint.Time
                                        if hitPoint.DidHit && dist < closestDist then
                                            closestDist <- dist
                                            closestHit <- Some hitPoint
                                    closestHit
        | _ ->  None

    // Function performs recursive searh in the structrue, with a maximum distance from the ray origin.
    let rec searchStructure (structure:BVHStructure) (ray:Ray) (shapes:array<Shape>) (tmax:float) =
        let nodeBBox = getBbox structure
        match nodeBBox.intersect ray with  
        | Some (t, t')  ->  
                        if t<tmax then 
                            if isLeaf structure then
                                let checkForHit = (closestHit structure ray shapes)
                                match checkForHit with
                                | Some hitPoint when hitPoint.Time<tmax -> Some hitPoint
                                | _ -> None
                            else 
                                match structure with
                                | Node (left, right, _, axis) ->
                                    let dir = getRayDirectionValue ray axis
                                    let fst, snd = order dir left right
                                    match searchStructure fst ray shapes tmax with
                                    | Some hitPointfst ->
                                            let someResult = searchStructure structure ray shapes hitPointfst.Time
                                            match someResult with
                                            | Some hitPointSec -> Some hitPointSec
                                            | _ -> Some hitPointfst            
                                    | _ -> searchStructure snd ray shapes tmax
                                | _ -> None
                        else None
        | None -> None
    
    // Function for traversal of the structure.
    let traverse (structure:BVHStructure) (ray:Ray) (shapes:array<Shape>) : HitPoint = 
        match searchStructure structure ray shapes infinity with
        | Some r -> r
        | None -> HitPoint ray