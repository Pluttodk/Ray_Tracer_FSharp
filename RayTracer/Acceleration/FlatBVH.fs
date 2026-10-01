namespace Tracer.Basics

#nowarn "9" // stackalloc for the traversal stack

module FlatBVH =
    open System
    open Microsoft.FSharp.NativeInterop
    open AccelerationCommon

    [<Struct>]
    type BuildOptions =
        { LeafSize: int
          BinCount: int
          MaxDepth: int }

    let defaultOptions = { LeafSize = 4; BinCount = 16; MaxDepth = 64 }

    /// Count=0 denotes a branch: First/Right are child node indices. Otherwise First is a primitive-index offset.
    [<Struct; System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Sequential)>]
    type NodeData =
        { MinX: float; MinY: float; MinZ: float
          MaxX: float; MaxY: float; MaxZ: float
          First: int
          Count: int
          Right: int }
        member node.IsLeaf = node.Count > 0

    /// A detached FP64 snapshot. PrimitiveCount includes all original source slots, including empty ones.
    /// Unbounded indices must be handled explicitly by a consuming backend.
    [<Sealed>]
    type ExportData internal (nodes: NodeData array, indices: int array, unbounded: int array, empty: int array, depth: int) =
        member _.Nodes = ReadOnlyMemory<NodeData>(nodes)
        member _.PrimitiveIndices = ReadOnlyMemory<int>(indices)
        member _.UnboundedPrimitiveIndices = ReadOnlyMemory<int>(unbounded)
        member _.EmptyPrimitiveIndices = ReadOnlyMemory<int>(empty)
        member _.RootIndex = if nodes.Length = 0 then -1 else 0
        member _.PrimitiveCount = indices.Length + unbounded.Length + empty.Length
        member _.ActivePrimitiveCount = indices.Length + unbounded.Length
        member _.IsFullyBounded = unbounded.Length = 0
        member _.MaxDepth = depth

    [<Struct; NoEquality; NoComparison>]
    type internal Node =
        { Bounds: Bounds
          First: int
          Count: int
          Right: int }

    [<Sealed>]
    type FlatBVHStructure internal (nodes: Node array, indices: int array, depth: int, leaves: int) =
        member internal _.Nodes = nodes
        member internal _.Indices = indices
        member _.NodeCount = nodes.Length
        member _.PrimitiveCount = indices.Length
        member _.MaxDepth = depth
        member _.LeafCount = leaves

    let internal export (tree: FlatBVHStructure) unbounded empty =
        let nodes =
            tree.Nodes |> Array.map (fun node ->
                { MinX = node.Bounds.MinX; MinY = node.Bounds.MinY; MinZ = node.Bounds.MinZ
                  MaxX = node.Bounds.MaxX; MaxY = node.Bounds.MaxY; MaxZ = node.Bounds.MaxZ
                  First = node.First; Count = node.Count; Right = node.Right })
        ExportData(nodes, Array.copy tree.Indices, Array.copy unbounded, Array.copy empty, tree.MaxDepth)

    let private validateOptions options =
        if options.LeafSize < 1 then invalidArg "options" "BVH leaf size must be positive."
        if options.BinCount < 2 || options.BinCount > 64 then invalidArg "options" "BVH bin count must be in [2,64]."
        if options.MaxDepth < 1 || options.MaxDepth > 128 then invalidArg "options" "BVH maximum depth must be in [1,128]."

    let private binIndex count minimum maximum value =
        let width = maximum * 0.5 - minimum * 0.5
        let fraction = (value * 0.5 - minimum * 0.5) / width
        max 0 (min (count - 1) (int (fraction * float count)))

    let internal buildBounded options (boxes: Bounds array) (sourceIndices: int array) =
        validateOptions options
        let indices = Array.copy sourceIndices
        let nodes = ResizeArray<Node>(min 2048 indices.Length)
        let counts = Array.zeroCreate<int> options.BinCount
        let binBounds = Array.create options.BinCount emptyBounds
        let suffixCounts = Array.zeroCreate<int> options.BinCount
        let suffixBounds = Array.create options.BinCount emptyBounds
        let mutable leafCount = 0
        let mutable actualDepth = 0
        let rec build start count depth =
            actualDepth <- max actualDepth (depth + 1)
            let bounds = rangeBounds boxes indices start count
            let nodeIndex = nodes.Count
            nodes.Add { Bounds = bounds; First = start; Count = count; Right = -1 }
            let mutable bestAxis = -1
            let mutable bestBin = -1
            let mutable bestMinimum = 0.
            let mutable bestMaximum = 0.
            let mutable bestCost = float count
            if count > options.LeafSize && depth + 1 < options.MaxDepth then
                let scale = max (bounds.HalfExtent 0) (max (bounds.HalfExtent 1) (bounds.HalfExtent 2))
                if scale > 0. then
                    let x, y, z = bounds.HalfExtent 0 / scale, bounds.HalfExtent 1 / scale, bounds.HalfExtent 2 / scale
                    let useLength = x * y + x * z + y * z = 0.
                    let measure (box: Bounds) =
                        let x, y, z = box.HalfExtent 0 / scale, box.HalfExtent 1 / scale, box.HalfExtent 2 / scale
                        if useLength then x + y + z else x * y + x * z + y * z
                    let parentMeasure = measure bounds
                    for axis = 0 to 2 do
                        let mutable minimum = infinity
                        let mutable maximum = -infinity
                        for position = start to start + count - 1 do
                            let centroid = boxes.[indices.[position]].Centroid axis
                            minimum <- min minimum centroid
                            maximum <- max maximum centroid
                        if minimum < maximum && maximum * 0.5 - minimum * 0.5 > 0. then
                            Array.Clear counts
                            Array.Fill(binBounds, emptyBounds)
                            for position = start to start + count - 1 do
                                let box = boxes.[indices.[position]]
                                let bin = binIndex options.BinCount minimum maximum (box.Centroid axis)
                                counts.[bin] <- counts.[bin] + 1
                                binBounds.[bin] <- union binBounds.[bin] box
                            let mutable rightCount = 0
                            let mutable rightBounds = emptyBounds
                            for bin = options.BinCount - 1 downto 0 do
                                rightCount <- rightCount + counts.[bin]
                                rightBounds <- union rightBounds binBounds.[bin]
                                suffixCounts.[bin] <- rightCount
                                suffixBounds.[bin] <- rightBounds
                            let mutable leftCount = 0
                            let mutable leftBounds = emptyBounds
                            for bin = 0 to options.BinCount - 2 do
                                leftCount <- leftCount + counts.[bin]
                                leftBounds <- union leftBounds binBounds.[bin]
                                let rightCount = suffixCounts.[bin + 1]
                                if leftCount > 0 && rightCount > 0 then
                                    let cost =
                                        1. + (float leftCount * measure leftBounds
                                              + float rightCount * measure suffixBounds.[bin + 1]) / parentMeasure
                                    if cost < bestCost then
                                        bestCost <- cost
                                        bestAxis <- axis
                                        bestBin <- bin
                                        bestMinimum <- minimum
                                        bestMaximum <- maximum
            if bestAxis < 0 then leafCount <- leafCount + 1
            else
                let mutable middle = start
                for position = start to start + count - 1 do
                    let bin = binIndex options.BinCount bestMinimum bestMaximum (boxes.[indices.[position]].Centroid bestAxis)
                    if bin <= bestBin then
                        let saved = indices.[middle]
                        indices.[middle] <- indices.[position]
                        indices.[position] <- saved
                        middle <- middle + 1
                if middle = start || middle = start + count then leafCount <- leafCount + 1
                else
                    let left = build start (middle - start) (depth + 1)
                    let right = build middle (start + count - middle) (depth + 1)
                    nodes.[nodeIndex] <- { Bounds = bounds; First = left; Count = 0; Right = right }
            nodeIndex
        if indices.Length > 0 then build 0 indices.Length 0 |> ignore
        FlatBVHStructure(nodes.ToArray(), indices, actualDepth, leafCount)

    /// Uses the same SAH builder without Shape objects. Bounds must be finite or BBox.Empty.
    let buildFromBoundsWithOptions options (boxes: BBox array) =
        if isNull boxes then nullArg "boxes"
        let bounds = Array.map (ofBoundedBBox "boxes") boxes
        let finite, _ = partitionBounds bounds
        let empty = [| for index = 0 to bounds.Length - 1 do if bounds.[index].IsEmpty then yield index |]
        export (buildBounded options bounds finite) [||] empty

    let buildFromBounds boxes = buildFromBoundsWithOptions defaultOptions boxes

    [<Struct>]
    type private StackEntry =
        { Node: int
          Entry: float }

    /// `fast` and `occluders` are per-slot fast paths (see AccelerationCommon.fastPaths), or empty.
    /// The traversal stack lives on the call stack: pushing both children and popping one grows it by at most
    /// one entry per level, so MaxDepth + 1 entries always suffice, and a query allocates nothing.
    let internal query (tree: FlatBVHStructure) ray data (shapes: Shape array) (fast: IHitTime array)
                       (occluders: IOccluder array) minimum maximum initial stopAtFirst =
        let nodes = tree.Nodes
        if nodes.Length = 0 || minimum >= maximum then initial
        else
            let capacity = tree.MaxDepth + 2
            let stack = Span<StackEntry>(NativePtr.toVoidPtr (NativePtr.stackalloc<StackEntry> capacity), capacity)
            let mutable count = 0
            let mutable result = initial
            let mutable stopped = false
            let entry = intersectNear nodes.[0].Bounds data minimum result.Distance
            if not (Double.IsNaN entry) then
                stack.[0] <- { Node = 0; Entry = entry }
                count <- 1
            while count > 0 && not stopped do
                count <- count - 1
                let item = stack.[count]
                if item.Entry <= result.Distance then
                    let node = nodes.[item.Node]
                    if node.Count > 0 then
                        let mutable offset = node.First
                        let finish = offset + node.Count
                        while offset < finish && not stopped do
                            let index = tree.Indices.[offset]
                            result <-
                                if occluders.Length > 0 && not (isNull occluders.[index]) then
                                    considerOccluder occluders.[index] index ray minimum maximum result
                                elif fast.Length > 0 && not (isNull fast.[index]) then
                                    considerTime fast.[index] index ray minimum maximum result
                                else consider shapes index ray minimum maximum result
                            stopped <- stopAtFirst && result.Found
                            offset <- offset + 1
                    else
                        let a = intersectNear nodes.[node.First].Bounds data minimum result.Distance
                        let b = intersectNear nodes.[node.Right].Bounds data minimum result.Distance
                        let hitA, hitB = not (Double.IsNaN a), not (Double.IsNaN b)
                        // Push the farther child first so the nearer one is visited next.
                        if hitA && hitB then
                            if a <= b then
                                stack.[count] <- { Node = node.Right; Entry = b }
                                stack.[count + 1] <- { Node = node.First; Entry = a }
                            else
                                stack.[count] <- { Node = node.First; Entry = a }
                                stack.[count + 1] <- { Node = node.Right; Entry = b }
                            count <- count + 2
                        elif hitA then
                            stack.[count] <- { Node = node.First; Entry = a }
                            count <- count + 1
                        elif hitB then
                            stack.[count] <- { Node = node.Right; Entry = b }
                            count <- count + 1
            result
