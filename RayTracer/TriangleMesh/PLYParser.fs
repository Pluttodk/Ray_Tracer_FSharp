module Tracer.Basics.PLYParser

open FParsec
open System.IO
open System
open Tracer.Basics
open System.Threading.Tasks
open System.Diagnostics


type UserState = unit
type Parser<'t> = Parser<'t,UserState>


type Vertex(x,y,z,nx, ny, nz, u, v) = 
    let mutable n : Vector = Vector(0.,0.,0.)
    member this.x = x
    member this.y = y
    member this.z = z
    member this.nx = nx
    member this.ny = ny
    member this.nz = nz
    member this.u = u
    member this.v = v
    member this.normal
        with get() = (n : Vector) 
        and set(value : Vector) = n <- (n |> ( + ) value) 


let parse parser str = 
    match run parser str with
    | Success(result,_,_) ->  result
    | Failure (errorMsg,_,_) ->  failwith(errorMsg)

let parseBool parser str = 
    match run parser str with
    | Success(_) ->  true
    | Failure (_) ->  false

let typeParser str = 
    match str with
    | "char" -> 1
    | "uchar" -> 1
    | "int8" -> 1
    | "uint8" -> 1
    | "short" -> 2
    | "int16" -> 2
    | "ushort" -> 2
    | "uint16" -> 2
    | "int" -> 3
    | "int32" -> 3
    | "uint" -> 3
    | "uint32" -> 3
    | "float" -> 4
    | "float32" -> 4
    | "double" -> 5
    | "float64" -> 5
    | _ -> 0

let WhiteSpace = pstring " "

let findVertexFromArray (floatsList : float list) (posArray : int array)= 
    let checkArrayIsWithinPLYFile ((a:float list),(b:int array),c) = 
        if (b.[c] = 0) then None else Some a.[b.[c]-1]

    let x = floatsList.[((posArray.[0])-1)]
    let y = floatsList.[((posArray.[1])-1)]
    let z = floatsList.[((posArray.[2])-1)]
    let nx = checkArrayIsWithinPLYFile(floatsList,posArray,3)
    let ny = checkArrayIsWithinPLYFile(floatsList,posArray,4)
    let nz = checkArrayIsWithinPLYFile(floatsList,posArray,5)
    let u = checkArrayIsWithinPLYFile(floatsList,posArray,6)
    let v = checkArrayIsWithinPLYFile(floatsList,posArray,7)
    new Vertex(x,y,z,nx,ny,nz,u,v)

let parsePLY (filepath:string) = 
    use sr = new StreamReader(filepath)
    let mutable headerCharCount = 0
    let proceed (s:StreamReader) = 
        let nextLine = s.ReadLine()
        headerCharCount <- headerCharCount + nextLine.Length + 1
        nextLine
    let mutable nextLine = proceed sr
    let mutable numberOfProperty = 1
    let mutable vertexProperty = []
    let mutable faceProperty = []
    let mutable skipAbleProperty = []
    let result = parseBool (pstring "ply") (nextLine)
    match result with
    | true -> 
        //printfn "Started Parsing..."
        headerCharCount <- headerCharCount + nextLine.ToCharArray().Length
        nextLine <- proceed sr

        let isAscii = parseBool (pstring "format ascii " .>> pfloat) (nextLine)
        let formatBoolean = (pstring "format binary_little_endian " .>> pfloat) 
        let isBigEndian = parseBool (pstring "format binary_big_endian " .>> pfloat) nextLine
        let isBoolean = (parseBool (formatBoolean) (nextLine)) || isBigEndian
            

        if (not (isBoolean) && not isAscii) then failwith ("Parsing Error: TAMPERED PLY FILE")

        nextLine <- proceed sr
        let arraySizeParser = pstring "element vertex " >>. pint32
        let isArraySize s = parseBool arraySizeParser s
        while (not (isArraySize nextLine)) do
            nextLine <- proceed sr

        let arraySize = parse arraySizeParser nextLine
        let triangleArray = Array.zeroCreate arraySize
        let isEndOfHeader s = parseBool (pstring "end_header") s
        let vertexPosition : int array = Array.zeroCreate 9
        let startWithProperty (s:string) = parseBool (pstring "property" .>> (anyString (s.Length-8))) s
        let readPropertyParser = 
            nextLine <- proceed sr
            while (startWithProperty nextLine) do
                let lineSplit = nextLine.Split [|' '|]
                match lineSplit.[2] with
                    | "x" -> vertexPosition.[0] <- numberOfProperty
                    | "y" -> vertexPosition.[1] <- numberOfProperty
                    | "z" -> vertexPosition.[2] <- numberOfProperty
                    | "nx" -> vertexPosition.[3] <- numberOfProperty
                    | "ny" -> vertexPosition.[4] <- numberOfProperty
                    | "nz" -> vertexPosition.[5] <- numberOfProperty
                    | "u" -> vertexPosition.[6] <- numberOfProperty
                    | "v" -> vertexPosition.[7] <- numberOfProperty
                    | _ -> vertexPosition.[8] <- numberOfProperty
                let typeVal = typeParser lineSplit.[1]
                vertexProperty <- List.append vertexProperty [typeVal]
                nextLine <- proceed sr
                numberOfProperty <- numberOfProperty + 1
        readPropertyParser

        let faceLength = parse (pstring "element face " >>. pint32) nextLine
        let faceArray = Array.zeroCreate faceLength
        nextLine <- proceed sr
        while (startWithProperty nextLine) do
            let lineSplit = nextLine.Split[|' '|]
            let parseListProps = pstring "property list " >>. (anyString (nextLine.Length-28)) .>> pstring "vertex_indices"
            if (parseBool (parseListProps) nextLine) then
                let typeVal = typeParser lineSplit.[2]
                faceProperty <- List.append faceProperty [typeVal]
                let listTypeVal = typeParser lineSplit.[3]
                faceProperty <- List.append faceProperty [listTypeVal]
            else 
                let typeVal = typeParser lineSplit.[1]
                skipAbleProperty <- List.append skipAbleProperty [typeVal]
            nextLine <- proceed sr
        //(nextLine <- sr.ReadLine()) |> ignore
        match isAscii,isBoolean with
        //ASCII PARSING
        | true,_ -> 
            //VERTEX PARSING
            let lines = Array.zeroCreate(triangleArray.Length + faceArray.Length)
            //READING UPCOMING LINES FOR USING PARALLEL
            for i in 0..lines.Length-1 do 
                lines.[i] <- sr.ReadLine()
            let listFloatParser = (sepBy pfloat WhiteSpace)
            let listIntParser = (sepBy pint32 WhiteSpace)
            let hasExstraWS = lines.[0].EndsWith(" ")

            Parallel.For(0, lines.Length, fun i ->
                let line = 
                    let tempLine = lines.[i]
                    if(hasExstraWS) then
                        tempLine.Substring(0,tempLine.Length-1)
                    else tempLine
                if (i < triangleArray.Length) then
                    let listFloat = parse listFloatParser line
                    triangleArray.[i] <- findVertexFromArray listFloat vertexPosition
                else
                    let k = i - triangleArray.Length
                    let listInt = parse listIntParser line
                    faceArray.[k] <- listInt) |> ignore

            (triangleArray,faceArray)
        | _,true -> 
            let stream = sr.BaseStream
            stream.Seek((int64 (headerCharCount - 3)), SeekOrigin.Begin) |> ignore
            let br = new BinaryReader(stream)
            let vertexProbs = vertexProperty
            for j in 0..triangleArray.Length-1 do 
                let vertexProps = Array.zeroCreate(vertexProbs.Length)
                for k in 0..(vertexProbs.Length-1) do
                    let f = 
                        match vertexProbs.[k] with
                        | 1 -> 
                            let buffer : byte[] = Array.zeroCreate(1)
                            br.Read(buffer,0,1) |> ignore
                            if(isBigEndian) then
                                Array.Reverse(buffer)
                            (float buffer.[0])
                        | 2 -> 
                            let buffer : byte[] = Array.zeroCreate(2)
                            br.Read(buffer,0,2) |> ignore
                            if(isBigEndian) then
                                Array.Reverse(buffer)
                            (float (BitConverter.ToInt16 (buffer, 0)))
                        | 3 -> 
                            let buffer : byte[] = Array.zeroCreate(4)
                            br.Read(buffer,0,4) |> ignore
                            if(isBigEndian) then
                                Array.Reverse(buffer)
                            (float (BitConverter.ToInt32 (buffer, 0)))
                        | 4 -> 
                            let buffer : byte[] = Array.zeroCreate(4)
                            br.Read(buffer,0,4) |> ignore
                            if(isBigEndian) then
                                Array.Reverse(buffer)
                            (float (BitConverter.ToSingle (buffer, 0)))
                        | 5 -> 
                            let buffer : byte[] = Array.zeroCreate(8)
                            br.Read(buffer,0,8) |> ignore
                            if(isBigEndian) then
                                Array.Reverse(buffer)
                            (float (BitConverter.ToDouble (buffer, 0)))
                        | _ -> 0.0
                    vertexProps.[k] <- f
                triangleArray.[j] <- findVertexFromArray (vertexProps |> Array.toList) vertexPosition
            //Parsing Triangles
            for j in 0..faceArray.Length-1 do 
                let numbers = Array.zeroCreate(4)
                let typeToByte i = 
                    match i with
                    | 1 -> 1
                    | 2 -> 2
                    | 3 -> 4
                    | 4 -> 4
                    | 5 -> 8
                    | _ -> 0
                for k in 0..(skipAbleProperty.Length-1) do 
                    let dumpBuffer : byte[] = Array.zeroCreate(typeToByte skipAbleProperty.[k])
                    br.Read(dumpBuffer,0,dumpBuffer.Length) |> ignore
                let intValue n = 
                    match n with
                        | 1 -> 
                            let buffer : byte[] = Array.zeroCreate(1)
                            br.Read(buffer,0,1) |> ignore
                            if(isBigEndian) then
                                Array.Reverse(buffer)
                            (int buffer.[0])
                        | 2 -> 
                            let buffer : byte[] = Array.zeroCreate(2)
                            br.Read(buffer,0,2) |> ignore
                            if(isBigEndian) then
                                Array.Reverse(buffer)
                            (int (BitConverter.ToInt16 (buffer, 0)))
                        | 3 -> 
                            let buffer : byte[] = Array.zeroCreate(4)
                            br.Read(buffer,0,4) |> ignore
                            if(isBigEndian) then
                                Array.Reverse(buffer)
                            (int (BitConverter.ToInt32 (buffer, 0)))
                        | 4 -> 
                            let buffer : byte[] = Array.zeroCreate(4)
                            br.Read(buffer,0,4) |> ignore
                            if(isBigEndian) then
                                Array.Reverse(buffer)
                            (int (BitConverter.ToSingle (buffer, 0)))
                        | 5 -> 
                            let buffer : byte[] = Array.zeroCreate(8)
                            br.Read(buffer,0,8) |> ignore
                            if(isBigEndian) then
                                Array.Reverse(buffer)
                            (int (BitConverter.ToDouble (buffer, 0)))
                        | _ -> 0
                faceArray.[j] <- [intValue faceProperty.[0] ; intValue faceProperty.[1]; intValue faceProperty.[1]; intValue faceProperty.[1]]
            (triangleArray,faceArray)
        | _,_ -> failwith ("Parsing Error: TAMPERED PLY FILE")
    | false -> failwith ("Not a ply file")