module Tracer.Basics.PLYParser

open System
open System.Buffers.Binary
open System.Globalization
open System.IO
open System.Text
open FParsec
open Tracer.Basics

type UserState = unit
type Parser<'t> = Parser<'t, UserState>

type Vertex(x: float, y: float, z: float, nx: float option, ny: float option,
            nz: float option, u: float option, v: float option, normal: Vector) =
    member _.x = x
    member _.y = y
    member _.z = z
    member _.nx = nx
    member _.ny = ny
    member _.nz = nz
    member _.u = u
    member _.v = v
    member _.normal = normal
    new(x, y, z, nx, ny, nz, u, v) =
        let n =
            match nx, ny, nz with
            | Some a, Some b, Some c -> Vector(a,b,c).Normalise
            | _ -> Vector.Zero
        Vertex(x, y, z, nx, ny, nz, u, v, n)

let parse parser str =
    match run parser str with
    | Success(result, _, _) -> result
    | Failure(message, _, _) -> failwith message

let parseBool parser str =
    match run parser str with
    | Success _ -> true
    | Failure _ -> false

// Retained for callers of the old parser helpers; decoding below uses distinct signed types.
let typeParser = function
    | "char" | "uchar" | "int8" | "uint8" -> 1
    | "short" | "ushort" | "int16" | "uint16" -> 2
    | "int" | "uint" | "int32" | "uint32" -> 3
    | "float" | "float32" -> 4
    | "double" | "float64" -> 5
    | _ -> 0

let WhiteSpace: Parser<string> = pstring " "

let findVertexFromArray (values: float list) (positions: int array) =
    let optional index = if positions.[index] = 0 then None else Some values.[positions.[index]-1]
    Vertex(values.[positions.[0]-1], values.[positions.[1]-1], values.[positions.[2]-1],
           optional 3, optional 4, optional 5, optional 6, optional 7)

type private ScalarType = Int8 | UInt8 | Int16 | UInt16 | Int32 | UInt32 | Float32 | Float64
type private Property = Scalar of string * ScalarType | List of string * ScalarType * ScalarType
type private Element = { Name: string; Count: int; Properties: Property array }
type private Format = Ascii | LittleEndian | BigEndian

let private invalid message = raise (InvalidDataException("PLY: " + message))

let private scalarType name =
    match name with
    | "char" | "int8" -> Int8
    | "uchar" | "uint8" -> UInt8
    | "short" | "int16" -> Int16
    | "ushort" | "uint16" -> UInt16
    | "int" | "int32" -> Int32
    | "uint" | "uint32" -> UInt32
    | "float" | "float32" -> Float32
    | "double" | "float64" -> Float64
    | _ -> invalid ("unsupported scalar type '" + name + "'.")

let private integerType = function Float32 | Float64 -> false | _ -> true

type private Input(stream: Stream) =
    let mutable pending = -1
    let mutable newline = 0
    member _.ReadByte() =
        if pending < 0 then stream.ReadByte()
        else let value = pending in pending <- -1; value
    member this.HeaderLine() =
        let bytes = ResizeArray<byte>()
        let mutable finished = false
        while not finished do
            let value = this.ReadByte()
            if value < 0 then invalid "unexpected end of stream in the header."
            elif value = 10 then
                if newline = 0 then newline <- 1
                finished <- true
            elif value = 13 then
                // A CR-only header must not consume a binary payload's first byte when it is 0x0A.
                if newline <> 3 then
                    let next = this.ReadByte()
                    if next = 10 then newline <- 2
                    else
                        pending <- next
                        newline <- 3
                finished <- true
            else
                bytes.Add(byte value)
                if bytes.Count > 1048576 then invalid "header line exceeds one MiB."
        Encoding.UTF8.GetString(bytes.ToArray())
    member this.Token() =
        let mutable value = this.ReadByte()
        while value >= 0 && Char.IsWhiteSpace(char value) do value <- this.ReadByte()
        if value < 0 then invalid "unexpected end of stream in ASCII data."
        let token = StringBuilder()
        while value >= 0 && not (Char.IsWhiteSpace(char value)) do
            token.Append(char value) |> ignore
            if token.Length > 128 then invalid "numeric token is too long."
            value <- this.ReadByte()
        token.ToString()

let private readHeader (input: Input) =
    if input.HeaderLine().TrimStart('\uFEFF').Trim() <> "ply" then invalid "missing 'ply' signature."
    let elements = ResizeArray<Element>()
    let properties = ResizeArray<Property>()
    let mutable current: (string * int) option = None
    let mutable format: Format option = None
    let mutable ended = false
    let mutable lineCount = 1
    let flush() =
        match current with
        | None -> ()
        | Some(name, count) ->
            elements.Add { Name = name; Count = count; Properties = properties.ToArray() }
            properties.Clear()
    while not ended do
        lineCount <- lineCount + 1
        if lineCount > 100000 then invalid "header contains too many lines."
        let fields = input.HeaderLine().Split([|' '; '\t'|], StringSplitOptions.RemoveEmptyEntries)
        match fields with
        | [||] -> ()
        | fields when fields.[0] = "comment" || fields.[0] = "obj_info" -> ()
        | [|"format"; encoding; "1.0"|] ->
            if format.IsSome then invalid "duplicate format declaration."
            format <-
                Some(match encoding with
                     | "ascii" -> Ascii
                     | "binary_little_endian" -> LittleEndian
                     | "binary_big_endian" -> BigEndian
                     | _ -> invalid ("unsupported encoding '" + encoding + "'."))
        | [|"element"; name; size|] ->
            let success, count = System.Int32.TryParse(size, NumberStyles.None, CultureInfo.InvariantCulture)
            if not success then invalid ("invalid count for element '" + name + "'.")
            if (elements |> Seq.exists (fun element -> element.Name = name))
               || (current |> Option.exists (fun (previous, _) -> previous = name)) then
                invalid ("duplicate element '" + name + "'.")
            flush()
            current <- Some(name, count)
        | [|"property"; typ; name|] ->
            if current.IsNone then invalid "property declared before any element."
            let property = Scalar(name, scalarType typ)
            if properties |> Seq.exists (function Scalar(n, _) | List(n, _, _) -> n = name) then
                invalid ("duplicate property '" + name + "'.")
            properties.Add property
        | [|"property"; "list"; countType; valueType; name|] ->
            if current.IsNone then invalid "property declared before any element."
            let countType, valueType = scalarType countType, scalarType valueType
            if not (integerType countType) then invalid "a list count must use an integer type."
            if properties |> Seq.exists (function Scalar(n, _) | List(n, _, _) -> n = name) then
                invalid ("duplicate property '" + name + "'.")
            properties.Add(List(name, countType, valueType))
        | [|"end_header"|] -> ended <- true
        | _ -> invalid (sprintf "unsupported or malformed declaration on header line %d." lineCount)
    flush()
    match format with
    | None -> invalid "missing format declaration (only version 1.0 is supported)."
    | Some format -> format, elements.ToArray()

let parseIndexedPLYStream (stream: Stream) : Vertex array * int array array =
    if isNull stream || not stream.CanRead then invalidArg "stream" "PLY input must be readable."
    let input = Input(stream)
    let format, elements = readHeader input
    let vertexElement =
        elements |> Array.tryFind (fun element -> element.Name = "vertex")
        |> Option.defaultWith (fun () -> invalid "missing vertex element.")
    let scalarNames =
        vertexElement.Properties |> Array.choose (function Scalar(name, _) -> Some name | _ -> None)
        |> Set.ofArray
    for name in ["x"; "y"; "z"] do
        if not (scalarNames.Contains name) then invalid ("vertex property '" + name + "' must be scalar.")
    let normals = ["nx"; "ny"; "nz"] |> List.filter scalarNames.Contains
    if not normals.IsEmpty && normals.Length <> 3 then invalid "vertex normals require nx, ny and nz."
    let uvNames =
        [("u","v"); ("s","t"); ("texture_u","texture_v")]
        |> List.tryFind (fun (u,v) -> scalarNames.Contains u || scalarNames.Contains v)
    match uvNames with
    | Some(u,v) when not (scalarNames.Contains u && scalarNames.Contains v) ->
        invalid "texture coordinates require a complete pair."
    | _ -> ()
    let faceElement = elements |> Array.tryFind (fun element -> element.Name = "face")
    match faceElement with
    | Some face when face.Count > 0 ->
        let indexProperties =
            face.Properties |> Array.filter (function Scalar(n, _) | List(n, _, _) -> n = "vertex_indices" || n = "vertex_index")
        match indexProperties with
        | [|List(_, _, itemType)|] when integerType itemType -> ()
        | _ -> invalid "faces require one integer vertex_indices (or vertex_index) list."
    | _ -> ()

    let scratch = Array.zeroCreate<byte> 8
    let mutable elementName, propertyName, rowIndex = "", "", 0
    let context() = sprintf "%s[%d].%s" elementName rowIndex propertyName
    let readBytes count =
        for i = 0 to count-1 do
            let value = input.ReadByte()
            if value < 0 then invalid ("short binary read at " + context() + ".")
            scratch.[i] <- byte value
    let asciiScalar typ =
        let token = input.Token()
        let integer = NumberStyles.Integer
        let culture = CultureInfo.InvariantCulture
        let malformed() = invalid (sprintf "invalid %A value '%s' at %s." typ token (context()))
        match typ with
        | Int8 -> match SByte.TryParse(token, integer, culture) with true, n -> float n | _ -> malformed()
        | UInt8 -> match Byte.TryParse(token, integer, culture) with true, n -> float n | _ -> malformed()
        | Int16 -> match System.Int16.TryParse(token, integer, culture) with true, n -> float n | _ -> malformed()
        | UInt16 -> match System.UInt16.TryParse(token, integer, culture) with true, n -> float n | _ -> malformed()
        | Int32 -> match System.Int32.TryParse(token, integer, culture) with true, n -> float n | _ -> malformed()
        | UInt32 -> match System.UInt32.TryParse(token, integer, culture) with true, n -> float n | _ -> malformed()
        | Float32 -> match Single.TryParse(token, NumberStyles.Float, culture) with true, n -> float n | _ -> malformed()
        | Float64 -> match Double.TryParse(token, NumberStyles.Float, culture) with true, n -> n | _ -> malformed()
    let binaryScalar typ =
        let little = format = LittleEndian
        match typ with
        | Int8 ->
            readBytes 1
            let n = int scratch.[0]
            float (if n >= 128 then n-256 else n)
        | UInt8 -> readBytes 1; float scratch.[0]
        | Int16 ->
            readBytes 2
            let bytes = ReadOnlySpan<byte>(scratch, 0, 2)
            float (if little then BinaryPrimitives.ReadInt16LittleEndian bytes else BinaryPrimitives.ReadInt16BigEndian bytes)
        | UInt16 ->
            readBytes 2
            let bytes = ReadOnlySpan<byte>(scratch, 0, 2)
            float (if little then BinaryPrimitives.ReadUInt16LittleEndian bytes else BinaryPrimitives.ReadUInt16BigEndian bytes)
        | Int32 ->
            readBytes 4
            let bytes = ReadOnlySpan<byte>(scratch, 0, 4)
            float (if little then BinaryPrimitives.ReadInt32LittleEndian bytes else BinaryPrimitives.ReadInt32BigEndian bytes)
        | UInt32 ->
            readBytes 4
            let bytes = ReadOnlySpan<byte>(scratch, 0, 4)
            float (if little then BinaryPrimitives.ReadUInt32LittleEndian bytes else BinaryPrimitives.ReadUInt32BigEndian bytes)
        | Float32 ->
            readBytes 4
            let bytes = ReadOnlySpan<byte>(scratch, 0, 4)
            let bits = if little then BinaryPrimitives.ReadInt32LittleEndian bytes else BinaryPrimitives.ReadInt32BigEndian bytes
            float (BitConverter.Int32BitsToSingle bits)
        | Float64 ->
            readBytes 8
            let bytes = ReadOnlySpan<byte>(scratch, 0, 8)
            let bits = if little then BinaryPrimitives.ReadInt64LittleEndian bytes else BinaryPrimitives.ReadInt64BigEndian bytes
            BitConverter.Int64BitsToDouble bits
    let readScalar typ =
        let value = if format = Ascii then asciiScalar typ else binaryScalar typ
        if not (Double.IsFinite value) then invalid ("non-finite value at " + context() + ".")
        value
    let vertices = ResizeArray<Vertex>(min vertexElement.Count 4096)
    let faces = ResizeArray<int array>()
    for element in elements do
        elementName <- element.Name
        for row = 0 to (if element.Properties.Length = 0 then -1 else element.Count-1) do
            rowIndex <- row
            let mutable x, y, z = 0., 0., 0.
            let mutable nx, ny, nz, u, v = None, None, None, None, None
            for property in element.Properties do
                let name = match property with Scalar(n, _) | List(n, _, _) -> n
                propertyName <- name
                match property with
                | Scalar(_, typ) ->
                    let value = readScalar typ
                    if element.Name = "vertex" then
                        match name with
                        | "x" -> x <- value
                        | "y" -> y <- value
                        | "z" -> z <- value
                        | "nx" -> nx <- Some value
                        | "ny" -> ny <- Some value
                        | "nz" -> nz <- Some value
                        | _ ->
                            match uvNames with
                            | Some(uname, _) when name = uname -> u <- Some value
                            | Some(_, vname) when name = vname -> v <- Some value
                            | _ -> ()
                | List(_, countType, valueType) ->
                    let length = readScalar countType
                    if length < 0. || length > float System.Int32.MaxValue then
                        invalid ("invalid list count at " + context() + ".")
                    let count = int length
                    if element.Name = "face" && (name = "vertex_indices" || name = "vertex_index") then
                        if count < 3 || count > vertexElement.Count then
                            invalid ("a face must contain at least three distinct, valid vertex indices at " + context() + ".")
                        let indices = ResizeArray<int>(min count 64)
                        for i = 0 to count-1 do
                            let index = readScalar valueType
                            if index < 0. || index >= float vertexElement.Count then
                                invalid ("vertex index out of range at " + context() + ".")
                            indices.Add(int index)
                        faces.Add(indices.ToArray())
                    else
                        for _ = 1 to count do readScalar valueType |> ignore
            if element.Name = "vertex" then vertices.Add(Vertex(x,y,z,nx,ny,nz,u,v))
    vertices.ToArray(), faces.ToArray()

let parseIndexedPLY (filepath: string) =
    use stream = new FileStream(filepath, FileMode.Open, FileAccess.Read, FileShare.Read, 65536, FileOptions.SequentialScan)
    parseIndexedPLYStream stream

let parsePLYStream stream =
    let vertices, faces = parseIndexedPLYStream stream
    vertices, faces |> Array.map (fun indices -> indices.Length :: Array.toList indices)

let parsePLY filepath =
    let vertices, faces = parseIndexedPLY filepath
    vertices, faces |> Array.map (fun indices -> indices.Length :: Array.toList indices)
