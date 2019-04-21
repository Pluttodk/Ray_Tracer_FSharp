namespace Tracer.Basics

open System.Drawing
open System
open Tracer.Basics.Sampling

[<AbstractClass>]
type Camera(position: Tracer.Basics.Point, lookat: Tracer.Basics.Point, up: Vector, zoom: float, width: float, height: float, resX: int, resY: int) =
    // Field of view and orthonormal coordinate system.
    let w = (position - lookat).Normalise
    let v = (up % w).Normalise
    let u = w % v
    let pw = width/float resX
    let ph = height/float resY
    member this.W = w
    member this.U = u
    member this.V = v
    member this.Pw = pw
    member this.Ph = ph
    member this.Position = position
    member this.Lookat = lookat
    member this.Up = up
    member this.Zoom = zoom
    member this.Width = width
    member this.Height = height
    member this.ResX = resX
    member this.ResY = resY

    abstract member CreateRays : int -> int -> Ray []
    
