namespace Tracer.BaseShape

open Tracer.Basics
open Tracer.Basics.Textures

[<AbstractClass>]
type BaseShape()=
    abstract member toShape: Texture -> Shape


type BaseRectangle(bottomLeft:Point, topLeft:Point, bottomRight:Point)=
    inherit BaseShape()
    member this.bottomLeft = bottomLeft
    member this.topLeft = topLeft
    member this.bottomRight = bottomRight
    member this.width = bottomRight.X - bottomLeft.X
    member this.height = topLeft.Y - bottomLeft.Y
    override this.toShape (mat:Texture) = new Rectangle(bottomLeft, topLeft, bottomRight, mat) :> Shape

                    
type BaseDisc(center:Point, radius:float)=
    inherit BaseShape()
    member this.center = center
    member this.radius = radius
    override this.toShape (mat:Texture) = new Disc(center, radius, mat) :> Shape
    

type BaseTriangle(a:Point, b:Point, c:Point)=
    inherit BaseShape()
    member this.a = a
    member this.b = b
    member this.c = c
    member this.u = a-b //in case of errors try swithing a and b around
    member this.v = a-c // same here
    member this.n = this.u.CrossProduct this.v
    override this.toShape tex = 
        let material = Textures.getFunc tex 0. 0.
        new Triangle(a, b, c, material) :> Shape


type BaseSphere(origin: Point, radius: float) = 
    inherit BaseShape()
    member this.origin = origin
    member this.radius = radius
    override this.toShape (mat:Texture) = new SphereShape(origin, radius, mat) :> Shape


type BaseHollowCylinder(center:Point, radius:float, height:float) =
    inherit BaseShape()
    member this.center = center
    member this.radius = radius
    member this.height = height
    override this.toShape (mat:Texture) = new HollowCylinder(center, radius, height, mat) :> Shape
