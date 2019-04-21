namespace Tracer.Basics

type Scene(shapes: Shape list, lights: Light list, ambient : AmbientLight, maxBounces : int) = 

    let backgroundColour = new Colour(0., 0., 0.)
    member this.Shapes = 
        let lightShapes = 
            [for light in lights do
                match light with 
                | :? AreaLight as a -> yield a.Shape
                | :? EnvironmentLight as e -> yield e.Sphere :> Shape
                | _ -> ()]

        shapes @ lightShapes
    member this.Ambient = ambient
    member this.Lights = lights
    member this.BackgroundColour = backgroundColour 
    member this.MaxBounces = maxBounces