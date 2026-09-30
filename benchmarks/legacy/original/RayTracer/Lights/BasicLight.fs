namespace Tracer.Basics


//- POINT LIGHT
type PointLight(colour: Colour, intensity: float, position: Point) = 
    inherit Light(colour, intensity)

    // Local methods
    member this.Position = position
    
    // Overwritten methods
    override this.GetColour _ = new Colour(colour.R * intensity, colour.G * intensity, colour.B * intensity)
    override this.GetDirectionFromPoint hitPoint = (position - hitPoint.Point).Normalise
    override this.GetShadowRay hitPoint = [| new Ray(hitPoint.EscapedPoint, (position - hitPoint.EscapedPoint).Normalise) |]
    override this.GetGeometricFactor _ = 1.
    override this.GetProbabilityDensity _ = 1.


//- DIRECTIONAL LIGHT  
type DirectionalLight(colour: Colour, intensity: float, direction: Vector) = 
    inherit Light(colour, intensity)

    // Local methods
    member this.Direction = direction

    // Overwritten mthods
    override this.GetColour hitPoint = new Colour(colour.R * intensity, colour.G * intensity, colour.B * intensity)
    override this.GetDirectionFromPoint hitPoint = direction.Normalise
    override this.GetShadowRay hitPoint = [| new Ray(hitPoint.EscapedPoint, direction.Normalise) |]
    override this.GetGeometricFactor point = 1.
    override this.GetProbabilityDensity hitPoint = 1.

