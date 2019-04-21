module ShapeTest

open Tracer.BaseShape
open Assert
open Tracer.Basics
open System


let allTest = 
    let rectangle = new Rectangle(Point(0.,0.,0.), Point(0.,1.,0.), Point(1.,0.,0.), Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.), 1., Colour(1.,1.,1.), 1.)))
    let disc = new Disc(Point(0.,0.,0.), 2., Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.), 1., Colour(1.,1.,1.), 1.)))
    let triangle = new Triangle(Point(0.,0.,0.), Point(0.,1.,0.), Point(1.,0.,0.), MatteMaterial(Colour(1.,1.,1.), 1., Colour(1.,1.,1.), 1.))
    let sphere = new SphereShape(Point(0.,0.,0.), 2., Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.), 1., Colour(1.,1.,1.), 1.)))
    let hollowCylinder = new HollowCylinder(Point(0.,0.,0.), 2., 4., Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.), 1., Colour(1.,1.,1.), 1.)))
    let solidCylinder = new SolidCylinder(Point(0.,0.,0.), 2., 4., Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.), 1., Colour(1.,1.,1.), 1.)),
                                                                   Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.), 1., Colour(1.,1.,1.), 1.)),
                                                                   Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.), 1., Colour(1.,1.,1.), 1.)))
    let box = new Box(Point(0.,0.,0.), Point(1.,1.,1.), Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.), 1., Colour(1.,1.,1.), 1.)), 
                                                        Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.), 1., Colour(1.,1.,1.), 1.)), 
                                                        Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.), 1., Colour(1.,1.,1.), 1.)), 
                                                        Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.), 1., Colour(1.,1.,1.), 1.)), 
                                                        Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.), 1., Colour(1.,1.,1.), 1.)), 
                                                        Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.), 1., Colour(1.,1.,1.), 1.)))
    let plane = new InfinitePlane(Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.), 1., Colour(1.,1.,1.), 1.)))

    let baseRectangle = new BaseRectangle(Point(0.,0.,0.), Point(0.,1.,0.), Point(1.,0.,0.))
    let baseDisc = new BaseDisc(Point(0.,0.,0.), 2.)
    let baseTriangle = new BaseTriangle(Point(0.,0.,0.), Point(0.,1.,0.), Point(1.,0.,0.))
    let baseSphere = new BaseSphere(Point(0.,0.,0.), 2.)
    let baseHollowCylinder = new BaseHollowCylinder(Point(0.,0.,0.), 2., 4.)


    //Tests that shapes are built correctly
    Assert.Equal(rectangle.bottomLeft, Point(0.,0.,0.), "rectangle.bottomLeft")
    Assert.Equal(rectangle.topLeft, Point(0.,1.,0.), "rectangle.topLeft")
    Assert.Equal(rectangle.bottomRight, Point(1.,0.,0.), "rectangle.bottomRight")

    Assert.Equal(disc.center, Point(0.,0.,0.), "disc.center")
    Assert.Equal(disc.radius, 2., "disc.radius")

    Assert.Equal(triangle.a, Point(0.,0.,0.), "triangle.a")
    Assert.Equal(triangle.b, Point(0.,1.,0.), "triangle.b")
    Assert.Equal(triangle.c, Point(1.,0.,0.), "triangle.c")

    Assert.Equal(sphere.origin, Point(0.,0.,0.), "sphere.origin")
    Assert.Equal(sphere.radius, 2., "sphere.radius")

    Assert.Equal(hollowCylinder.center, Point(0.,0.,0.), "hollowCylinder.center")
    Assert.Equal(hollowCylinder.radius, 2., "hollowCylinder.radius")
    Assert.Equal(hollowCylinder.height, 4., "hollowCylinder.height")

    Assert.Equal(solidCylinder.center, Point(0.,0.,0.), "solidCylinder.center")
    Assert.Equal(solidCylinder.radius, 2., "solidCylinder.radius")
    Assert.Equal(solidCylinder.height, 4., "solidCylinder.height")

    Assert.Equal(box.low, Point(0.,0.,0.), "box.low")
    Assert.Equal(box.low, Point(0.,0.,0.), "box.high")

    
    //Tests that BaseShapes are built correctly
    Assert.Equal(baseRectangle.bottomLeft, Point(0.,0.,0.), "baseRectangle.bottomLeft")
    Assert.Equal(baseRectangle.topLeft, Point(0.,1.,0.), "baseRectangle.topLeft")
    Assert.Equal(baseRectangle.bottomRight, Point(1.,0.,0.), "baseRectangle.bottomRight")

    Assert.Equal(baseDisc.center, Point(0.,0.,0.), "baseDisc.center")
    Assert.Equal(baseDisc.radius, 2., "baseDisc.radius")

    Assert.Equal(baseTriangle.a, Point(0.,0.,0.), "baseTriangle.a")
    Assert.Equal(baseTriangle.b, Point(0.,1.,0.), "baseTriangle.b")
    Assert.Equal(baseTriangle.c, Point(1.,0.,0.), "baseTriangle.c")

    Assert.Equal(baseSphere.origin, Point(0.,0.,0.), "baseSphere.origin")
    Assert.Equal(baseSphere.radius, 2., "baseSphere.radius")

    Assert.Equal(baseHollowCylinder.center, Point(0.,0.,0.), "baseHollowCylinder.center")
    Assert.Equal(baseHollowCylinder.radius, 2., "baseHollowCylinder.radius")
    Assert.Equal(baseHollowCylinder.height, 4., "baseHollowCylinder.height")

    //Tests for Bounding boxes being correctly returned with, getBoundingBox()
    let recBBox = rectangle.getBoundingBox ()
    Assert.Equal(recBBox.lowPoint.X, -0.000001, "test for rectangle Bounding box, lowPoint.X")
    Assert.Equal(recBBox.lowPoint.Y, -0.000001, "test for rectangle Bounding box, lowPoint.Y")
    Assert.Equal(recBBox.lowPoint.Z, -0.000001, "test for rectangle Bounding box, lowPoint.Z")
    Assert.Equal(recBBox.highPoint.X, 1.000001, "test for rectangle Bounding box, highPoint.X")
    Assert.Equal(recBBox.highPoint.Y, 1.000001, "test for rectangle Bounding box, highPoint.Y")
    Assert.Equal(recBBox.highPoint.Z, 0.000001, "test for rectangle Bounding box, highPoint.Z")

    let discBBox = disc.getBoundingBox ()
    Assert.Equal(discBBox.lowPoint.X, -2.000001, "test for disc Bounding box, lowPoint.X")
    Assert.Equal(discBBox.lowPoint.Y, -2.000001, "test for disc Bounding box, lowPoint.Y")
    Assert.Equal(discBBox.lowPoint.Z, -0.000001, "test for disc Bounding box, lowPoint.Z")
    Assert.Equal(discBBox.highPoint.X, 2.000001, "test for disc Bounding box, highPoint.X")
    Assert.Equal(discBBox.highPoint.Y, 2.000001, "test for disc Bounding box, highPoint.Y")
    Assert.Equal(discBBox.highPoint.Z, 0.000001, "test for disc Bounding box, highPoint.Z")

    let triBBox = triangle.getBoundingBox ()
    Assert.Equal(triBBox.lowPoint.X, -0.000001, "test for triangle Bounding box, lowPoint.X")
    Assert.Equal(triBBox.lowPoint.Y, -0.000001, "test for triangle Bounding box, lowPoint.Y")
    Assert.Equal(triBBox.lowPoint.Z, -0.000001, "test for triangle Bounding box, lowPoint.Z")
    Assert.Equal(triBBox.highPoint.X, 1.000001, "test for triangle Bounding box, highPoint.X")
    Assert.Equal(triBBox.highPoint.Y, 1.000001, "test for triangle Bounding box, highPoint.Y")
    Assert.Equal(triBBox.highPoint.Z, 0.000001, "test for triangle Bounding box, highPoint.Z")

    let sphereBBox = sphere.getBoundingBox ()
    Assert.Equal(sphereBBox.lowPoint.X, -2.000001, "test for sphere Bounding box, lowPoint.X")
    Assert.Equal(sphereBBox.lowPoint.Y, -2.000001, "test for sphere Bounding box, lowPoint.Y")
    Assert.Equal(sphereBBox.lowPoint.Z, -2.000001, "test for sphere Bounding box, lowPoint.Z")
    Assert.Equal(sphereBBox.highPoint.X, 2.000001, "test for sphere Bounding box, highPoint.X")
    Assert.Equal(sphereBBox.highPoint.Y, 2.000001, "test for sphere Bounding box, highPoint.Y")
    Assert.Equal(sphereBBox.highPoint.Z, 2.000001, "test for sphere Bounding box, highPoint.Z")

    let hollowBBox = hollowCylinder.getBoundingBox ()
    Assert.Equal(hollowBBox.lowPoint.X, -2.000001, "test for hollowCylinder Bounding box, lowPoint.X")
    Assert.Equal(hollowBBox.lowPoint.Y, -2.000001, "test for hollowCylinder Bounding box, lowPoint.Y")
    Assert.Equal(hollowBBox.lowPoint.Z, -2.000001, "test for hollowCylinder Bounding box, lowPoint.Z")
    Assert.Equal(hollowBBox.highPoint.X, 2.000001, "test for hollowCylinder Bounding box, highPoint.X")
    Assert.Equal(hollowBBox.highPoint.Y, 2.000001, "test for hollowCylinder Bounding box, highPoint.Y")
    Assert.Equal(hollowBBox.highPoint.Z, 2.000001, "test for hollowCylinder Bounding box, highPoint.Z")

    let solidBBox = solidCylinder.getBoundingBox ()
    Assert.Equal(solidBBox.lowPoint.X, -2.000001, "test for solidCylinder Bounding box, lowPoint.X")
    Assert.Equal(solidBBox.lowPoint.Y, -2.000001, "test for solidCylinder Bounding box, lowPoint.Y")
    Assert.Equal(solidBBox.lowPoint.Z, -2.000001, "test for solidCylinder Bounding box, lowPoint.Z")
    Assert.Equal(solidBBox.highPoint.X, 2.000001, "test for solidCylinder Bounding box, highPoint.X")
    Assert.Equal(solidBBox.highPoint.Y, 2.000001, "test for solidCylinder Bounding box, highPoint.Y")
    Assert.Equal(solidBBox.highPoint.Z, 2.000001, "test for solidCylinder Bounding box, highPoint.Z")

    let boxBBox = box.getBoundingBox ()
    Assert.Equal(boxBBox.lowPoint.X, -0.000001, "test for box Bounding box, lowPoint.X")
    Assert.Equal(boxBBox.lowPoint.Y, -0.000001, "test for box Bounding box, lowPoint.Y")
    Assert.Equal(boxBBox.lowPoint.Z, -0.000001, "test for box Bounding box, lowPoint.Z")
    Assert.Equal(boxBBox.highPoint.X, 1.000001, "test for box Bounding box, highPoint.X")
    Assert.Equal(boxBBox.highPoint.Y, 1.000001, "test for box Bounding box, highPoint.Y")
    Assert.Equal(boxBBox.highPoint.Z, 1.000001, "test for box Bounding box, highPoint.Z")


    //isInside function for shapes
    Assert.True(sphere.isInside (Point(0.,0.,0.)), "test isInside function, for point inside sphere")
    Assert.True(not(sphere.isInside (Point(7.,7.,7.))), "test isInside function, for point outside sphere")
    Assert.True(solidCylinder.isInside (Point(0.,0.,0.)), "test isInside function, for point inside solidCylinder")
    Assert.True(not(solidCylinder.isInside (Point(7.,7.,7.))), "test isInside function, for point outside solidCylinder")
    Assert.True(box.isInside (Point(0.5,0.5,0.5)), "test isInside function, for point inside box")
    Assert.True(not(box.isInside (Point(7.,7.,7.))), "test isInside function, for point outside box")

    //BBox Intersect test
    let bBox = BBox(Point(0.,0.,0.), Point(1.,1.,1.))
    let rayInside = Ray(Point(0.5,0.5,0.5), Vector(0.5,0.5,0.5).Normalise)
    let hitInside = bBox.intersect(rayInside)
    Assert.True(hitInside.IsSome, "test on BBox intersect, for point inside BBox")
    let rayOutside = Ray(Point(1.5,1.5,1.5), Vector(-0.5,-0.5,-0.5).Normalise)
    let hitOutside = bBox.intersect(rayOutside)
    Assert.True(hitOutside.IsSome, "test on BBox intersect, for point outside BBox")
    let rayOutsideMiss = Ray(Point(1.5,1.5,1.5), Vector(0.5,0.5,0.5).Normalise)
    let hitOutsideMiss = bBox.intersect(rayOutsideMiss)
    Assert.True(hitOutsideMiss.IsNone, "test on BBox not intersect, for point outside BBox")

    //Tests that hitFunctions act as expected
    let rayHit =  Ray(Point(0.25, 0.25, 0.25), Vector(0.5, 0.5, -0.5).Normalise)
    let rayMiss =  Ray(Point(6., 3., 3.), Vector(1., -0.5, -0.5).Normalise)
    //Hit
    Assert.True((rectangle.hitFunction rayHit).DidHit, "test on rectangle HitFunction, for Ray hitting")
    Assert.True((disc.hitFunction rayHit).DidHit, "test on disc HitFunction, for Ray hitting")
    Assert.True((triangle.hitFunction rayHit).DidHit, "test on triangle HitFunction, for Ray hitting")
    Assert.True((sphere.hitFunction rayHit).DidHit, "test on sphere HitFunction, for Ray hitting")
    Assert.True((hollowCylinder.hitFunction rayHit).DidHit, "test on hollowCylinder HitFunction, for Ray hitting")
    Assert.True((solidCylinder.hitFunction rayHit).DidHit, "test on solidCylinder HitFunction, for Ray hitting")
    Assert.True((box.hitFunction rayHit).DidHit, "test on box HitFunction, for Ray hitting")

    //Miss
    Assert.True(not (rectangle.hitFunction rayMiss).DidHit, "test on rectangle HitFunction, for Ray missing")
    Assert.True(not (disc.hitFunction rayMiss).DidHit, "test on disc HitFunction, for Ray missing")
    Assert.True(not (triangle.hitFunction rayMiss).DidHit, "test on triangle HitFunction, for Ray missing")
    Assert.True(not (sphere.hitFunction rayMiss).DidHit, "test on sphere HitFunction, for Ray missing")
    Assert.True(not (hollowCylinder.hitFunction rayMiss).DidHit, "test on hollowCylinder HitFunction, for Ray missing")
    Assert.True(not (solidCylinder.hitFunction rayMiss).DidHit, "test on solidCylinder HitFunction, for Ray missing")
    Assert.True(not (box.hitFunction rayMiss).DidHit, "test on box HitFunction, for Ray missing")
    

    //Tests that toShape functions in BaseShape works
    let toRectangle = baseRectangle.toShape (Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.), 1., Colour(1.,1.,1.), 1.))) :?> Rectangle
    let toDisc = baseDisc.toShape (Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.), 1., Colour(1.,1.,1.), 1.))) :?> Disc
    let toTriangle = baseTriangle.toShape (Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.), 1., Colour(1.,1.,1.), 1.))) :?> Triangle
    let toSphere = baseSphere.toShape (Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.), 1., Colour(1.,1.,1.), 1.))) :?> SphereShape
    let toHollowCylinder = baseHollowCylinder.toShape (Textures.mkMatTexture(MatteMaterial(Colour(1.,1.,1.), 1., Colour(1.,1.,1.), 1.))) :?> HollowCylinder

    Assert.Equal(toRectangle.bottomLeft, Point(0.,0.,0.), "rectangle.bottomLeft")
    Assert.Equal(rectangle.topLeft, Point(0.,1.,0.), "rectangle.topLeft")
    Assert.Equal(rectangle.bottomRight, Point(1.,0.,0.), "rectangle.bottomRight")

    Assert.Equal(toDisc.center, Point(0.,0.,0.), "disc.center")
    Assert.Equal(toDisc.radius, 2., "disc.radius")

    Assert.Equal(toTriangle.a, Point(0.,0.,0.), "triangle.a")
    Assert.Equal(toTriangle.b, Point(0.,1.,0.), "triangle.b")
    Assert.Equal(toTriangle.c, Point(1.,0.,0.), "triangle.c")

    Assert.Equal(toSphere.origin, Point(0.,0.,0.), "sphere.origin")
    Assert.Equal(toSphere.radius, 2., "sphere.radius")

    Assert.Equal(toHollowCylinder.center, Point(0.,0.,0.), "hollowCylinder.center")
    Assert.Equal(toHollowCylinder.radius, 2., "hollowCylinder.radius")
    Assert.Equal(toHollowCylinder.height, 4., "hollowCylinder.height")

    
    ////CSG TESTS////

    let csgUnion = CSG(sphere, box, Union)
    let csgIntersection = CSG(sphere, box, Intersection)
    let csgSubtraction = CSG(sphere, box, Subtraction)
    let csgGrouping = CSG(sphere, box, Grouping)

    //test for CSG isInside function
    let pointInside = Point(0.2,0.2,0.2)
    let pointInside2 = Point(-0.2,-0.2,-0.2)
    let pointOutside = Point(7.,7.,7.)

    Assert.True(csgUnion.isInside (pointInside), "test isInside function, for point inside CSG Union")
    Assert.True(not(csgUnion.isInside (pointOutside)), "test isInside function, for point outside CSG Union")
    Assert.True(csgIntersection.isInside (pointInside), "test isInside function, for point inside CSG Intersection")
    Assert.True(not(csgIntersection.isInside (pointOutside)), "test isInside function, for point outside CSG Intersection")
    Assert.True(csgSubtraction.isInside (pointInside2), "test isInside function, for point inside CSG Subtraction")
    Assert.True(not(csgSubtraction.isInside (pointOutside)), "test isInside function, for point outside CSG Subtraction")
    Assert.True(csgGrouping.isInside (pointInside), "test isInside function, for point inside CSG Union")
    Assert.True(not(csgGrouping.isInside (pointOutside)), "test isInside function, for point outside CSG Union")


    //Test for CSG getBoundingBox
    let unionBBox = csgUnion.getBoundingBox ()
    Assert.Equal(unionBBox.lowPoint.X, -2.000001, "test for CSG Union Bounding box, lowPoint.X")
    Assert.Equal(unionBBox.lowPoint.Y, -2.000001, "test for CSG Union Bounding box, lowPoint.Y")
    Assert.Equal(unionBBox.lowPoint.Z, -2.000001, "test for CSG Union Bounding box, lowPoint.Z")
    Assert.Equal(unionBBox.highPoint.X, 2.000001, "test for CSG Union Bounding box, highPoint.X")
    Assert.Equal(unionBBox.highPoint.Y, 2.000001, "test for CSG Union Bounding box, highPoint.Y")
    Assert.Equal(unionBBox.highPoint.Z, 2.000001, "test for CSG Union Bounding box, highPoint.Z")

    let intersectionBBox = csgIntersection.getBoundingBox ()
    Assert.Equal(intersectionBBox.lowPoint.X, -0.000001, "test for CSG Intersection Bounding box, lowPoint.X")
    Assert.Equal(intersectionBBox.lowPoint.Y, -0.000001, "test for CSG Intersection Bounding box, lowPoint.Y")
    Assert.Equal(intersectionBBox.lowPoint.Z, -0.000001, "test for CSG Intersection Bounding box, lowPoint.Z")
    Assert.Equal(intersectionBBox.highPoint.X, 1.000001, "test for CSG Intersection Bounding box, highPoint.X")
    Assert.Equal(intersectionBBox.highPoint.Y, 1.000001, "test for CSG Intersection Bounding box, highPoint.Y")
    Assert.Equal(intersectionBBox.highPoint.Z, 1.000001, "test for CSG Intersection Bounding box, highPoint.Z")

    let subtractionBBox = csgSubtraction.getBoundingBox ()
    Assert.Equal(subtractionBBox.lowPoint.X, -2.000001, "test for CSG Subtraction Bounding box, lowPoint.X")
    Assert.Equal(subtractionBBox.lowPoint.Y, -2.000001, "test for CSG Subtraction Bounding box, lowPoint.Y")
    Assert.Equal(subtractionBBox.lowPoint.Z, -2.000001, "test for CSG Subtraction Bounding box, lowPoint.Z")
    Assert.Equal(subtractionBBox.highPoint.X, 2.000001, "test for CSG Subtraction Bounding box, highPoint.X")
    Assert.Equal(subtractionBBox.highPoint.Y, 2.000001, "test for CSG Subtraction Bounding box, highPoint.Y")
    Assert.Equal(subtractionBBox.highPoint.Z, 2.000001, "test for CSG Subtraction Bounding box, highPoint.Z")

    let groupingBBox = csgGrouping.getBoundingBox ()
    Assert.Equal(groupingBBox.lowPoint.X, -2.000001, "test for CSG Grouping Bounding box, lowPoint.X")
    Assert.Equal(groupingBBox.lowPoint.Y, -2.000001, "test for CSG Grouping Bounding box, lowPoint.Y")
    Assert.Equal(groupingBBox.lowPoint.Z, -2.000001, "test for CSG Grouping Bounding box, lowPoint.Z")
    Assert.Equal(groupingBBox.highPoint.X, 2.000001, "test for CSG Grouping Bounding box, highPoint.X")
    Assert.Equal(groupingBBox.highPoint.Y, 2.000001, "test for CSG Grouping Bounding box, highPoint.Y")
    Assert.Equal(groupingBBox.highPoint.Z, 2.000001, "test for CSG Grouping Bounding box, highPoint.Z")

    //Tests that hitFunctions act as expected
    let rayHitcsg =  Ray(Point(0.25, 0.25, 0.25), Vector(0.5, 0.5, -0.5))
    let rayHitcsgSub =  Ray(Point(0.25, 0.25, 0.25), Vector(-0.5, -0.5, -0.5))
    let rayMisscsg =  Ray(Point(6., 3., 3.), Vector(1., -0.5, -0.5))
    //Hit
    Assert.True((csgUnion.hitFunction rayHitcsg).DidHit, "test on CSG Union HitFunction, for Ray hitting")
    Assert.True((csgIntersection.hitFunction rayHitcsg).DidHit, "test on CSG Intersection HitFunction, for Ray hitting")
    Assert.True((csgSubtraction.hitFunction rayHitcsgSub).DidHit, "test on CSG Subtraction HitFunction, for Ray hitting")
    Assert.True((csgGrouping.hitFunction rayHitcsg).DidHit, "test on CSG Grouping HitFunction, for Ray hitting")

    //Miss
    Assert.True(not (csgUnion.hitFunction rayMisscsg).DidHit, "test on CSG Union HitFunction, for Ray missing")
    Assert.True(not (csgIntersection.hitFunction rayMisscsg).DidHit, "test on CSG Intersection HitFunction, for Ray missing")
    Assert.True(not (csgSubtraction.hitFunction rayMisscsg).DidHit, "test on CSG Subtraction HitFunction, for Ray missing")
    Assert.True(not (csgGrouping.hitFunction rayMisscsg).DidHit, "test on CSG Grouping HitFunction, for Ray missing")


