type Object
type Point <: Object
type Line <: Object
type Circle <: Object

predicate Between(Point a, Point b, Point c)
predicate OnLine(Point a, Line L)
predicate OnCircle(Point a, Circle C)
predicate InCircle(Point a, Circle C)
predicate CenterCircle(Point a, Circle C)
predicate CirclesInter(Circle C, Circle D)

predicate Emphasize(Object o)
