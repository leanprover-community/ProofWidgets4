type Targettable
type Point <: Targettable
type Line <: Targettable

predicate Collinear(Point a, Point b, Point c)
predicate On(Point a, Line L)

constructor MkLine(Point p, Point q) -> Line
