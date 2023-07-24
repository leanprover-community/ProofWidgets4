type Targettable
type Point <: Targettable
type Line <: Targettable

predicate Between(Point a, Point b, Point c)
predicate On(Point a, Line L)

constructor MkLine(Point p, Point q) -> Line
