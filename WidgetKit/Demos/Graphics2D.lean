import WidgetKit.HtmlWidget
import WidgetKit.Json

open Lean.Widget.Jsx
open Lean Widget

structure Color where
  (r := 0.0)
  (g := 0.0)
  (b := 0.0)


def Color.toRGB (c : Color) : String := s!"rgb({255*c.r}, {255*c.g}, {255*c.b})"


structure Point where
  (x y : Float)
deriving Inhabited

structure Frame where
  (min : Point)
  (xSize : Float)
  (width height : Nat)

def Frame.ySize (frame : Frame) : Float := frame.height.toFloat * (frame.xSize / frame.width.toFloat)

def Frame.max (frame : Frame) : Point := ⟨frame.min.x + frame.xSize, frame.min.y + frame.ySize⟩


def Point.toPixels (p : Point) (frame : Frame) : Nat × Nat :=
  let xmin := frame.min.x
  let xmax := frame.max.x
  let ymin := frame.min.y
  let ymax := frame.max.y
  let px := frame.width.toFloat  * (p.x - xmin) / (xmax - xmin)
  let py := frame.height.toFloat * (p.y - ymax) / (ymin - ymax)
  (px.toUInt64.toNat, py.toUInt64.toNat)


inductive Size where
| zero : Size
| pixels   (size : Nat)   : Size
| absolute (size : Float) : Size

def Size.toPixels (s : Size) (frame : Frame) : Nat :=
  match s with
  | .zero => 0
  | .pixels   x => x
  | .absolute x => x * (frame.width.toFloat / frame.xSize) |>.toUInt64.toNat


structure Edge where
  (src trg : Point)
  (width : Size)
  (color : Color)

def Edge.toSvgHtml (edge : Edge) (frame : Frame) : Html := 
  let (x1,y1) := edge.src.toPixels frame
  let (x2,y2) := edge.trg.toPixels frame
  let strokeWidth := edge.width.toPixels frame;
  <line x1={x1} y1={y1} x2={x2} y2={y2} stroke={edge.color.toRGB} strokeWidth={strokeWidth} /> 


structure Circle where
  (center : Point)
  (radius : Size)
  (color : Color)

def Circle.toSvgHtml (circle : Circle) (frame : Frame) : Html :=
  let (cx,cy) := circle.center.toPixels frame
  let r := circle.radius.toPixels frame;
  <circle cx={cx} cy={cy} r={r} fill={circle.color.toRGB} />


structure Polygon where
  (points : Array Point)
  (color : Color)

def Polygon.toSvgHtml (polygon : Polygon) (frame : Frame) : Html :=
  let pixelPoint (p : Point) : String := 
    let (x,y) := p.toPixels frame
    s!"{x},{y}"
  let points : String := polygon.points[1:].foldl (init := pixelPoint polygon.points[0]!) 
    (λ s p => s ++ " " ++ pixelPoint p);

  <polygon fillRule="nonzero" fill={polygon.color.toRGB} points={points} />

inductive SvgShape where
| line     (src trg : Point)
| circle   (center : Point) (radius : Size)
| polyline (points : Array Point)
| polygon  (points : Array Point)

/- def SvgShape.to : SvgShape → String × Array (String)
| .line src trg width => sorry
| .circle center radius => sorry
| .polyline points width 
 -/
 
structure SvgElement where
  shape : SvgShape
  strokeColor : Color
  strokeWidth : Size
  id : String
  

-- structure Triangle where
--   (a b c : Point)
--   (color : Color)

structure GeometryData where
  edges     := (#[] : Array Edge)
  circles   := (#[] : Array Circle)
  polygons  := (#[] : Array Polygon)
  -- triangles : Array Triangle

def GeometryData.toSvgHtml (data : GeometryData) (frame : Frame) : Html := Id.run do

  let items := (data.edges.map   λ x => x.toSvgHtml frame) |>.append
               (data.circles.map λ x => x.toSvgHtml frame)

  return .element "svg" #[("xmlns", "http://www.w3.org/2000/svg"), ("version", "1.1"), ("width", frame.width), ("height", frame.height)] items



section Example

private def frame : Frame where
  min    := ⟨-1,-1⟩
  xSize  := 2
  width  := 400
  height := 400


private def data : GeometryData where
  edges   := #[{ src := ⟨0, 0⟩, trg := ⟨0, 1⟩, width := .pixels 2, color := { r := 1, g := 0, b := 0 }},
               { src := ⟨0, 1⟩, trg := ⟨1, 0⟩, width := .pixels 2, color := { r := 0, g := 1, b := 0 }},
               { src := ⟨1, 0⟩, trg := ⟨0, 0⟩, width := .pixels 2, color := { r := 0, g := 0, b := 1 }}]
  circles := #[{ center := ⟨0,0⟩, radius := .pixels 5, color := ⟨1,1,0⟩ },
               { center := ⟨1,0⟩, radius := .absolute 0.1, color := ⟨1,0,1⟩ },
               { center := ⟨0,1⟩, radius := .pixels 5, color := ⟨0,1,1⟩ }]
  
-- #eval toJson (data.toSvgHtml frame)
#html data.toSvgHtml frame

end Example
