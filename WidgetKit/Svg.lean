import WidgetKit.HtmlWidget
import WidgetKit.Json

open Lean.Widget.Jsx
open Lean Widget


private def Float.toInt (x : Float) : Int :=
  if x >= 0 then
    x.toUInt64.toNat
  else
    -((-x).toUInt64.toNat)

namespace Svg

structure Color where
  (r := 0.0)
  (g := 0.0)
  (b := 0.0)
deriving ToJson, FromJson

def Color.toRGB (c : Color) : String := s!"rgb({255*c.r}, {255*c.g}, {255*c.b})"

structure Point where
  (x y : Float)
deriving Inhabited, ToJson, FromJson


structure Frame where
  (min : Point)
  (xSize : Float)
  (width height : Nat)
deriving ToJson, FromJson

def Frame.ySize (frame : Frame) : Float := frame.height.toFloat * (frame.xSize / frame.width.toFloat)

def Frame.max (frame : Frame) : Point := ⟨frame.min.x + frame.xSize, frame.min.y + frame.ySize⟩

def Point.toPixels (p : Point) (frame : Frame) : Int × Int :=
  let xmin := frame.min.x
  let xmax := frame.max.x
  let ymin := frame.min.y
  let ymax := frame.max.y
  let px := frame.width.toFloat  * (p.x - xmin) / (xmax - xmin)
  let py := frame.height.toFloat * (p.y - ymax) / (ymin - ymax)
  (px.toInt, py.toInt)


inductive Size where
| pixels   (size : Nat)   : Size
| absolute (size : Float) : Size
deriving ToJson, FromJson

def Size.toPixels (s : Size) (frame : Frame) : Nat :=
  match s with
  | .pixels   x => x
  | .absolute x => x * (frame.width.toFloat / frame.xSize) |>.toUInt64.toNat

-- inductive PolylineType

inductive Shape where
| line     (src trg : Point)
| circle   (center : Point) (radius : Size)
| polyline (points : Array Point) -- (type : PolylineType)
| polygon  (points : Array Point)
deriving ToJson, FromJson

def Shape.toHtmlData (frame : Frame) : Shape → String × Array (String × Json)
| .line src trg => 
  let (x1,y1) := src.toPixels frame
  let (x2,y2) := trg.toPixels frame
  ("line", #[("x1", x1), ("y1", y1), ("x2", x2), ("y2", y2)])
| .circle center radius => 
  let (cx,cy) := center.toPixels frame
  let r := radius.toPixels frame
  ("circle", #[("cx", cx), ("cy", cy), ("r", r)])
| .polyline points => 
  let pts := points 
      |>.map (λ p => let (x,y) := p.toPixels frame; s!"{x},{y}")
      |>.foldl (init := "") (λ s p => s ++ " " ++ p)
  ("polyline", #[("points", pts)])
| .polygon points => 
  let pts := points 
      |>.map (λ p => let (x,y) := p.toPixels frame; s!"{x},{y}")
      |>.foldl (init := "") (λ s p => s ++ " " ++ p)
  ("polygon", #[("fillRule", "nonzero"), ("points", pts)])

 
structure Element where
  shape : Shape
  strokeColor := (none : Option Color)
  strokeWidth := (none : Option Size)
  fillColor   := (none : Option Color)
  clickData   := (none : Option Json)
deriving ToJson, FromJson


def Element.toHtml (frame : Frame) (e : Element) : Html := Id.run do
  let mut (tag, args) := e.shape.toHtmlData frame

  if let .some color ← e.strokeColor then
    args := args.push ("stroke", color.toRGB)
  if let .some width ← e.strokeWidth then
    args := args.push ("strokeWidth", width.toPixels frame)
  if let .some color ← e.fillColor then
    args := args.push ("fill", color.toRGB)
  if let .some clickData ← e.clickData then
    args := args.push ("click", clickData)

  return .element tag args #[]


end Svg

-- def mkIdToIdx (elements : Array Svg.Element) : HashMap String (Fin elements.size) := 
--   let idToIdx := (elements
--     |>.mapIdx (λ idx el => (idx,el))) -- zip with index
--     |>.filterMap (λ (idx,el) => el.id.map (λ id => (id,idx))) -- keep only elements with specified id
--     |>.toList 
--     |> HashMap.ofList
--   idToIdx

structure Svg where
  elements : Array Svg.Element
  frame    : Svg.Frame
  -- idToIdx := mkIdToIdx elements 

def Svg.toHtml (svg : Svg) : Html := 
  .element "svg" 
           #[("xmlns", "http://www.w3.org/2000/svg"), 
             ("version", "1.1"), 
             ("width", svg.frame.width), 
             ("height", svg.frame.height)] 
           (svg.elements.map λ e => e.toHtml svg.frame)
  
instance : GetElem Svg Nat Svg.Element (λ svg idx => idx < svg.elements.size) where
  getElem svg i h := svg.elements[i]



section Example

  open Svg

  private def frame : Frame where
    min    := ⟨-2,-2⟩
    xSize  := 4
    width  := 400
    height := 400


  private def svg : Svg := 
    { elements := 
        #[{ shape := .line ⟨0,0⟩ ⟨1,0⟩, strokeWidth := some (.pixels 2), strokeColor := some ⟨1,0,0⟩},
          { shape := .line ⟨1,0⟩ ⟨0,1⟩, strokeWidth := some (.pixels 2), strokeColor := some ⟨0,1,0⟩},
          { shape := .line ⟨0,1⟩ ⟨0,0⟩, strokeWidth := some (.pixels 2), strokeColor := some ⟨0,0,1⟩},
          { shape := .circle ⟨0,0⟩ (.absolute 0.1) , strokeWidth := some (.pixels 2), strokeColor := some ⟨0,0,0⟩, fillColor := some ⟨0,1,1⟩},
          { shape := .circle ⟨1,0⟩ (.absolute 0.1) , strokeWidth := some (.pixels 2), strokeColor := some ⟨0,0,0⟩, fillColor := some ⟨1,0,1⟩},
          { shape := .circle ⟨0,1⟩ (.absolute 0.1) , strokeWidth := some (.pixels 2), strokeColor := some ⟨0,0,0⟩, fillColor := some ⟨1,1,0⟩}
               ],
      frame := frame }

  #eval toJson svg.toHtml

  #html svg.toHtml

end Example
