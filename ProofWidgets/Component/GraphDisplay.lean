import ProofWidgets.Component.Basic
import ProofWidgets.Data.Html

namespace ProofWidgets.GraphDisplay
open Lean Server Jsx

/-- A themed `<circle>` SVG element, with optional extra attributes. -/
def mkCircle (attrs : Array (String × Json) := #[]) : Html :=
  <circle
    r={5}
    fill="var(--vscode-editor-background)"
    stroke="var(--vscode-editor-foreground)"
    strokeWidth={.num 1.5}
    {...attrs}
  />

structure Vertex where
  /-- Identifier for this vertex. Must be unique. -/
  id : String
  /-- The label is drawn at the vertex position.
  This must be an SVG element.
  Use `<foreignObject>` to draw non-SVG elements. -/
  label : Html := mkCircle
  /-- Radius of a circle bounding this vertex.
  Used to place incident edge endpoints. -/
  radius : Float := 5
  /-- Details are shown below the graph display
  after the vertex label has been clicked.
  See also `Props.showDetails`. -/
  details? : Option Html := none
  deriving Inhabited, RpcEncodable

structure Edge where
  /-- Source vertex. Must match the `id` of one of the vertices. -/
  source : String
  /-- Target vertex. Must match the `id` of one of the vertices. -/
  target : String
  /-- Extra attributes to set on the SVG `<line>` element representing this edge.
  See also `Props.defaultEdgeAttrs`. -/
  attrs : Array (String × Json) := #[]
  /-- If present, the label is shown over the edge midpoint.
  This must be an SVG element.
  Use `<foreignObject>` to draw non-SVG elements. -/
  label? : Option Html := none
  /-- Details are shown below the graph display
  after the edge has been clicked.
  See also `Props.showDetails`. -/
  details? : Option Html := none
  deriving Inhabited, RpcEncodable

structure ForceCenterParams where
  x? : Option Float := none
  y? : Option Float := none
  strength? : Option Float := none
  deriving Inhabited, FromJson, ToJson

structure ForceCollideParams where
  radius? : Option Float := none
  strength? : Option Float := none
  iterations? : Option Nat := none
  deriving Inhabited, FromJson, ToJson

structure ForceLinkParams where
  distance? : Option Float := none
  strength? : Option Float := none
  iterations? : Option Nat := none
  deriving Inhabited, FromJson, ToJson

structure ForceManyBodyParams where
  strength? : Option Float := none
  theta? : Option Float := none
  distanceMin? : Option Float := none
  distanceMax? : Option Float := none
  deriving Inhabited, FromJson, ToJson

structure ForceXParams where
  x? : Option Float := none
  strength? : Option Float := none
  deriving Inhabited, FromJson, ToJson

structure ForceYParams where
  y? : Option Float := none
  strength? : Option Float := none
  deriving Inhabited, FromJson, ToJson

structure ForceRadialParams where
  radius : Float
  x? : Option Float := none
  y? : Option Float := none
  strength? : Option Float := none
  deriving Inhabited, FromJson, ToJson

/-- Settings for the simulation of forces on vertices.
See https://d3js.org/d3-force. -/
inductive ForceParams where
  | center : ForceCenterParams → ForceParams
  | collide : ForceCollideParams → ForceParams
  | link : ForceLinkParams → ForceParams
  | manyBody : ForceManyBodyParams → ForceParams
  | x : ForceXParams → ForceParams
  | y : ForceYParams → ForceParams
  | radial : ForceRadialParams → ForceParams
  deriving Inhabited, FromJson, ToJson

structure Props where
  vertices : Array Vertex
  /-- At most one edge may exist between any two vertices.
  Self-loops are allowed,
  but (TODO) are currently not rendered well. -/
  edges : Array Edge
  /-- Attributes to set by default on `<line>` elements representing edges. -/
  defaultEdgeAttrs : Array (String × Json) := #[
    ("fill", "var(--vscode-editor-foreground)"),
    ("stroke", "var(--vscode-editor-foreground)"),
    ("strokeWidth", 2),
    ("markerEnd", "url(#arrow)")
  ]
  /-- Which forces to apply to the vertices.
  Most force parameters are optional, using default values if not specified. -/
  forces : Array ForceParams := #[ .link {}, .manyBody {}, .x {}, .y {} ]
  /-- Whether to show a details box below the graph. -/
  showDetails : Bool := false
  deriving Inhabited, RpcEncodable

end GraphDisplay

/-- Display a graph with an interactive force simulation. -/
@[widget_module]
def GraphDisplay : Component GraphDisplay.Props where
  javascript := include_str ".." / ".." / ".lake" / "build" / "js" / "d3Graph.js"

end ProofWidgets
