import ProofWidgets.Component.Basic
import ProofWidgets.Data.Html

namespace ProofWidgets
open Lean Server Jsx

structure DigraphDisplay.Vertex where
  /-- Identifier for this vertex. Must be unique. -/
  id : String
  /-- The label is drawn at the vertex position.
  This must be an SVG element.
  Use `<foreignObject>` to draw non-SVG elements. -/
  label : Html :=
    <circle
      r={5}
      className="dim"
      fill="var(--vscode-editor-background)"
      strokeWidth={.num 1.5}
      stroke="var(--vscode-editor-foreground)"
    />
  /-- Details are shown below the graph display
  after the vertex label has been clicked. -/
  details? : Option Html := none
  deriving Inhabited, RpcEncodable

structure DigraphDisplay.Edge where
  /-- Source vertex. Must match the `id` of one of the vertices. -/
  source : String
  /-- Target vertex. Must match the `id` of one of the vertices. -/
  target : String
  /-- Details are shown below the graph display
  after the edge has been clicked. -/
  details? : Option Html := none
  deriving Inhabited, RpcEncodable

structure DigraphDisplay.Props where
  vertices : Array Vertex
  edges : Array Edge
  -- TODO: Allow configuring `d3-force` parameters
  -- TODO: Allow hiding the vertex/edge details element
  deriving Inhabited, RpcEncodable

@[widget_module]
def DigraphDisplay : Component DigraphDisplay.Props where
  javascript := include_str ".." / ".." / ".lake" / "build" / "js" / "d3Graph.js"

end ProofWidgets
