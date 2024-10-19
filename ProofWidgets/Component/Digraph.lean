import ProofWidgets.Component.Basic
import ProofWidgets.Data.Html

namespace ProofWidgets
open Lean Server

structure DigraphDisplay.Vertex where
  id : String
  label? : Option Html := none
  details? : Option Html := none
  deriving Inhabited, RpcEncodable

structure DigraphDisplay.Edge where
  source : String
  target : String
  label? : Option Html := none
  details? : Option Html := none
  deriving Inhabited, RpcEncodable

structure DigraphDisplay.Props where
  vertices : Array Vertex
  edges : Array Edge
  deriving Inhabited, RpcEncodable

@[widget_module]
def DigraphDisplay : Component DigraphDisplay.Props where
  javascript := include_str ".." / ".." / ".lake" / "build" / "js" / "d3Graph.js"

end ProofWidgets
