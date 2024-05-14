import ProofWidgets.Component.Basic
import ProofWidgets.Component.HtmlDisplay

open Lean
namespace ProofWidgets

structure Node where
  id : String
  html : Html
deriving Inhabited, Server.RpcEncodable

structure InfoGraphProps where
  nodes : Array Node
  dot : String
  defaultHtml : Html
deriving Inhabited, Server.RpcEncodable

@[widget_module]
def InfoGraph : Component InfoGraphProps where
  javascript := include_str ".." / ".." / ".lake" / "build" / "js" / "infoGraph.js"
