import ProofWidgets.Component.Basic
import ProofWidgets.Component.HtmlDisplay

open Lean
namespace ProofWidgets

structure InfoGraph.Node where
  id : String
  html : Html
deriving Inhabited, Server.RpcEncodable

structure InfoGraph.Props where
  nodes : Array Node
  dot : String
  defaultHtml : Html
deriving Inhabited, Server.RpcEncodable

/--
This component renders an interactive graphviz graph.

Users must provide three fields:

- `nodes : Array InfoGraph.Node`
- `dot : String`
- `defaultHtml : Html`

Here `InfoGraph.Node` is a structure with two fields:

- `id : String`
- `html : Html`

The `nodes` specify the nodes of the graph, `dot` is the contents of a dotfile used
to render the graph itself and `defaultHtml` is the default html which will be rendered
in the information section.

Example usage:
`<InfoGraph nodes={nodes} dot={dot} defaultHtml={defaultHtml} />`

This will display the graph specified in the string `dot`.
The component expects that the nodes defined in `dot` all have an `id`
which matches the id field in exactly one of the elements of `nodes`.
When clicking on the node with a given `id`, the html in that particular
node will then be displayed.

Clicking outside of any node will dislpay the default html (which also appears by default).
-/
@[widget_module]
def InfoGraph : Component InfoGraph.Props where
  javascript := include_str ".." / ".." / ".lake" / "build" / "js" / "infoGraph.js"
