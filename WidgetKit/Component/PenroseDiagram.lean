import WidgetKit.Component.Basic
import WidgetKit.Data.Html

namespace WidgetKit
open Lean Server

structure PenroseDiagramProps where
  embeds : Array (String × EncodableHtml)
  dsl    : String
  sty    : String
  sub    : String
  deriving Inhabited, RpcEncodable

/-- Displays the given diagram using [Penrose](https://penrose.cs.cmu.edu/). The website contains
explanations of how to write domain (`dsl`), style (`sty`), and substance (`sub`) programs.

The diagram may also contain embedded HTML trees which are specified in `embeds`. Each embed is HTML
together with the name of an object `x` in the substance program which *must* be assigned
a `x.textBox : Rectangle` field in the style program. This rectangle will be replaced with the HTML
tree.
-/
@[widget_module]
def PenroseDiagram : Component PenroseDiagramProps where
  javascript := include_str ".." / ".." / "widget" / "dist" / "penroseDisplay.js"

end WidgetKit
