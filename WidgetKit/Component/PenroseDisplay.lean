import WidgetKit.Component.Basic
import WidgetKit.Data.Html

namespace WidgetKit
open Lean Server

structure DiagramData where
  embeds : Array (String × EncodableHtml)
  dsl    : String
  sty    : String
  sub    : String
  deriving Inhabited, RpcEncodable

@[widget_module]
def PenroseDisplay.module : String :=
  include_str ".." / ".." / "widget" / "dist" / "penroseDisplay.js"

/-- Displays the given diagram using penrose.ink. -/
def PenroseDisplay : Component DiagramData where
  javascript := PenroseDisplay.module

end WidgetKit
