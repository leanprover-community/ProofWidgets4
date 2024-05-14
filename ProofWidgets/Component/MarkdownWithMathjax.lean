import ProofWidgets.Component.Basic

open Lean

namespace ProofWidgets

structure MarkdownWithMathjaxProps where
  markdown : String
deriving Inhabited, Server.RpcEncodable

@[widget_module]
def MarkdownWithMathjax : Component MarkdownWithMathjaxProps where
  javascript := include_str ".." / ".." / ".lake" / "build" / "js" / "markdownWithMathjax.js"
