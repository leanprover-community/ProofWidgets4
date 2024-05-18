import Lean.Server.Rpc.Basic

import ProofWidgets.Component.Basic

namespace ProofWidgets

open Lean Server

structure LatexProps where
  content : String
  deriving Server.RpcEncodable

@[widget_module]
def Latex : Component LatexProps where
  javascript := include_str ".." / ".." / ".lake" / "build" / "js" / "latexToSvg.js"

end ProofWidgets
