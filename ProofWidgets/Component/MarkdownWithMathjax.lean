import ProofWidgets.Component.Basic

open Lean

namespace ProofWidgets

structure MarkdownWithMathjax.Props where
  markdown : String
deriving Inhabited, Server.RpcEncodable

/--
This component renders a markdown string with mathjax.

Example usage:
```lean
<MarkdownWithMathjax markdown={"$a + b = c$"} />
```

Use `$` for inline math and
```
```math
a + b = c
```
```

for displayed math.
-/
@[widget_module]
def MarkdownWithMathjax : Component MarkdownWithMathjax.Props where
  javascript := include_str ".." / ".." / ".lake" / "build" / "js" / "markdownWithMathjax.js"
