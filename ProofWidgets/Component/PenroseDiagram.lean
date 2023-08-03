import ProofWidgets.Component.Basic
import ProofWidgets.Data.Html

namespace ProofWidgets
open Lean Server

structure PenroseDiagramProps where
  embeds : Array (String × Html)
  dsl    : String
  sty    : String
  sub    : String
  deriving Inhabited

#mkrpcenc PenroseDiagramProps

/-- Displays the given diagram using [Penrose](https://penrose.cs.cmu.edu/).
The website contains explanations of how to write domain (`dsl`), style (`sty`),
and substance (`sub`) programs.

The diagram may also contain embedded HTML trees which are specified in `embeds`.
Each embed is HTML together with the name of an object `x` in the substance program.
The objhect `x` can be of any type but *must* be assigned an `x.textBox : Rectangle` field
in the style program. This rectangle will be replaced with the HTML tree.

The following additional constants are prepended to the style program:
```penrose
theme {
  color foreground
  color tooltipBackground
  color tooltipForeground
  color tooltipBorder
}
```
and can be accessed as, for example, `theme.foreground` in the provided `sty` in order to match
the editor theme. -/
@[widget_module]
def PenroseDiagram : Component PenroseDiagramProps where
  javascript := include_str ".." / ".." / "build" / "js" / "penroseDisplay.js"

end ProofWidgets
