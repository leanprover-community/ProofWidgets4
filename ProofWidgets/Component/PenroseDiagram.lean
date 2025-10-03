module

public meta import ProofWidgets.Component.Basic
public meta import ProofWidgets.Data.Html
public meta import Std.Data.HashMap

public meta section

namespace ProofWidgets.Penrose
open Lean Server Std

structure DiagramProps where
  embeds      : Array (String × Html)
  dsl         : String
  sty         : String
  sub         : String
  /-- Maximum number of optimization steps to take before showing the diagram.
  Optimization may converge earlier, before taking this many steps. -/
  maxOptSteps : Nat := 500
  deriving Inhabited, RpcEncodable

/-- Displays the given diagram using [Penrose](https://penrose.cs.cmu.edu/).
The website contains explanations of how to write domain (`dsl`), style (`sty`),
and substance (`sub`) programs.

The diagram may also contain embedded HTML trees which are specified in `embeds`.
Each embed is HTML together with the name of an object `x` in the substance program.
The object `x` can be of any type but *must* be assigned an `x.textBox : Rectangle` field
in the style program.
This rectangle will be replaced with the HTML tree.
Its dimensions will be overridden in the style program
to match those of the HTML node.

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
def Diagram : Component DiagramProps where
  javascript := include_str ".." / ".." / ".lake" / "build" / "js" / "penroseDisplay.js"

/-! # `DiagramBuilderM` -/

structure DiagramState where
  /-- The Penrose substance program.
  Note that `embeds` are added lazily at the end. -/
  sub : String := ""
  /-- Components to display as labels in the diagram,
  stored in the map as name ↦ (type, html). -/
  embeds : Std.HashMap String (String × Html) := ∅

/-- A monad to easily build Penrose diagrams in. -/
abbrev DiagramBuilderM := StateT DiagramState MetaM

namespace DiagramBuilderM

open scoped Jsx in
/-- Assemble the diagram using the provided domain and style programs.

`none` is returned iff nothing was added to the diagram. -/
def buildDiagram (dsl sty : String) (maxOptSteps : Nat := 500) : DiagramBuilderM (Option Html) := do
  let st ← get
  if st.sub == "" && st.embeds.isEmpty then
    return none
  let mut sub := "AutoLabel All\n"
  let mut embedHtmls := #[]
  for (n, (tp, h)) in st.embeds.toArray do
    sub := sub ++ s!"{tp} {n}\n"
    embedHtmls := embedHtmls.push (n, h)
  -- Note: order matters here, embed variables are declared first.
  sub := sub ++ st.sub
  return <Diagram
    embeds={embedHtmls}
    dsl={dsl} sty={sty} sub={sub}
    maxOptSteps={maxOptSteps} />

/-- Add an object `nm` of Penrose type `tp`,
labelled by `h`, to the substance program. -/
def addEmbed (nm : String) (tp : String) (h : Html) : DiagramBuilderM Unit := do
  modify fun st => { st with embeds := st.embeds.insert nm (tp, h) }

open scoped Jsx in
/-- Add an object of Penrose type `tp`,
corresponding to (and labelled by) the expression `e`,
to the substance program.
Return its Penrose name. -/
def addExpr (tp : String) (e : Expr) : DiagramBuilderM String := do
  let nm ← toString <$> Lean.Meta.ppExpr e
  let h := <InteractiveCode fmt={← Widget.ppExprTagged e} />
  addEmbed nm tp h
  return nm

/-- Add an instruction `i` to the substance program. -/
def addInstruction (i : String) : DiagramBuilderM Unit := do
  modify fun st => { st with sub := st.sub ++ s!"{i}\n" }

def run (x : DiagramBuilderM α) : MetaM α :=
  x.run' {}

end DiagramBuilderM
end Penrose

/-- Abbreviation for backwards-compatibility. -/
abbrev PenroseDiagramProps := Penrose.DiagramProps
/-- Abbreviation for backwards-compatibility. -/
abbrev PenroseDiagram := Penrose.Diagram

end ProofWidgets
