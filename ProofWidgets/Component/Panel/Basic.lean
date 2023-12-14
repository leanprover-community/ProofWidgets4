import ProofWidgets.Compat
import ProofWidgets.Component.Basic
import Lean.Elab.Tactic

namespace ProofWidgets
open Lean Elab Tactic

/-- Describes the props passed to a panel widget.
The TypeScript version is exported as `PanelWidgetProps` from `@leanprover/infoview`. -/
structure PanelWidgetProps : Type where
  /-- Cursor position in the file at which the widget is being displayed. -/
  pos : DocumentPosition
  /-- The current tactic-mode goals. -/
  goals : Array Widget.InteractiveGoal
  /-- The current term-mode goal, if any. -/
  termGoal? : Option Widget.InteractiveTermGoal
  /-- Locations currently selected in the goal state. -/
  selectedLocations : Array SubExpr.GoalsLocation
  deriving Server.RpcEncodable

/-- In the infoview, an **info block** is a top-level collapsible block
corresponding to a given location in a Lean file
(e.g. with the header `▼ Basic.lean:12:34`).

A **panel widget** is a component which can appear
as a panel inside an info block in the infoview.
For example, a tactic state display.

Note that to be a good citizen which doesn't mess up the infoview layout,
a panel widget should be a block element,
and should provide some way to collapse it,
usually by using `<details>` as the top-level tag. -/
abbrev PanelWidget LProps := Component LProps PanelWidgetProps

/-- Display the selected panel widgets in the nested tactic script. For example,
assuming we have written a `GeometryDisplay` component,
```lean
by with_panel_widgets [GeometryDisplay]
  simp
  rfl
```
will show the geometry display alongside the usual tactic state throughout the proof.
-/
syntax (name := withPanelWidgetsTacticStx)
  "with_panel_widgets" "[" Widget.widgetInstanceSpec,+ "]" tacticSeq : tactic

@[tactic withPanelWidgetsTacticStx]
def withPanelWidgets : Tactic
  | stx@`(tactic| with_panel_widgets [ $specs,* ] $seq) => do
    specs.getElems.forM fun specStx => do
      let spec ← Widget.elabWidgetInstanceSpec specStx
      let wi ← Widget.evalWidgetInstance spec
      Widget.savePanelWidgetInfo wi.javascriptHash wi.props stx
    evalTacticSeq seq
  | _ => throwUnsupportedSyntax

/-- Save the data of a panel widget which will be displayed whenever the text cursor is on `stx`.
The field `c.javascript` must come from a widget module annotated with `@[widget_module]`.

This is a more strongly-typed version of `Lean.Widget.savePanelWidgetInfo`. -/
def savePanelWidgetInfo' [Monad m] [MonadEnv m] [MonadError m] [MonadInfoTree m]
    [Server.RpcEncodable Props]
    (c : PanelWidget Props) (props : Props) (stx : Syntax) : m Unit := do
  Widget.savePanelWidgetInfo c.javascriptHash (Server.rpcEncode props) stx

end ProofWidgets
