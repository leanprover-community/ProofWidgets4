import ProofWidgets.Compat
import ProofWidgets.Component.Basic
import Lean.Elab.Tactic
import Lean.Widget.Commands
import Batteries.Tactic.OpenPrivate

namespace ProofWidgets
open Lean Elab Tactic

/-- In the infoview, an **info block** is a top-level collapsible block corresponding to a given
location in a Lean file (e.g. with the header `▼ Basic.lean:12:34`).

A **panel widget** is a component which can appear as a panel inside an info block in the infoview.
For example, a tactic state display.
The type `PanelWidgetProps` represents the props passed to a panel widget.
The TypeScript version is exported as `PanelWidgetProps` from `@leanprover/infoview`.

Note that to be a good citizen which doesn't mess up the infoview layout,
a panel widget should be a block element,
and should provide some way to collapse it,
for example by using `<details>` as the top-level tag. -/
structure PanelWidgetProps : Type where
  /-- Cursor position in the file at which the widget is being displayed. -/
  pos : Lsp.Position
  /-- The current tactic-mode goals. -/
  goals : Array Widget.InteractiveGoal
  /-- The current term-mode goal, if any. -/
  termGoal? : Option Widget.InteractiveTermGoal
  /-- Locations currently selected in the goal state. -/
  selectedLocations : Array SubExpr.GoalsLocation
  deriving Server.RpcEncodable

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

open private Widget.elabWidgetInstanceSpec from Lean.Widget.Commands

@[tactic withPanelWidgetsTacticStx]
def withPanelWidgets : Tactic
  | stx@`(tactic| with_panel_widgets [ $specs,* ] $seq) => do
    specs.getElems.forM fun specStx => do
      let spec ← Widget.elabWidgetInstanceSpec specStx
      let wi ← Widget.evalWidgetInstance spec
      Widget.savePanelWidgetInfo wi.javascriptHash wi.props stx
    evalTacticSeq seq
  | _ => throwUnsupportedSyntax

end ProofWidgets
