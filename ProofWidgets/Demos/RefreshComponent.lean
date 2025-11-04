import ProofWidgets.Component.RefreshComponent
import ProofWidgets.Component.HtmlDisplay
import ProofWidgets.Component.OfRpcMethod
import ProofWidgets.Component.Panel.SelectionPanel
import ProofWidgets.Demos.RefreshComponent.CancelToken

/-!
This file showcases the `RefreshComponent` using some basic examples
-/

open Lean.Widget ProofWidgets RefreshComponent Jsx Lean Server

/-! Example 1: Counting up to 10 -/

partial def countToTen : CoreM Html := do
  mkRefreshComponentM (.text "Let's count to ten!!!") (count 0)
where
  count (n : Nat) : RefreshT CoreM Unit := refreshM do
    IO.sleep 1000
    dbg_trace "counted {n}"
    Core.checkSystem "count to ten"
    if n = 10 then
      return .last <| .text s!"Wie niet weg is, is gezien, ik kom!!!"
    else
      return .cont (.text s!"{n + 1}!!!") (count (n + 1))

-- If you close and reopen the infoview, the counting continues as if it was open the whole time.
#html countToTen

-- Here, we duplicate the widget using the same underlying computation
#html (do let x ← countToTen; return <span>{x}{x}</span>)

-- Here, we duplicate the widget and duplicate the underlying computation
#html (return <span>{← countToTen}{← countToTen}</span>)


/-! Example 2: print the selected expressions one by one in an infinite loop -/

@[server_rpc_method]
partial def cycleSelections (props : PanelWidgetProps) : RequestM (RequestTask Html) :=
  RequestM.asTask do
    let cancelTk ← IO.CancelToken.new
    if let some oldTk ← globalCancelToken.swap cancelTk then
      oldTk.set
    let some goal := props.goals[0]? | return .text "there are no goals"
    mkGoalRefreshComponent goal cancelTk (.text "loading...") do refreshM do
      let args ← props.selectedLocations.mapM (·.saveExprWithCtx)
      if h : args.size ≠ 0 then
        have : NeZero args.size := ⟨h⟩
        loop args 0
      else
        return .last <| .text "please select some expression"
where
  loop (args : Array ExprWithCtx) (i : Fin args.size) : MetaM (RefreshResult MetaM) := do
    Core.checkSystem "cycleSelections"
    return .cont (<InteractiveCode fmt={← args[i].runMetaM Widget.ppExprTagged}/>) do refreshM do
    IO.sleep 1000
    Core.checkSystem "cycleSelections"
    have : NeZero args.size := ⟨by cases i; grind⟩
    loop args (i + 1)

@[widget_module]
def cylceComponent : Component PanelWidgetProps :=
  mk_rpc_widget% cycleSelections

-- show_panel_widgets [cylceComponent, cylceComponent]
show_panel_widgets [cylceComponent]
example : 1 + 2 + 3 = 6 ^ 1 := by -- place your cursor here and select some expressions
  rfl
-- If you activate the widget multiple times, then thanks to the global cancel token ref,
-- All but one of the widget computations will say `This component was cancelled`.


/-! Example 4: compute the fibonacci numbers, and show the results incrementally. -/

def slowFibo (n : Nat) : Nat :=
  match n with
  | 0 => 0
  | 1 => 1
  | n + 2 => slowFibo n + slowFibo (n + 1)
