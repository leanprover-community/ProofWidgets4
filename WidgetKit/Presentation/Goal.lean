import Lean.Elab.Tactic
import Lean.Meta.ExprLens

import WidgetKit.Component.Basic
import WidgetKit.Presentation.Expr -- Needed for RPC calls in PresentSelectionPanel

namespace WidgetKit
open Lean Server

structure LocationsToExprParams where
  locations : Array (WithRpcRef Elab.ContextInfo × SubExpr.GoalsLocation)

#mkrpcenc LocationsToExprParams

structure LocationsToExprResponse where
  exprs : Array (WithRpcRef ExprWithCtx)

#mkrpcenc LocationsToExprResponse

@[server_rpc_method]
def locationsToExpr (args : LocationsToExprParams) :
    RequestM (RequestTask LocationsToExprResponse) :=
  RequestM.asTask do
    let mut exprs := #[]
    for ⟨⟨ci⟩, loc⟩ in args.locations do
      exprs := exprs.push ⟨← ci.runMetaM {} <| go loc.mvarId loc.loc⟩
    return { exprs }
where
  go (mvarId : MVarId) : SubExpr.GoalLocation → MetaM ExprWithCtx
  | .hyp fv =>
    mvarId.withContext <|
      ExprWithCtx.save (mkFVar fv)
  | .hypType fv pos => mvarId.withContext do
    let tp ← Meta.inferType (mkFVar fv)
    Meta.viewSubexpr (visit := fun _ => ExprWithCtx.save) pos tp
  | .hypValue fv pos => mvarId.withContext do
    let some val ← fv.getValue?
      | throwError "fvar {mkFVar fv} is not a let-binding"
    Meta.viewSubexpr (visit := fun _ => ExprWithCtx.save) pos val
  | .target pos => mvarId.withContext do
    let tp ← Meta.inferType (mkMVar mvarId)
    Meta.viewSubexpr (visit := fun _ => ExprWithCtx.save) pos tp

@[widget_module]
def PresentSelectionPanel : Component PanelWidgetProps where
  javascript := include_str ".." / ".." / "widget" / "dist" / "presentSelection.js"

/-- Displays any expressions selected in the goal state using available `WidgetKit.ExprPresenter`s.
-/
syntax (name := withSelectionDisplayTacStx) "withSelectionDisplay " tacticSeq : tactic

open Elab Tactic in
@[tactic withSelectionDisplayTacStx]
def withSelectionDisplay : Tactic
  | stx@`(tactic| withSelectionDisplay $seq) => do
    savePanelWidgetInfo stx ``PresentSelectionPanel (pure .null)
    evalTacticSeq seq
  | _ => throwUnsupportedSyntax

-- TODO: replace the panel with an extensible one which invokes an arbitrary
-- `@[goals_presenter]` where `GoalsPresenter ≈ PanelWidgetProps → MetaM EncodableHtml`
-- with access to the entire goal state in order to display it in a "global" way.
-- In short, implement custom tactic state displays.

end WidgetKit
