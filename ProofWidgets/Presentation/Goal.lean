import Lean.Elab.Tactic
import Lean.Meta.ExprLens

import ProofWidgets.Component.Basic
import ProofWidgets.Presentation.Expr -- Needed for RPC calls in PresentSelectionPanel

namespace ProofWidgets
open Lean Server

structure GoalsLocationsToExprsParams where
  locations : Array (WithRpcRef Elab.ContextInfo × SubExpr.GoalsLocation)

#mkrpcenc GoalsLocationsToExprsParams

structure GoalsLocationsToExprsResponse where
  exprs : Array (WithRpcRef ExprWithCtx)

#mkrpcenc GoalsLocationsToExprsResponse

/-- Compute expressions corresponding to the given `GoalsLocation`s. -/
@[server_rpc_method]
def goalsLocationsToExprs (args : GoalsLocationsToExprsParams) :
    RequestM (RequestTask GoalsLocationsToExprsResponse) :=
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
  javascript := include_str ".." / ".." / "build" / "js" / "presentSelection.js"

/-- Displays any expressions selected in the goal state using registered `ProofWidgets.ExprPresenter`s.
Expressions can be selected using shift-click -/
syntax (name := withSelectionDisplayTacStx) "withSelectionDisplay " tacticSeq : tactic

open Elab Tactic in
@[tactic withSelectionDisplayTacStx]
def withSelectionDisplay : Tactic
  | stx@`(tactic| withSelectionDisplay $seq) => do
    savePanelWidgetInfo stx ``PresentSelectionPanel (pure .null)
    evalTacticSeq seq
  | _ => throwUnsupportedSyntax

-- TODO: replace the panel with an extensible one which invokes an arbitrary
-- `@[goals_presenter]` where `GoalsPresenter ≈ PanelWidgetProps → MetaM Html`
-- with access to the entire goal state in order to display it in a "global" way.
-- In short, implement custom tactic state displays.

end ProofWidgets
