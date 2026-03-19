module

public meta import Lean.Meta.ExprLens
public meta import ProofWidgets.Component.Panel.Basic
public meta import ProofWidgets.Presentation.Expr

public meta section -- Needed for RPC calls in SelectionPanel

open ProofWidgets in
/-- Save the expression corresponding to a goals location. -/
def Lean.SubExpr.GoalsLocation.saveExprWithCtx (loc : GoalsLocation) : MetaM ExprWithCtx :=
  let mvarId := loc.mvarId
  mvarId.withContext do
    match loc.loc with
    | .hyp fvarId => ExprWithCtx.save (.fvar fvarId)
    | .hypType fvarId pos =>
      Meta.viewSubexpr (fun _ => ExprWithCtx.save) pos (← instantiateMVars (← fvarId.getType))
    | .hypValue fvarId pos =>
      let some val ← fvarId.getValue? | throwError "fvar {Expr.fvar fvarId} is not a let-binding"
      Meta.viewSubexpr (fun _ => ExprWithCtx.save) pos (← instantiateMVars val)
    | .target pos =>
      Meta.viewSubexpr (fun _ => ExprWithCtx.save) pos (← instantiateMVars (← mvarId.getType))

namespace ProofWidgets
open Lean Server

structure GoalsLocationsToExprsParams where
  locations : Array (WithRpcRef Elab.ContextInfo × SubExpr.GoalsLocation)
  deriving RpcEncodable

structure GoalsLocationsToExprsResponse where
  exprs : Array (WithRpcRef ExprWithCtx)
  deriving RpcEncodable

/-- Compute expressions corresponding to the given `GoalsLocation`s. -/
@[server_rpc_method]
def goalsLocationsToExprs (args : GoalsLocationsToExprsParams) :
    RequestM (RequestTask GoalsLocationsToExprsResponse) :=
  RequestM.asTask do
    let mut exprs := #[]
    for ⟨ref, loc⟩ in args.locations do
      let ci := ref.val
      exprs := exprs.push (← WithRpcRef.mk (← ci.runMetaM {} loc.saveExprWithCtx))
    return { exprs }

/-- Display a list of all expressions selected in the goal state, with a choice of which `Expr`
presenter should be used to display each of those expressions.

Expressions can be selected using shift-click. -/
@[widget_module]
def SelectionPanel : Component PanelWidgetProps where
  javascript := include_str ".." / ".." / ".." / ".lake" / "build" / "js" / "presentSelection.js"

end ProofWidgets
