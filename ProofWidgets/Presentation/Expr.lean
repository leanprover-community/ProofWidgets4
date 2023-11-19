import ProofWidgets.Data.Html

namespace ProofWidgets
open Lean Server

/-- An `Expr` presenter is similar to a delaborator but outputs HTML trees instead of syntax, and
the output HTML can contain elements which interact with the original `Expr` in some way. We call
interactive outputs with a reference to the original input *presentations*. -/
structure ExprPresenter where
  /-- A user-friendly name for this presenter. For example, "LaTeX". -/
  userName : String
  /-- Whether the output HTML has inline (think something which fits in the space normally occupied
  by an `Expr`, e.g. LaTeX) or block (think large diagram which needs dedicated space) layout. -/
  layoutKind : LayoutKind := .block
  present : Expr → MetaM Html

initialize exprPresenters : TagAttribute ←
  registerTagAttribute `expr_presenter
    "Register an Expr presenter. It must have the type `ProofWidgets.ExprPresenter`."
    (validate := fun nm => do
      let const ← getConstInfo nm
      if !const.type.isConstOf ``ExprPresenter then
        throwError m!"type mismatch, expected {mkConst ``ExprPresenter} but got {const.type}"
      return ())

private unsafe def evalExprPresenterUnsafe (env : Environment) (opts : Options)
    (constName : Name) : Except String ExprPresenter :=
  env.evalConstCheck ExprPresenter opts ``ExprPresenter constName

@[implemented_by evalExprPresenterUnsafe]
opaque evalExprPresenter (env : Environment) (opts : Options) (constName : Name) :
  Except String ExprPresenter

structure GetExprPresentationsParams where
  expr : WithRpcRef ExprWithCtx

#mkrpcenc GetExprPresentationsParams

structure ExprPresentationData where
  name : Name
  userName : String
  html : Html
  deriving RpcEncodable

structure ExprPresentations where
  presentations : Array ExprPresentationData
  deriving RpcEncodable

@[server_rpc_method]
def getExprPresentations : GetExprPresentationsParams → RequestM (RequestTask ExprPresentations)
  | ⟨⟨expr⟩⟩ => RequestM.asTask do
    let mut presentations : Array ExprPresentationData := #[]
    let env := expr.ci.env
    for nm in exprPresenters.ext.getState env do
      presentations ← addPresenterIfApplicable expr nm presentations
    -- FIXME: The fact that we need this loop suggests that TagAttribute is not the right way
    -- to implement a persistent, iterable set of names.
    for modNm in env.allImportedModuleNames do
      let some modIdx := env.getModuleIdx? modNm
        | throw <| RequestError.internalError s!"unknown module {modNm}"
      for nm in exprPresenters.ext.getModuleEntries env modIdx do
        presentations ← addPresenterIfApplicable expr nm presentations
    return { presentations }
where addPresenterIfApplicable (expr : ExprWithCtx) (nm : Name) (ps : Array ExprPresentationData) :
    RequestM (Array ExprPresentationData) :=
  match evalExprPresenter expr.ci.env expr.ci.options nm with
  | .ok p =>
    try
      let html ← expr.runMetaM fun e => do
        let e ← Lean.instantiateMVars e
        p.present e
      return ps.push ⟨nm, p.userName, html⟩
    catch _ =>
      return ps
  | .error e =>
    throw <| RequestError.internalError s!"Failed to evaluate Expr presenter '{nm}': {e}"

structure GetExprPresentationParams where
  expr : WithRpcRef ExprWithCtx
  /-- Name of the presenter to use. -/
  name : Name

#mkrpcenc GetExprPresentationParams

@[server_rpc_method]
def getExprPresentation : GetExprPresentationParams → RequestM (RequestTask Html)
  | { expr := ⟨expr⟩, name } => RequestM.asTask do
    let ci := expr.ci
    if !exprPresenters.hasTag ci.env name then
      throw <| RequestError.invalidParams s!"The constant '{name}' is not an Expr presenter."
    match evalExprPresenter ci.env ci.options name with
    | .ok p =>
      expr.runMetaM p.present
    | .error e =>
      throw <| RequestError.internalError s!"Failed to evaluate Expr presenter '{name}': {e}"

-- https://leanprover.zulipchat.com/#narrow/stream/287929-mathlib4/topic/deprecation.20warning.20from.20ProofWidgets
attribute [deprecated] getExprPresentation

structure ExprPresentationProps where
  expr : WithRpcRef ExprWithCtx

#mkrpcenc ExprPresentationProps

/-- This component shows a selection of all known and applicable `ProofWidgets.ExprPresenter`s which
are used to render the expression when selected. The one with highest precedence (TODO) is shown by
default. -/
@[widget_module]
def ExprPresentation : Component ExprPresentationProps where
  javascript := include_str ".." / ".." / ".lake" / "build" / "js" / "exprPresentation.js"

end ProofWidgets
