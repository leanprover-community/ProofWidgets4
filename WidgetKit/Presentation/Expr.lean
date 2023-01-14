import WidgetKit.Data.Html

namespace WidgetKit
open Lean Server

/-- An `Expr` presenter is similar to a delaborator but outputs HTML trees instead of syntax, and
the output HTML can contain elements which interact with the original `Expr` in some way. We call
interactive outputs with a reference to the original input *presentations*. -/
structure ExprPresenter where
  /-- A user-friendly name for this presenter. For example, "LaTeX". -/
  userName : String
  /- TODO: there is a general problem of writing env extensions which store an extendable list of
  functions to run on `Expr`s, but not all of which are applicable (actually, most are not) to any
  single `Expr`. Invoking them in sequence is O(n); we should better use sth like `DiscrTree`.
  Registering new entries would need to extend the DiscrTree, perhaps like
  `registerSelf : DiscrTree ExprPresenter → DiscrTree ExprPresenter`.
  Dispatching on just one constant like e.g. delaborators (`app.MyType.myCtr`) is not sufficient
  because one entry may apply to multiple expressions of a given form which could be represented
  as a schematic with mvars, say `@ofNat ? 0 ?`. -/
  /-- Should quickly determine if the `Expr` is within this presenter's domain of applicability.
  For example it could check for a constant like the `` `name `` in ``@[delab `name]``. -/
  isApplicable : Expr → MetaM Bool
  /-- *Must* return `some _` or throw when `isApplicable` is `true`. -/
  present : Expr → MetaM (Option EncodableHtml)

initialize exprPresenters : TagAttribute ←
  registerTagAttribute `expr_presenter
    "Register an Expr presenter. It must have the type `WidgetKit.ExprPresenter`."
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

structure ApplicableExprPresentersParams where
  expr : WithRpcRef ExprWithCtx

#mkrpcenc ApplicableExprPresentersParams

structure ExprPresenterId where
  name : Name
  userName : String
  deriving FromJson, ToJson

structure ApplicableExprPresenters where
  presenters : Array ExprPresenterId
  deriving FromJson, ToJson

@[server_rpc_method]
def applicableExprPresenters : ApplicableExprPresentersParams →
    RequestM (RequestTask ApplicableExprPresenters)
  | ⟨⟨expr⟩⟩ => RequestM.asTask do
    let mut presenters : Array ExprPresenterId := #[]
    let ci := expr.ci
    for nm in exprPresenters.ext.getState expr.ci.env do
      match evalExprPresenter ci.env ci.options nm with
      | .ok p =>
        if ← expr.runMetaM p.isApplicable then
          presenters := presenters.push ⟨nm, p.userName⟩
      | .error e =>
        throw <| RequestError.internalError s!"Failed to evaluate Expr presenter '{nm}': {e}"
    return { presenters }

structure GetExprPresentationParams extends ApplicableExprPresentersParams where
  /-- Name of the presenter to use. -/
  name : Name

#mkrpcenc GetExprPresentationParams

@[server_rpc_method]
def getExprPresentation : GetExprPresentationParams →
    RequestM (RequestTask EncodableHtml)
  | { expr := ⟨expr⟩, name } => RequestM.asTask do
    let ci := expr.ci
    if !exprPresenters.hasTag ci.env name then
      throw <| RequestError.invalidParams s!"The constant '{name}' is not an Expr presenter."
    match evalExprPresenter ci.env ci.options name with
    | .ok p =>
      let some ret ← expr.runMetaM p.present
        | throw <| RequestError.internalError <|
          s!"Got none from {name}.present e, expected some _ because {name}.isApplicable e " ++
          s!"returned true, where e := {expr.expr}"
      return ret
    | .error e =>
      throw <| RequestError.internalError s!"Failed to evaluate Expr presenter '{name}': {e}"

structure ExprPresentationProps where
  expr : WithRpcRef ExprWithCtx

#mkrpcenc ExprPresentationProps

/-- This component shows a selection of all known and applicable `WidgetKit.ExprPresenter`s which
are used to render the expression when selected. By default `WidgetKit.InteractiveExpr` is shown. -/
@[widget_module]
def ExprPresentation : Component ExprPresentationProps where
  javascript := include_str ".." / ".." / "widget" / "dist" / "exprPresentation.js"

end WidgetKit
