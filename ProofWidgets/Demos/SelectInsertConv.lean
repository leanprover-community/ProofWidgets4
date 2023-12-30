/-
Copyright (c) 2023 Patrick Massot. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Robin Böhne, Wojciech Nawrocki, Patrick Massot
-/
import Lean.Meta.ExprLens
import Std.Lean.Position
import Std.Data.Json
import ProofWidgets.Data.Html
import ProofWidgets.Component.OfRpcMethod
import ProofWidgets.Component.MakeEditLink
import ProofWidgets.Component.Panel.Basic

open Lean Meta Server
open ProofWidgets

/-! # The conv? example

This demo defines a `conv?` tactic that displays a widget for point-and-click `conv` insertion.
Whenever the user selects a subter, of the goal in the tactic state,
the widget will display a button that, upon being clicked, replaces the `conv?` call
with a `conv => enter [...]` call zooming in on the selected subterm. -/

private structure SolveReturn where
  expr : Expr
  val? : Option String
  listRest : List Nat

private def solveLevel (expr : Expr) (path : List Nat) : MetaM SolveReturn := match expr with
  | Expr.app _ _ => do
    let mut descExp := expr
    let mut count := 0
    let mut explicitList := []

    -- we go through the application until we reach the end, counting how many explicit arguments
    -- it has and noting whether they are explicit or implicit
    while descExp.isApp do
      if (←Lean.Meta.inferType descExp.appFn!).bindingInfo!.isExplicit then
        explicitList := true::explicitList
        count := count + 1
      else
        explicitList := false::explicitList
      descExp := descExp.appFn!

    -- we get the correct `enter` command by subtracting the number of `true`s in our list
    let mut mutablePath := path
    let mut length := count
    explicitList := List.reverse explicitList
    while !mutablePath.isEmpty && mutablePath.head! == 0 do
      if explicitList.head! == true then
        count := count - 1
      explicitList := explicitList.tail!
      mutablePath := mutablePath.tail!

    let mut nextExp := expr
    while length > count do
      nextExp := nextExp.appFn!
      length := length - 1
    nextExp := nextExp.appArg!

    let pathRest := if mutablePath.isEmpty then [] else mutablePath.tail!

    return { expr := nextExp, val? := toString count , listRest := pathRest }

  | Expr.lam n _ b _ => do
    let name := match n with
      | Name.str _ s => s
      | _ => panic! "no name found"
    return { expr := b, val? := name, listRest := path.tail! }

  | Expr.forallE n _ b _ => do
    let name := match n with
      | Name.str _ s => s
      | _ => panic! "no name found"
    return { expr := b, val? := name, listRest := path.tail! }

  | Expr.mdata _ b => do
    match b with
      | Expr.mdata _ _ => return { expr := b, val? := none, listRest := path }
      | _ => return { expr := b.appFn!.appArg!, val? := none, listRest := path.tail!.tail! }

  | _ => do
    return {
      expr := ←(Lean.Core.viewSubexpr path.head! expr)
      val? := toString (path.head! + 1)
      listRest := path.tail!
    }

open Lean Syntax in
def insertEnter (locations : Array Lean.SubExpr.GoalsLocation) (goalType : Expr) : MetaM String := do
  let some pos := locations[0]? | throwError "You must select something."
  let ⟨_, .target subexprPos⟩ := pos | throwError "You must select something in the goal."
  let mut list := (SubExpr.Pos.toArray subexprPos).toList
    let mut expr := goalType
  let mut retList := []
  -- generate list of commands for `enter`
  while !list.isEmpty do
    let res ← solveLevel expr list
    expr := res.expr
    retList := match res.val? with
      | none => retList
      | some val => val::retList
    list := res.listRest

  -- build `enter [...]` string
  retList := List.reverse retList
  let mut enterval := "conv => enter " ++ toString retList
  if enterval.contains '0' then enterval := "Error: Not a valid conv target"
  if retList.isEmpty then enterval := ""
  return enterval

def findGoalForLocation (goals : Array Widget.InteractiveGoal) (loc : SubExpr.GoalsLocation) :
    Option Widget.InteractiveGoal :=
  goals.find? (·.mvarId == loc.mvarId)

structure ConvSelectionPanelProps extends PanelWidgetProps where
  /-- The range in the source document where the `conv` command will be inserted. -/
  replaceRange : Lsp.Range
  deriving RpcEncodable

open scoped Jsx in
@[server_rpc_method]
def ConvSelectionPanel.rpc (props : ConvSelectionPanelProps) : RequestM (RequestTask Html) :=
  RequestM.asTask do
    let doc ← RequestM.readDoc
    let inner : Html ← (do
      if props.selectedLocations.isEmpty then
        return <span>Use shift-click to select one sub-expression in the goal that you want to zoom on.</span>
      let some selectedLoc := props.selectedLocations[0]? | unreachable!

      let some g := findGoalForLocation props.goals selectedLoc
        | throw $ .invalidParams
            s!"could not find goal for location {toJson selectedLoc}"
      g.ctx.val.runMetaM {} do
        let md ← g.mvarId.getDecl
        let lctx := md.lctx |>.sanitizeNames.run' {options := (← getOptions)}
        Meta.withLCtx lctx md.localInstances do
          let newCode ← insertEnter props.selectedLocations md.type
          return .ofComponent
            MakeEditLink
            (.ofReplaceRange doc.meta props.replaceRange newCode)
            #[ .text newCode ])
    return <details «open»={true}>
        <summary className="mv2 pointer">Conv 🔍</summary>
        <div className="ml1">{inner}</div>
      </details>

@[widget_module]
def ConvSelectionPanel : Component ConvSelectionPanelProps :=
  mk_rpc_widget% ConvSelectionPanel.rpc

open scoped Json in
elab stx:"conv?" : tactic => do
  let some replaceRange := (← getFileMap).rangeOfStx? stx | return
  Widget.savePanelWidgetInfo ConvSelectionPanel.javascriptHash
    (pure $ json% { replaceRange: $(replaceRange) }) stx

-- Like `sorry` but avoids a warning for demonstration purposes.
axiom test_sorry {α} : α

example (a : Nat) : a + a - a + a = a := by
  conv?
  -- Put your cursor on the next line
  all_goals exact test_sorry
