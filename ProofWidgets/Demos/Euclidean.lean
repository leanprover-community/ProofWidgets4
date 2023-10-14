/-
 Copyright (c) 2023 Vladimir Sedlacek. All rights reserved.
 Released under Apache 2.0 license as described in the file LICENSE.
 Authors: Vladimir Sedlacek
 -/

import Lean.Data.HashMap
import Lean.Elab.Tactic
import ProofWidgets.Component.PenroseDiagram
import ProofWidgets.Component.HtmlDisplay
import ProofWidgets.Component.Panel.Basic

open Lean Meta Server
open ProofWidgets

/-! # Minimal definitions of synthetic geometric primitives, inspired by https://github.com/ah1112/synthetic_euclid_4 -/

class IncidenceGeometry where
  Point : Type u₁
  Line : Type u₂

  between : Point → Point → Point → Prop -- implies colinearity
  onLine : Point → Line → Prop
  ne_23_of_between : ∀ {a b c : Point}, between a b c → b ≠ c
  line_unique_of_pts : ∀ {a b : Point}, ∀ {L M : Line}, a ≠ b → onLine a L → onLine b L → onLine a M → onLine b M → L = M
  onLine_2_of_between : ∀ {a b c : Point}, ∀ {L : Line}, between a b c → onLine a L → onLine c L → onLine b L

variable [i : IncidenceGeometry]
open IncidenceGeometry

/-! # Metaprogramming utilities to break down expressions -/

/-- If `e == onLine a L` return `some (a, L)`, otherwise `none`. -/
def isOnLinePred? (e : Expr) : Option (Expr × Expr) := do
  let some (_, a, L) := e.app3? ``onLine | none
  return (a, L)

/-- If `e == between a b c` return `some (a, b, c)`, otherwise `none`. -/
def isBetweenPred? (e : Expr) : Option (Expr × Expr × Expr) := do
  let some (_, a, b, c) := e.app4? ``between | none
  return (a, b, c)

/-- Expressions to display as labels in a diagram. -/
abbrev ExprEmbeds := Array (String × Expr)

open scoped Jsx in
def mkEuclideanDiag (sub : String) (embeds : ExprEmbeds) : MetaM Html := do
  let embeds ← embeds.mapM fun (s, h) =>
      return (s, <InteractiveCode fmt={← Widget.ppExprTagged h} />)
  return (
    <PenroseDiagram
      embeds={embeds}
      dsl={include_str ".."/".."/"widget"/"penrose"/"euclidean.dsl"}
      sty={include_str ".."/".."/"widget"/"penrose"/"euclidean.sty"}
      sub={sub} />)

def isEuclideanGoal? (hyps : Array LocalDecl) : MetaM (Option Html) := do
  let mut sub := "AutoLabel All\n"
  let mut sets : HashMap String Expr := .empty
  for assm in hyps do
    let tp ← instantiateMVars assm.type
    if let some (a, L) := isOnLinePred? tp then
      let sa ← toString <$> Lean.Meta.ppExpr a
      let sL ← toString <$> Lean.Meta.ppExpr L
      let (sets', ca) := sets.insert' sa a
      let (sets', cL) := sets'.insert' sL L
      sets := sets'
      if !ca then
        sub := sub ++ s!"Point {sa}\n"
      if !cL then
        sub := sub ++ s!"Line {sL}\n"
      sub := sub ++ s!"On({sa}, {sL})\n"
    if let some (a, b, c) := isBetweenPred? tp then
      let sa ← toString <$> Lean.Meta.ppExpr a
      let sb ← toString <$> Lean.Meta.ppExpr b
      let sc ← toString <$> Lean.Meta.ppExpr c
      let (sets', ca) := sets.insert' sa a
      let (sets', cb) := sets'.insert' sb b
      let (sets', cc) := sets'.insert' sc c
      sets := sets'
      if !ca then
        sub := sub ++ s!"Point {sa}\n"
      if !cb then
        sub := sub ++ s!"Point {sb}\n"
      if !cc then
        sub := sub ++ s!"Point {sc}\n"
      sub := sub ++ s!"Between({sa}, {sb}, {sc})\n"
  if sets.isEmpty then return none
  some <$> mkEuclideanDiag sub sets.toArray

/-! # RPC handler and client-side code for the widget -/

structure Params where
  ci : WithRpcRef Elab.ContextInfo
  mvar : MVarId
  locs : Array SubExpr.GoalLocation

#mkrpcenc Params

structure Response where
  html? : Option Html

#mkrpcenc Response

open scoped Jsx in
@[server_rpc_method]
def getEuclideanGoal (ps : Params) : RequestM (RequestTask Response) := do
  RequestM.asTask do
    let html? ← ps.ci.val.runMetaM {} <| ps.mvar.withContext do
      let locs : Array LocalDecl ← ps.locs.filterMapM fun
        | .hyp fv => return some (← fv.getDecl)
        | .hypType fv _ => return some (← fv.getDecl)
        | _ => return none
      isEuclideanGoal? locs
    return { html? }

@[widget_module]
def EuclideanDisplayPanel : Component PanelWidgetProps where
  javascript := s!"
    import * as React from 'react';
    import \{ DynamicComponent, useAsync, RpcContext } from '@leanprover/infoview';
    const e = React.createElement;

    function findGoalForLocation(goals, loc) \{
      for (const g of goals) \{
        if (g.mvarId === loc.mvarId) return g
      }
      throw new Error(`Could not find goal for location $\{JSON.stringify(loc)}`)
    }

    export default function(props) \{
      const rs = React.useContext(RpcContext)
      const st = useAsync(async () => \{
        if (props.selectedLocations.length === 0)
          return \{ html: \{ text: 'Select hypotheses with shift-click.' } }
        const g = findGoalForLocation(props.goals, props.selectedLocations[0])
        const locs = props.selectedLocations.map(loc => loc.loc)
        return rs.call('getEuclideanGoal', \{ ci: g.ctx, mvar: g.mvarId, locs })
      }, [props.selectedLocations, props.goals, rs])
      let inner = undefined
      if (st.state === 'resolved')
        inner = e(DynamicComponent, \{
          pos: props.pos,
          hash: '{hash HtmlDisplay.javascript}',
          props: \{
            pos: props.pos,
            html: st.value.html ?? \{ text: 'No Euclidean goal.' }
          }
        }, null);
      else
        inner = JSON.stringify(st)
      return e('details', \{open: true}, [
        e('summary', \{className: 'mv2 pointer'}, 'Euclidean diagram'),
        inner
      ])
    }
  "

/-! # Example usage -/

example {a b c : Point} {L M : Line} (Babc : between a b c) (aL : onLine a L) (bM : onLine b M)
    (cL : onLine c L) (cM : onLine c M) : L = M := by
  with_panel_widgets [EuclideanDisplayPanel]
      -- Place your cursor here.
    have bc := ne_23_of_between Babc
    have bL := onLine_2_of_between Babc aL cL
    exact line_unique_of_pts bc bL cL bM cM
