import Lean.Data.HashMap
import Lean.Elab.Tactic
import ProofWidgets.Component.PenroseDiagram
import ProofWidgets.Component.HtmlDisplay
import ProofWidgets.Component.Panel

open Lean Meta Server
open ProofWidgets

/-! # Minimal definiton of sets copied from Mathlib -/

/-- Notation type class for the subset relation `⊆`. -/
class HasSubset (α : Type u) where
  /-- Subset relation: `a ⊆ b`  -/
  Subset : α → α → Prop
export HasSubset (Subset)

/-- Subset relation: `a ⊆ b`  -/
infix:50 " ⊆ " => HasSubset.Subset

def Set (α : Type u) := α → Prop

namespace Set

/-- Membership in a set -/
protected def Mem (a : α) (s : Set α) : Prop :=
  s a

instance : Membership α (Set α) :=
  ⟨Set.Mem⟩

theorem ext {a b : Set α} (h : ∀ (x : α), x ∈ a ↔ x ∈ b) : a = b :=
  funext (fun x ↦ propext (h x))

protected def Subset (s₁ s₂ : Set α) :=
  ∀ ⦃a⦄, a ∈ s₁ → a ∈ s₂

/-- Porting note: we introduce `≤` before `⊆` to help the unifier when applying lattice theorems
to subset hypotheses. -/
instance : LE (Set α) :=
  ⟨Set.Subset⟩

instance : HasSubset (Set α) :=
  ⟨(· ≤ ·)⟩

end Set

/-! # Metaprogramming utilities to break down set expressions -/

/-- If `e == S ⊆ T` return `some (S, T)`, otherwise `none`. -/
def isSubsetPred? (e : Expr) : Option (Expr × Expr) := do
  let some (_, _, S, T) := e.app4? ``Subset | none
  return (S, T)

/-- Expressions to display as labels in a diagram. -/
abbrev ExprEmbeds := Array (String × Expr)

open scoped Jsx in
def mkSetDiag (sub : String) (embeds : ExprEmbeds) : MetaM Html := do
  let embeds ← embeds.mapM fun (s, h) =>
      return (s, Html.ofTHtml <InteractiveCode fmt={← Widget.ppExprTagged h} />)
  return Html.ofTHtml
    <PenroseDiagram
      embeds={embeds}
      dsl={include_str ".."/".."/"widget"/"penrose"/"setTheory.dsl"}
      sty={include_str ".."/".."/"widget"/"penrose"/"venn.sty"}
      sub={sub} />

def isSetGoal? (hyps : Array LocalDecl) : MetaM (Option Html) := do
  let mut sub := "AutoLabel All\n"
  let mut sets : HashMap String Expr := .empty
  for assm in hyps do
    let tp ← instantiateMVars assm.type
    if let some (S, T) := isSubsetPred? tp then
      let sS ← toString <$> Lean.Meta.ppExpr S
      let sT ← toString <$> Lean.Meta.ppExpr T
      let (sets', cS) := sets.insert' sS S
      let (sets', cT) := sets'.insert' sT T
      sets := sets'
      if !cS then
        sub := sub ++ s!"Set {sS}\n"
      if !cT then
        sub := sub ++ s!"Set {sT}\n"
      sub := sub ++ s!"IsSubset({sS}, {sT})\n"
  if sets.isEmpty then return none
  some <$> mkSetDiag sub sets.toArray

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
def getVennGoal (ps : Params) : RequestM (RequestTask Response) := do
  RequestM.asTask do
    let html? ← ps.ci.val.runMetaM {} <| ps.mvar.withContext do
      let locs : Array LocalDecl ← ps.locs.filterMapM fun
        | .hyp fv => return some (← fv.getDecl)
        | .hypType fv _ => return some (← fv.getDecl)
        | _ => return none
      isSetGoal? locs
    return { html? }

@[widget_module]
def SetDisplayPanel : Component PanelWidgetProps where
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
        return rs.call('getVennGoal', \{ ci: g.ctx, mvar: g.mvarId, locs })
      }, [props.selectedLocations, props.goals, rs])
      let inner = undefined
      if (st.state === 'resolved')
        inner = e(DynamicComponent, \{
          pos: props.pos,
          hash: '{hash HtmlDisplay.javascript}',
          props: \{
            pos: props.pos,
            html: st.value.html ?? \{ text: 'No set goal.' }
          }
        }, null);
      else
        inner = JSON.stringify(st)
      return e('details', \{open: true}, [
        e('summary', \{className: 'mv2 pointer'}, 'Venn diagram'),
        inner
      ])
    }
  "

/-! # Example usage -/

example {R S T U : Set Nat} :
    S ⊆ U → T ⊆ U → R ⊆ S → R ⊆ U := by
  with_panel_widgets [SetDisplayPanel]
    intro h₁ _ h₃
    -- Place your cursor here.
    exact fun n h => h |> h₃ |> h₁
