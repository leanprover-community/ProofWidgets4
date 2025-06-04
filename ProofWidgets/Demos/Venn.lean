import Lean.Elab.Tactic
import ProofWidgets.Component.Panel.Basic
import ProofWidgets.Component.PenroseDiagram
import ProofWidgets.Component.HtmlDisplay
import ProofWidgets.Component.OfRpcMethod

open Lean Meta Server
open ProofWidgets

/-! # Minimal definiton of sets copied from Mathlib -/
def Set (α : Type u) := α → Prop

namespace Set

/-- Membership in a set -/
protected def Mem (s : Set α) (a : α) : Prop :=
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
  let some (_, _, S, T) := e.app4? ``HasSubset.Subset | none
  return (S, T)

/-- Expressions to display as labels in a diagram. -/
abbrev ExprEmbeds := Array (String × Expr)

open scoped Jsx in
def mkSetDiag (sub : String) (embeds : ExprEmbeds) : MetaM Html := do
  let embeds ← embeds.mapM fun (s, h) =>
      return (s, <InteractiveCode fmt={← Widget.ppExprTagged h} />)
  return <PenroseDiagram
      embeds={embeds}
      dsl={include_str ".."/".."/"widget"/"penrose"/"setTheory.dsl"}
      sty={include_str ".."/".."/"widget"/"penrose"/"venn.sty"}
      sub={sub} />

def isSetGoal? (hyps : Array LocalDecl) : MetaM (Option Html) := do
  let mut sub := "AutoLabel All\n"
  let mut sets : Std.HashMap String Expr := ∅
  for assm in hyps do
    let tp ← instantiateMVars assm.type
    if let some (S, T) := isSubsetPred? tp then
      let sS ← toString <$> Lean.Meta.ppExpr S
      let sT ← toString <$> Lean.Meta.ppExpr T
      let (cS, sets') := sets.containsThenInsert sS S
      let (cT, sets') := sets'.containsThenInsert sT T
      sets := sets'
      if !cS then
        sub := sub ++ s!"Set {sS}\n"
      if !cT then
        sub := sub ++ s!"Set {sT}\n"
      sub := sub ++ s!"IsSubset({sS}, {sT})\n"
  if sets.isEmpty then return none
  some <$> mkSetDiag sub sets.toArray

/-! # Implementation of the widget -/

def findGoalForLocation (goals : Array Widget.InteractiveGoal) (loc : SubExpr.GoalsLocation) :
    Option Widget.InteractiveGoal :=
  goals.find? (·.mvarId == loc.mvarId)

open scoped Jsx in
@[server_rpc_method]
def VennDisplay.rpc (props : PanelWidgetProps) : RequestM (RequestTask Html) :=
  RequestM.asTask do
    let inner : Html ← (do
      if props.selectedLocations.isEmpty then
        return <span>Use shift-click to select hypotheses to include in the diagram.</span>
      let some selectedLoc := props.selectedLocations[0]? | unreachable!

      let some g := findGoalForLocation props.goals selectedLoc
        | throw $ .invalidParams
            s!"could not find goal for location {toJson selectedLoc}"
      g.ctx.val.runMetaM {} do
        let md ← g.mvarId.getDecl
        let lctx := md.lctx |>.sanitizeNames.run' {options := (← getOptions)}
        Meta.withLCtx lctx md.localInstances do
          let locs : Array LocalDecl ← props.selectedLocations.filterMapM fun
            | ⟨mv, .hyp fv⟩ | ⟨mv, .hypType fv _⟩ =>
              if mv == g.mvarId then return some (← fv.getDecl) else return none
            | _ => return none
          match ← isSetGoal? locs with
          | some html => return html
          | none => return <span>No set goal.</span>)
    return <details «open»={true}>
        <summary className="mv2 pointer">Venn diagram</summary>
        <div className="ml1">{inner}</div>
      </details>

@[widget_module]
def VennDisplay : Component PanelWidgetProps :=
  mk_rpc_widget% VennDisplay.rpc

/-! # Example usage -/

example {R S T U : Set Nat} :
    S ⊆ U → T ⊆ U → R ⊆ S → R ⊆ U := by
  with_panel_widgets [VennDisplay]
    intro h₁ _ h₃
    -- Place your cursor here.
    exact fun n h => h |> h₃ |> h₁
