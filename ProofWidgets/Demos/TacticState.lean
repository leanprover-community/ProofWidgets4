module

public meta import ProofWidgets.Component.Panel.Basic
public meta import ProofWidgets.Component.HtmlDisplay

public meta section

/-! # Tactic state display

This demo re-implements the tactic state display of the Lean 4 infoview
(`lean4-infoview/src/infoview/{goals,info}.tsx` in https://github.com/leanprover/vscode-lean4)
as ProofWidgets components.

The JavaScript side (`widget/src/tacticState.tsx`)
only uses the public API of `@leanprover/infoview`.
Each component in the original hierarchy
```
GoalInfoDisplay
└─ FilteredGoals
   └─ Goals
      └─ Goal
         ├─ Hyp
         │  ├─ HypName
         │  └─ InteractiveCode
         └─ InteractiveCode
```
is exposed as a `ProofWidgets.Component` below,
so that each level can also be instantiated from Lean.

The top-level `GoalInfoDisplay` is a panel widget.
Place the cursor in the examples at the end of this file
to compare it with the built-in tactic state display, which is shown just above it. -/

namespace ProofWidgets.TacticState
open Lean Server Widget

@[widget_module]
def TacticStateModule : Widget.Module where
  javascript := include_str ".." / ".." / "widget" / "js" / "tacticState.js"

/-- An action that the user can request in the editor.
Corresponds to `InfoviewActionKind` in `@leanprover/infoview`. -/
inductive InfoviewActionKind where
  | toggleAllMessages | togglePaused | togglePin | copyToComment | toggleExpectedType
  deriving ToJson, FromJson

/-- Settings controlling how goals are displayed. -/
structure GoalSettingsState where
  /-- If true reverse the list of hypotheses, if false present the order received from LSP. -/
  reverse : Bool
  /-- If true hide the names of goals, otherwise show them. -/
  hideGoalNames : Bool
  /-- If true emphasize the first goal, otherwise don't emphasize it. -/
  emphasizeFirstGoal : Bool
  /-- If true show hypotheses that have isType=True, otherwise hide them. -/
  showType : Bool
  /-- If true show hypotheses that have isInstance=True, otherwise hide them. -/
  showInstance : Bool
  /-- If true show hypotheses that contain a dagger in the name, otherwise hide them. -/
  showHiddenAssumption : Bool
  /-- If true show the bodies of let-values, otherwise hide them. -/
  showLetValue : Bool
  deriving ToJson, FromJson

/-- The settings obtained from `defaultInfoviewConfig` in `@leanprover/infoview`.

The Lean server does not know the user's infoview configuration,
so these are only defaults. -/
def GoalSettingsState.default : GoalSettingsState where
  reverse := false
  hideGoalNames := false
  emphasizeFirstGoal := false
  showType := true
  showInstance := true
  showHiddenAssumption := true
  showLetValue := true

structure HypNameProps where
  name : String
  isInserted : Bool
  isRemoved : Bool
  mvarId? : Option MVarId := none
  fvarId? : Option FVarId := none
  deriving RpcEncodable

/-- A single hypothesis name.
It is highlighted on hover and selectable with shift-click
when a `LocationsContext` is available. -/
def HypName : Component HypNameProps where
  javascript := TacticStateModule.javascript
  «export» := "HypName"

structure HypProps where
  hyp : InteractiveHypothesisBundle
  mvarId? : Option MVarId := none
  deriving RpcEncodable

/-- A hypothesis bundle `h₁ h₂ : α` or `x : α := v`. -/
def Hyp : Component HypProps where
  javascript := TacticStateModule.javascript
  «export» := "Hyp"

structure GoalProps where
  goal : InteractiveGoal
  settings : GoalSettingsState := .default
  additionalClassNames : String := ""
  deriving RpcEncodable

/-- A goal with its hypotheses, target type, and optional case label. -/
def Goal : Component GoalProps where
  javascript := TacticStateModule.javascript
  «export» := "Goal"

structure GoalsProps where
  goals : InteractiveGoals
  settings : GoalSettingsState := .default
  /-- Whether or not to display the number of goals. -/
  displayCount : Bool
  deriving RpcEncodable

/-- A list of goals, or `No goals`. -/
def Goals : Component GoalsProps where
  javascript := TacticStateModule.javascript
  «export» := "Goals"

structure FilteredGoalsProps where
  /-- When this is `none`, the component is hidden but keeps its state. -/
  goals? : Option InteractiveGoals
  /-- Whether or not to display the number of goals. -/
  displayCount : Bool
  /-- Whether the list of goals should be expanded on first render. -/
  initiallyOpen : Bool
  /-- If specified, the display is toggled when this action is requested by the user. -/
  togglingAction? : Option InfoviewActionKind := none
  deriving RpcEncodable

/-- A collapsible list of goals whose header consists of the children of this component.
The goal settings are initialized from the infoview configuration,
and can be changed using the context menu. -/
def FilteredGoals : Component FilteredGoalsProps where
  javascript := TacticStateModule.javascript
  «export» := "FilteredGoals"

/-- The tactic state and expected type, displayed like in the infoview.
This is a panel widget. -/
def GoalInfoDisplay : Component PanelWidgetProps where
  javascript := TacticStateModule.javascript

/-! ## Composing the components from Lean -/

open Elab Tactic in
/-- Display the current goals in the infoview using `FilteredGoals` instantiated from Lean. -/
elab stx:"show_goals_html" : tactic => do
  let goals ← (← getUnsolvedGoals).toArray.mapM fun g => goalToInteractive g
  let html : Html := jsx%{
    <FilteredGoals goals?={some { goals }} displayCount={true} initiallyOpen={true}>
      <b>Goals rendered from Lean</b>
    </FilteredGoals>}
  Widget.savePanelWidgetInfo (hash HtmlDisplayPanel.javascript)
    (return json% { html: $(← rpcEncode html) }) stx

end ProofWidgets.TacticState

/-! ## Examples -/

open ProofWidgets.TacticState

show_panel_widgets [local GoalInfoDisplay]

-- Multiple goals with case names
example (p q : Prop) (hp : p) (hq : q) : p ∧ q := by
  constructor
  · exact hp
  · exact hq

-- Type and instance assumptions, inaccessible names, and let-values
example (α : Type) [Inhabited α] : ∀ _ : α, ∃ x : α, x = x := by
  intro
  let y : α := default
  exact ⟨y, rfl⟩

-- `conv` goals use a different goal prefix
example (a b : Nat) : a + b = b + a := by
  conv =>
    lhs
    rw [Nat.add_comm]

-- The expected type is displayed in term mode
example : 2 + 2 = 4 := by
  exact (rfl : 2 + 2 = 4)

-- The same components, composed in Lean
example (n : Nat) (h : 0 < n) : n ≠ 0 := by
  show_goals_html
  omega
