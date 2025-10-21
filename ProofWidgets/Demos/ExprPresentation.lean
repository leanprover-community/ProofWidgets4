module

public meta import ProofWidgets.Component.Panel.SelectionPanel
public meta import ProofWidgets.Component.Panel.GoalTypePanel

public meta section

open ProofWidgets Jsx

@[expr_presenter]
def presenter : ExprPresenter where
  userName := "With octopodes"
  layoutKind := .inline
  present e :=
    return <span>
        {.text "🐙 "}<InteractiveCode fmt={← Lean.Widget.ppExprTagged e} />{.text " 🐙"}
      </span>

example : 2 + 2 = 4 ∧ 3 + 3 = 6 := by
  with_panel_widgets [GoalTypePanel]
    -- Place cursor here.
    constructor
    rfl
    rfl

example (_h : 2 + 2 = 5) : 2 + 2 = 4 := by
  with_panel_widgets [SelectionPanel]
    -- Place cursor here and select subexpressions in the goal with shift-click.
    rfl
