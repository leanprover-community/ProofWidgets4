import ProofWidgets.Component.SelectionPanel

open ProofWidgets Jsx

@[expr_presenter]
def presenter : ExprPresenter where
  userName := "With octopodes"
  layoutKind := .inline
  present e :=
    return Html.ofTHtml
      <span>
        {.text "🐙 "}<InteractiveCode fmt={← Lean.Widget.ppExprTagged e} />{.text " 🐙"}
      </span>

example (h : 2 + 2 = 5) : 2 + 2 = 4 := by
  withPanelWidgets [SelectionPanel]
  -- Place cursor here and select subexpressions in the goal with shift-click
    rfl
