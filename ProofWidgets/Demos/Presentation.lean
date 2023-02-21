import ProofWidgets.Presentation.Goal

open ProofWidgets Jsx

@[expr_presenter]
def presenter : ExprPresenter where
  userName := "With octopodes"
  isApplicable _ := return true
  present e :=
    return EncodableHtml.ofHtml
      <span>
        {.text "🐙 "}<InteractiveCode fmt={← Lean.Widget.ppExprTagged e} />{.text " 🐙"}
      </span>

example (h : 2 + 2 = 5) : 2 + 2 = 4 := by
  withSelectionDisplay
  -- Place cursor here and select subexpressions in the goal with shift-click
    rfl
