import ProofWidgets.Component.Panel.GoalTypePanel
import ProofWidgets.Component.HtmlDisplay
import ProofWidgets.Component.Latex
import ProofWidgets.Presentation.Expr

open ProofWidgets Jsx

@[expr_presenter]
def latex_presenter : ExprPresenter where
  userName := "Latex"
  layoutKind := .inline
  present e :=
    return <span>
        <Latex content={(← Lean.Meta.ppExpr e).pretty} />
      </span>

example : 2 + 2 = 4 ∧ 3 + 3 = 6 := by
  with_panel_widgets [GoalTypePanel]
    -- Place cursor here.
    constructor
    rfl
    rfl

macro "#latex " text: String : command =>
  Lean.TSyntax.mkInfoCanonical <$>
    `(#html <Latex content={$text} />)

#latex "\\mbox{This is Latex: }\\forall x \\in \\mathbb{N}, x\\ge 0,\, \\mbox{ rendered as SVG.}"
