import ProofWidgets.Component.Panel.SelectionPanel
import ProofWidgets.Component.Panel.GoalTypePanel

open ProofWidgets Jsx Lean Server

structure LatexProps where
  content : String
  deriving Server.RpcEncodable

@[widget_module]
def Latex : Component LatexProps where
  javascript := include_str ".." / ".." / ".lake" / "build" / "js" / "latexToSvg.js"

@[expr_presenter]
def latex_presenter : ExprPresenter where
  userName := "Latex"
  layoutKind := .inline
  present e :=
    return <span>
        {.text "🐙 "}<Latex content={(← Lean.Meta.ppExpr e).pretty} />{.text " 🐙"}
      </span>

@[expr_presenter]
def latex_presenter2 : ExprPresenter where
  userName := "Latex2"
  layoutKind := .block
  present e :=
    return <span>
        {.text "🐙 "}<Latex content="\\frac{1}{2}\\mbox{hello, we will prove that } \\begin{align}a sdfs&=&sdfsb\\\\c&d&i\\end{align}" />{.text " 🐙"}
      </span>

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
