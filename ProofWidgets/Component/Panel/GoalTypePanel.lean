import ProofWidgets.Component.Panel.Basic

namespace ProofWidgets

/-- Display the goal type using known `Expr` presenters. -/
@[widget_module]
def GoalTypePanel : PanelWidget NoProps where
  javascript := include_str ".." / ".." / ".." / ".lake" / "build" / "js" / "goalTypePanel.js"

end ProofWidgets
