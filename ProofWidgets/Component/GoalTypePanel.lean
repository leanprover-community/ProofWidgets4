import ProofWidgets.Component.Panel

namespace ProofWidgets

/-- Display the goal type using known `Expr` presenters. -/
@[widget_module]
def GoalTypePanel : Component PanelWidgetProps where
  javascript := include_str ".." / ".." / "build" / "js" / "goalTypePanel.js"

end ProofWidgets
