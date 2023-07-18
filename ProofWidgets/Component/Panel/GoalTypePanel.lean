import ProofWidgets.Component.Panel.Basic

namespace ProofWidgets

/-- Display the goal type using known `Expr` presenters. -/
@[widget_module]
def GoalTypePanel : Component PanelWidgetProps where
  javascript := include_str ".." / ".." / ".." / "build" / "js" / "goalTypePanel.js"

end ProofWidgets
