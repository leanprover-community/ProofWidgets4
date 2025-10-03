module

public meta import ProofWidgets.Component.Panel.Basic

public meta section

namespace ProofWidgets

/-- Display the goal type using known `Expr` presenters. -/
@[widget_module]
def GoalTypePanel : Component PanelWidgetProps where
  javascript := include_str ".." / ".." / ".." / ".lake" / "build" / "js" / "goalTypePanel.js"

end ProofWidgets
