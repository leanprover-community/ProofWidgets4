import ProofWidgets.Component.Panel


namespace ProofWidgets

@[widget_module]
def GeogebraDiagram : Component PanelWidgetProps where
  javascript := include_str ".." / ".." / "build" / "js" / "geogebraDiagram.js"

end ProofWidgets
