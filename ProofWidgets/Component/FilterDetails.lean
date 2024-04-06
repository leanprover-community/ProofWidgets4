import ProofWidgets.Data.Html

open Lean ProofWidgets

/-- The props for the `FilterDetails` component. -/
structure FilterDetailsProps where
  /-- The title -/
  message : String
  /-- What is shown in the filtered state -/
  filtered : Html
  /-- What is shown in the non-filtered state -/
  all : Html
  /-- Whether to start in the filtered state -/
  initiallyFiltered : Bool
deriving Server.RpcEncodable

/-- the `FilterDetails` component is like a details Html element, but it also has a filter button
that allows you to switch between a filtered and unfiltered state -/
@[widget_module]
def FilterDetails : Component FilterDetailsProps where
  javascript := include_str ".." /  ".." / ".lake" / "build" / "js" / "FilterDetails.js"
