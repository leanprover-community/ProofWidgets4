import ProofWidgets.Component.Basic
import ProofWidgets.Component.HtmlDisplay

namespace ProofWidgets.Plotly
open Lean

@[widget_module]
def Plotly : Module where
  javascript := include_str "../../build/js/react-plotly.js"

structure PlotData where
  x : Array Float
  y : Array Float
  type : String
    deriving FromJson, ToJson

structure PlotLayout where
  width : Nat
  height : Nat
  title : String
    deriving FromJson, ToJson

structure PlotParams where
  data : Array PlotData
  layout : PlotLayout
    deriving FromJson, ToJson

def Plot : Component PlotParams where
  javascript := Plotly.javascript

end ProofWidgets.Plotly

open Lean ProofWidgets Plotly in
open scoped ProofWidgets.Jsx in
open scoped ProofWidgets.Json in
def plot : THtml :=
  .component Plot {
    data := #[{
      x := #[1, 2, 3],
      y := #[4, 1, 12],
      type := "scatter"
    }],
    layout := {
      width := 500,
      height := 500,
      title := "Plots in Lean4"
    }
  } #[]

#html plot
