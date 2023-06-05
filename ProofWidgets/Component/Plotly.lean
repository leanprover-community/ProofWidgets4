import ProofWidgets.Component.Basic
import ProofWidgets.Component.HtmlDisplay

open Lean ProofWidgets

namespace Plotly

@[widget_module]
def Plotly : ProofWidgets.Module where
  javascript := include_str "../../build/js/react-plotly.js"

def Plot : Component Json where
  javascript := Plotly.javascript

end Plotly

open Plotly
open scoped ProofWidgets.Jsx
open scoped ProofWidgets.Json

syntax (name := plotCmd) "#plot " jso : command

open Elab Command Server Json in
@[command_elab plotCmd]
def elabPlotCmd : CommandElab
  | stx@`(command| #plot $cfg) => do
    runTermElabM fun _ => do
      let plot ← `(ProofWidgets.Html.ofTHtml <| THtml.component Plot (json% $cfg) #[])
      let ht ← evalHtml plot
      savePanelWidgetInfo stx ``HtmlDisplayPanel do
        return json% { html: $(← rpcEncode ht) }
  | _ => throwError "Unexpected syntax for plotting."
