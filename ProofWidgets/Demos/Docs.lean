import ProofWidgets.Component.HtmlDisplay

open scoped ProofWidgets.Jsx

syntax (name := docsCmd) "#docs" : command

#html <iframe src="https://leanprover-community.github.io/mathlib4_docs/" width="100%" height="500px" frameborder="0"></iframe>

open Lean ProofWidgets Elab Command Server Json in
@[command_elab docsCmd]
def elabDocsCmd : CommandElab
  | stx@`(command| #docs) => do
    runTermElabM fun _ => do
      let docs := ProofWidgets.Html.ofTHtml <|
        <iframe src="https://leanprover-community.github.io/mathlib4_docs/"
            width="100%" height="600px" frameborder="0">
        </iframe>
      savePanelWidgetInfo stx ``HtmlDisplayPanel do
        return json% { html: $(← rpcEncode docs) }
  | _ => throwError "Unexpected syntax for rendering docs."

#docs
