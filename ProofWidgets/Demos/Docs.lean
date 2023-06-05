import ProofWidgets.Component.HtmlDisplay

open Lean ProofWidgets Elab Command Server Json
open scoped ProofWidgets.Jsx

#html <iframe src="https://leanprover-community.github.io/mathlib4_docs/" width="100%" height="500px" frameborder="0"></iframe>

syntax (name := browse_macro) "browse " ident str : command

@[macro browse_macro]
def browseMacro : Macro
| `(command| browse $site:ident $url:str) => do
  let raw := toString site.getId
  let cmdName : TSyntax `str := quote <| "#" ++ raw
  let cmdCmd : TSyntax `command := ⟨.node .none (.mkSimple raw) #[.atom .none ("#" ++ raw)]⟩
  let out ←  `(syntax (name := $site) $cmdName:str : command

    @[command_elab $site]
    def elabCmd : CommandElab
      | stx@`(command| $cmdCmd:command) =>
        runTermElabM fun _ => do
          let docs := ProofWidgets.Html.ofTHtml <|
            <iframe src=$url:str
                width="100%" height="600px" frameborder="0">
            </iframe>
          savePanelWidgetInfo stx ``HtmlDisplayPanel do
            return json% { html: $(← rpcEncode docs) }
      | stx => throwError s!"Unexpected syntax {stx}."
  )
  dbg_trace out.raw.reprint.get!
  pure out
| _ => Macro.throwUnsupported

#check liftM

syntax (name := docs) "#docs" : command

@[command_elab docs]
def elabDocsCmd : CommandElab
  | stx@`(command| #docs) => do
    runTermElabM fun _ => do
      let docs := ProofWidgets.Html.ofTHtml <|
        <iframe src="https://leanprover-community.github.io/mathlib4_docs/"
            width="100%" height="600px" frameborder="0">
        </iframe>
      savePanelWidgetInfo stx ``HtmlDisplayPanel do
        return json% { html: $(← rpcEncode docs) }
  | stx => throwError "Unexpected syntax {stx}."

browse desmos "https://www.desmos.com/calculator"

#desmos
#docs
