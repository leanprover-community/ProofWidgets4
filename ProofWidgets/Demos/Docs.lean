import ProofWidgets.Component.HtmlDisplay

open Lean ProofWidgets Elab Command Server Json
open scoped ProofWidgets.Jsx

syntax (name := infoview_browse) "#browse " str : command

@[command_elab infoview_browse]
def browseElab : CommandElab
  | stx@`(command| #browse $url:str) => do
    runTermElabM fun _ => do
    let docs := ProofWidgets.Html.ofTHtml <|
      THtml.element "iframe" #[("src", url.getString),
        ("width", "100%"), ("height", "600px"), ("frameborder", "0")] #[]
    savePanelWidgetInfo stx ``HtmlDisplayPanel do
      return json% { html: $(← rpcEncode docs) }
  | stx => throwError s!"Unsupported syntax {stx}."

syntax (name := web_macro) "#web " ident str : command

@[macro web_macro]
def webMacro : Macro
| `(command| #web $site:ident $url:str) => do
  let raw := toString site.getId
  let cmdName : TSyntax `str := quote <| "#" ++ raw
  let cmdCmd : TSyntax `command := ⟨.node .none (.mkSimple raw) #[.atom .none ("#" ++ raw)]⟩
  `(syntax (name := $site) $cmdName:str : command

    @[command_elab $site]
    def elabCmd : CommandElab
      | stx@`(command| $cmdCmd:command) =>
        runTermElabM fun _ => do
          let docs := ProofWidgets.Html.ofTHtml <|
            THtml.element "iframe" #[("src", $url:str),
              ("width", "100%"), ("height", "600px"), ("frameborder", "0")] #[]
          savePanelWidgetInfo stx ``HtmlDisplayPanel do
            return .mkObj [("html", ← rpcEncode docs)]
      | stx => throwError s!"Unexpected syntax {stx}."
  )
| _ => Macro.throwUnsupported


#browse "https://leanprover-community.github.io/mathlib-port-status/"
#browse "https://leanprover-community.github.io/blog/"

#web desmos "https://www.desmos.com/calculator"
#web fplean "https://leanprover.github.io/functional_programming_in_lean/"
#web zulip "https://leanprover.zulipchat.com/"
#web docs "https://leanprover-community.github.io/mathlib4_docs/"

#desmos
#fplean
#zulip
#docs
