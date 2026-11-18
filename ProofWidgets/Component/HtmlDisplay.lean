module

import Lean.Data.Html.Elab
public import ProofWidgets.Data.Html

public meta section

namespace ProofWidgets
open Lean Server

structure HtmlDisplayProps where
  html : ProofWidgets.Html
  deriving RpcEncodable

@[widget_module]
def HtmlDisplay : Component HtmlDisplayProps where
  javascript := include_str ".." / ".." / "widget" / "js" / "htmlDisplay.js"

@[widget_module]
def HtmlDisplayPanel : Component HtmlDisplayProps where
  javascript := include_str ".." / ".." / "widget" / "js" / "htmlDisplayPanel.js"

/-- Construct a structured message from ProofWidgets HTML.

The nested message `alt` should approximate `html`
without itself using `html.toMessageData` (see `MessageData.ofWidget`). -/
def Html.toMessageData (html : ProofWidgets.Html) (alt : MessageData) : MessageData :=
  -- FIXME: use `ProofWidgets.Component.toMessageData`
  .ofWidget {
    id := ``HtmlDisplay
    javascriptHash := HtmlDisplay.javascriptHash
    props := Server.RpcEncodable.rpcEncode (α := HtmlDisplayProps) { html }
  } alt

open Lean Server Elab Command

@[deprecated MonadEval +typeChanged (since := "2026-10-19")]
class HtmlEval (α : Type u) where
  eval : α → CommandElabM ProofWidgets.Html

namespace HtmlCommand

open Elab Command

@[deprecated "No longer used." (since := "2026-11-05")]
unsafe def evalCommandMHtmlUnsafe (stx : Term) : TermElabM (CommandElabM ProofWidgets.Html) := do
  let tp := mkApp (mkConst ``CommandElabM) (mkConst ``ProofWidgets.Html)
  Term.evalTerm _ tp stx

@[deprecated "No longer used" (since := "2026-11-05"), implemented_by evalCommandMHtmlUnsafe]
opaque evalCommandMHtml : Term → TermElabM (CommandElabM ProofWidgets.Html)

open Meta in
@[command_elab Lean.Elab.Html.htmlCmd]
meta def elabHtmlCmd : CommandElab
  | `(#html%$tk $t:term) => elabEvalCore false tk t none mkMessageData (silenceUnit := false)
  | _ => throwUnsupportedSyntax
where
  mkMessageData (h : Expr) : MetaM Expr := do
    let ty ← inferType h
    unless ← isDefEq ty (mkConst ``ProofWidgets.Html) do
      -- Try core elaborator instead.
      throwUnsupportedSyntax
    let alt := mkApp (mkConst ``MessageData.ofFormat)
      -- FIXME: render HTML into `alt`
      (mkApp (mkConst ``Format.text) (mkStrLit "<#html command>"))
    return mkApp2 (mkConst ``ProofWidgets.Html.toMessageData) h alt

end HtmlCommand
end ProofWidgets

-- FIXME: deprecate in favor of `ProofWidgets.Html.toMessageData`
/-- Construct a structured message from ProofWidgets HTML.

For the meaning of `alt`, see `MessageData.ofWidget`. -/
def Lean.MessageData.ofHtml (h : ProofWidgets.Html) (alt : String) : CoreM MessageData :=
  MessageData.ofComponent ProofWidgets.HtmlDisplay ⟨h⟩ alt
