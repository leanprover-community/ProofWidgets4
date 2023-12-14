import Lean.Server.Rpc.Basic

import ProofWidgets.Component.Panel.Basic
import ProofWidgets.Data.Html

namespace ProofWidgets
open Lean Server

structure HtmlDisplayProps where
  html : Html
  deriving RpcEncodable

@[widget_module]
def HtmlDisplay : Component HtmlDisplayProps where
  javascript := include_str ".." / ".." / ".lake" / "build" / "js" / "htmlDisplay.js"

@[widget_module]
def HtmlDisplayPanel : PanelWidget HtmlDisplayProps where
  javascript := include_str ".." / ".." / ".lake" / "build" / "js" / "htmlDisplayPanel.js"

open Elab in
unsafe def evalHtmlUnsafe (stx : Term) : TermElabM Html := do
  let htmlT := mkConst ``Html
  Term.evalTerm Html htmlT stx

open Elab in
@[implemented_by evalHtmlUnsafe]
opaque evalHtml : Term → TermElabM Html

syntax (name := htmlCmd) "#html " term : command

open Elab Command Json in
@[command_elab htmlCmd]
def elabHtmlCmd : CommandElab := fun
  | stx@`(#html $t:term) =>
    runTermElabM fun _ => do
      let ht ← evalHtml t
      savePanelWidgetInfo' HtmlDisplayPanel ⟨ht⟩ stx
  | stx => throwError "Unexpected syntax {stx}."

syntax (name := htmlTac) "html! " term : tactic

open Elab Tactic Json in
@[tactic htmlTac]
def elabHtmlTac : Tactic
  | stx@`(tactic| html! $t:term) => do
    let ht ← evalHtml t
    savePanelWidgetInfo' HtmlDisplayPanel ⟨ht⟩ stx
  | stx => throwError "Unexpected syntax {stx}."

end ProofWidgets
