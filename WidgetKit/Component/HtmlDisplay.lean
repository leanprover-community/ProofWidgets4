import Lean.Server.Rpc.Basic

import WidgetKit.Data.Html

namespace WidgetKit
open Lean Server

structure HtmlDisplayProps where
  html : EncodableHtml

#mkrpcenc HtmlDisplayProps

@[widget_module]
def HtmlDisplay : Component HtmlDisplayProps where
  javascript := include_str ".." / ".." / "build" / "js" / "htmlDisplay.js"

@[widget_module]
def HtmlDisplayPanel : Component HtmlDisplayProps where
  javascript := include_str ".." / ".." / "build" / "js" / "htmlDisplayPanel.js"

open Elab in
unsafe def evalEncodableHtmlUnsafe (stx : Term) : TermElabM EncodableHtml := do
  let htmlT := mkConst ``EncodableHtml
  Term.evalTerm EncodableHtml htmlT stx

open Elab in
@[implemented_by evalEncodableHtmlUnsafe]
opaque evalEncodableHtml : Term → TermElabM EncodableHtml

syntax (name := htmlCmd) "#html " term : command

open Elab Command Json in
@[command_elab htmlCmd]
def elabHtmlCmd : CommandElab := fun
  | stx@`(#html $t:term) =>
    runTermElabM fun _ => do
      let ht ← evalEncodableHtml <| ← `(WidgetKit.EncodableHtml.ofHtml $t)
      savePanelWidgetInfo stx ``HtmlDisplayPanel do
        return json% { html: $(← rpcEncode ht) }
  | stx => throwError "Unexpected syntax {stx}."

syntax (name := htmlTac) "html! " term : tactic

open Elab Tactic Json in
@[tactic htmlTac]
def elabHtmlTac : Tactic
  | stx@`(tactic| html! $t:term) => do
    let ht ← evalEncodableHtml <| ← `(WidgetKit.EncodableHtml.ofHtml $t)
    savePanelWidgetInfo stx ``HtmlDisplayPanel do
      return json% { html: $(← rpcEncode ht) }
  | stx => throwError "Unexpected syntax {stx}."

end WidgetKit
