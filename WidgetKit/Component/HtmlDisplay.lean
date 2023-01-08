import Lean.Server.Rpc.Basic

import WidgetKit.Data.Html

namespace WidgetKit
open Lean Server

/-- We cannot build `RpcEncodable Html` because `Html : Type 1` and `RpcEncodable` is (sadly) not
universe-polymorphic, but we can do it for this intermediate type. -/
inductive HtmlRaw where
  | element : String → Array (String × Json) → Array HtmlRaw → HtmlRaw
  | text : String → HtmlRaw
  | component : UInt64 → StateM RpcObjectStore Json → Array HtmlRaw → HtmlRaw
  deriving Inhabited, RpcEncodable

partial def HtmlRaw.ofHtml : Html → HtmlRaw
  | .element t as cs => element t as (cs.map ofHtml)
  | .text s => text s
  | @Html.component _ _ c ps cs => component (hash c.javascript) (rpcEncode ps) (cs.map ofHtml)

@[widget_module]
def htmlDisplay : String :=
  include_str ".." / ".." / "widget" / "dist" / "htmlDisplay.js"

open Elab in
unsafe def evalHtmlRawUnsafe (stx : Term) : TermElabM HtmlRaw := do
  let htmlT := mkConst ``HtmlRaw
  Term.evalTerm HtmlRaw htmlT stx

open Elab in
@[implemented_by evalHtmlRawUnsafe]
opaque evalHtmlRaw : Term → TermElabM HtmlRaw

syntax (name := htmlCmd) "#html " term : command

open Elab Command Json in
@[command_elab htmlCmd]
def elabHtmlCmd : CommandElab := fun
  | stx@`(#html $t:term) =>
    runTermElabM fun _ => do
      let ht ← evalHtmlRaw <| ← `(WidgetKit.HtmlRaw.ofHtml $t)
      savePanelWidgetInfo stx ``htmlDisplay do
        return json% { html: $(← rpcEncode ht) }
  | stx => throwError "Unexpected syntax {stx}."

syntax (name := htmlTac) "html! " term : tactic

open Elab Tactic Json in
@[tactic htmlTac]
def elabHtmlTac : Tactic
  | stx@`(tactic| html! $t:term) => do
    let ht ← evalHtmlRaw <| ← `(WidgetKit.HtmlRaw.ofHtml $t)
    savePanelWidgetInfo stx ``htmlDisplay do
      return json% { html: $(← rpcEncode ht) }
  | stx => throwError "Unexpected syntax {stx}."

end WidgetKit
