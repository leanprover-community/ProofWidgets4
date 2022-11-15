/- Author: Wojciech Nawrocki -/
import Lean.Widget.UserWidget
import Lean.Elab.Eval
import WidgetKit.Html

open Lean Widget in
@[widget]
def staticHtmlWidget : UserWidgetDefinition where
  name := "HTML Display"
  javascript := include_str ".." / "widget" / "dist" / "staticHtml.js"

open Lean Elab Widget in
unsafe def evalHtmlUnsafe (stx : Syntax) : TermElabM Html := do
  let htmlT := mkConst ``Html
  Term.evalTerm Html htmlT stx

open Lean Elab Widget in
@[implemented_by evalHtmlUnsafe]
opaque evalHtml : Syntax → TermElabM Html

syntax (name := htmlCmd) "#html " term : command

open Lean Meta Elab Command in
@[command_elab htmlCmd]
def elabHtmlCmd : CommandElab := fun
  | stx@`(#html $t:term) =>
    runTermElabM fun _ => do
      let id := `staticHtmlWidget
      let ht ← evalHtml t
      let props := Json.mkObj [("html", toJson ht)]
      Lean.Widget.saveWidgetInfo id props stx
  | stx => throwError "Unexpected syntax {stx}."

syntax (name := htmlTac) "html! " term : tactic

open Lean Elab Tactic in
@[tactic htmlTac]
def elabHtmlTac : Tactic
  | stx@`(tactic| html! $t:term) => do
    let id := `staticHtmlWidget
    let ht ← evalHtml t
    let props := Json.mkObj [("html", toJson ht)]
    Lean.Widget.saveWidgetInfo id props stx
  | stx => throwError "Unexpected syntax {stx}."
