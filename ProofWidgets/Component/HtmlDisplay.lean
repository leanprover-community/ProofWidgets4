import Lean.Server.Rpc.Basic
import Lean.Elab.Command

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
def HtmlDisplayPanel : Component HtmlDisplayProps where
  javascript := include_str ".." / ".." / ".lake" / "build" / "js" / "htmlDisplayPanel.js"

open Lean Server Elab Command

/-- Any term `t : α` with a `HtmlEval α` instance
can be evaluated in a `#html t` command.

This is analogous to how `Lean.MetaEval` supports `#eval`. -/
class HtmlEval (α : Type u) where
  eval : α → CommandElabM Html

instance : HtmlEval Html where
  eval ht := pure ht

instance [MonadLiftT m CommandElabM] : HtmlEval (m Html) where
  eval := monadLift

instance : HtmlEval (CoreM Html) where
  eval := liftCoreM

instance : HtmlEval (MetaM Html) where
  eval x := liftTermElabM x

instance : HtmlEval (TermElabM Html) where
  eval := liftTermElabM

namespace HtmlCommand

open Elab Command

unsafe def evalCommandMHtmlUnsafe (stx : Term) : TermElabM (CommandElabM Html) := do
  let tp := mkApp (mkConst ``CommandElabM) (mkConst ``Html)
  Term.evalTerm _ tp stx

@[implemented_by evalCommandMHtmlUnsafe]
opaque evalCommandMHtml : Term → TermElabM (CommandElabM Html)

/-- Display a value of type `Html` in the infoview.

The input can be a pure value
or a computation in any Lean metaprogramming monad
(e.g. `CommandElabM Html`). -/
syntax (name := htmlCmd) "#html " term : command

@[command_elab htmlCmd]
def elabHtmlCmd : CommandElab := fun
  | stx@`(#html $t:term) => do
    let htX ← liftTermElabM <| evalCommandMHtml <| ← ``(HtmlEval.eval $t)
    let ht ← htX
    liftCoreM <| Widget.savePanelWidgetInfo
      (hash HtmlDisplayPanel.javascript)
      (return json% { html: $(← rpcEncode ht) })
      stx
  | stx => throwError "Unexpected syntax {stx}."

/-- The `html!` tactic is deprecated and does nothing.
If you have a use for it,
please open an issue on https://github.com/leanprover-community/ProofWidgets4. -/
@[deprecated]
syntax (name := htmlTac) "html! " term : tactic

open Tactic in
@[tactic htmlTac]
def elabHtmlTac : Tactic | _ => pure ()

end HtmlCommand
end ProofWidgets

/-- Construct a structured message from ProofWidgets HTML.

For the meaning of `alt`, see `MessageData.ofWidget`. -/
def Lean.MessageData.ofHtml (h : ProofWidgets.Html) (alt : String) : CoreM MessageData :=
  MessageData.ofComponent ProofWidgets.HtmlDisplay ⟨h⟩ alt
