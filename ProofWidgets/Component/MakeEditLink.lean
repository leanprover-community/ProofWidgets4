import Lean.Server.Utils
import ProofWidgets.Component.Basic

namespace ProofWidgets
open Lean

structure MakeEditLinkProps where
  /-- The edit to perform on the file. -/
  edit : Lsp.TextDocumentEdit
  /-- Where to place the cursor after the edit. It is in the same file as `edit`.
  If not present, the cursor is not moved. -/
  newCursorPos? : Option Lsp.Position := none
  /-- The `title` property, if any, to set on `<a>`. -/
  title? : Option String := none
  deriving FromJson, ToJson

/-- Replace `range` with `newText` and then place the cursor at the end of the new text. -/
def MakeEditLinkProps.ofReplaceRange (meta : Server.DocumentMeta) (range : Lsp.Range)
    (newText : String) : MakeEditLinkProps :=
  let edit := { textDocument := { uri := meta.uri, version? := meta.version }
                edits        := #[{ range, newText }] }
  let newCursorPos? := some {
    line := range.start.line
    character := range.start.character + newText.codepointPosToUtf16Pos newText.length
  }
  { edit, newCursorPos? }

/-- A link that, when clicked, makes the specifies edit and potentially moves the cursor. -/
@[widget_module]
def MakeEditLink : Component MakeEditLinkProps where
  javascript := include_str ".." / ".." / "build" / "js" / "makeEditLink.js"

end ProofWidgets
