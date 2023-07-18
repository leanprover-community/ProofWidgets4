/-
Copyright (c) 2023 Patrick Massot. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Robin Böhne, Wojciech Nawrocki, Patrick Massot
-/

import Std.Lean.Position
import ProofWidgets.Component.Panel.Basic

open Lean Meta Server

/-! # Widgets generating tactic calls from locations selected in the tactic state.

This file builds infrastructure to define tactics allowing users to select things in the info
view and generate a tactic call. For instance one can use this infrastructure to build
a `conv?` tactic that allows to select a subterm of the goal and, after clicking on some link
below the tactic state, replace the `conv?` call by a `conv => enter [...]` call zooming on the
selected subterm.

The only public part of this file is the `mkSelectInsertTactic` macro. Other declarations
are internal details of the implementation of this macro. The corresponding JaveScript code,
which is in `widget/src/selectInsertPanel.tsx`, is also an implemantation detail.
-/

/-- Parameter for a widget callback that will insert a tactic call after selecting things in
the tactic state. -/
structure SelectInsertParams where
  /-- The current cursor position. -/
  cursorPos : Lsp.Position
  /-- The current context information. -/
  ctx : WithRpcRef Elab.ContextInfo
  /-- The locations selected in the tactic state. -/
  selectedLocations : Array SubExpr.GoalsLocation
  /-- The range in the source document where the result will be inserted. -/
  replaceRange : Lsp.Range
  deriving RpcEncodable

/-- Response for a widget callback that will insert a tactic call after selecting things in
the tactic state. -/
structure SelectInsertResponse where
  /-- The tactic call to be displayed in the panel. -/
  content : String
  /-- The edit to perform on the file. -/
  edit : Lsp.WorkspaceEdit
  /-- Where to place the cursor after the edit. It is always in the same file. -/
  newCursorPos : Lsp.Position
  deriving FromJson, ToJson

/-- Create the function that will actually handle the call from the widget. -/
def mkSelectInsertCommand
    (mkCmdStr : (pos : Array Lean.SubExpr.GoalsLocation) → (goalType : Expr) → MetaM String) :
    SelectInsertParams → RequestM (RequestTask (Option SelectInsertResponse)) :=
  fun ⟨cursorPos, ⟨ctx⟩, selectedLocations, replaceRange⟩ ↦
    RequestM.withWaitFindSnapAtPos cursorPos fun _ => do
      let some firstLoc := selectedLocations[0]? | return none
      let doc ← RequestM.readDoc
      ctx.runMetaM {} do
        let md ← firstLoc.mvarId.getDecl
        let lctx := md.lctx |>.sanitizeNames.run' {options := (← getOptions)}
        Meta.withLCtx lctx md.localInstances do
          let insertedCode ← mkCmdStr selectedLocations md.type
          let textDocumentEdit : Lsp.TextDocumentEdit := {
            textDocument := { uri := doc.meta.uri, version? := doc.meta.version },
            edits        := #[{range := replaceRange, newText := insertedCode }] }
          return some { content := insertedCode,
                        edit := Lsp.WorkspaceEdit.ofTextDocumentEdit textDocumentEdit,
                        /- CHECKME: the new cursor position computation below looks very naive.
                           Will it work with non-ascii insertedCode? -/
                        newCursorPos := {line := replaceRange.start.line,
                                         character := replaceRange.start.character + insertedCode.length } }

/-- Props that will be passed to the JavaScript component on top of the props inserted
automatically by `savePanelWidgetInfo`. -/
structure InsertPanelProps where
  /-- The panel title. -/
  title : String
  /-- The help text displayed when nothing is selected. -/
  help : String
  /-- The name of the callback function as a String. -/
  callback : String
  /-- The range to be replaced. -/
  replaceRange : Lsp.Range
  deriving FromJson, ToJson

@[widget_module]
def mkInsertWidget : ProofWidgets.Component InsertPanelProps where
  javascript := include_str ".."/".."/".."/"build"/"js"/"selectInsertPanel.js"

open Elab Tactic ProofWidgets in
/-- Create the tactic that will display the widget. -/
def mkInsertTactic (panel: Name) (title help : String) (callback : Name) (stx : Syntax) :
    TacticM Unit := do
  let some replaceRange := (← getFileMap).rangeOfStx? stx | return
  let props : InsertPanelProps := {
    title := title
    help := help
    callback := callback.toString
    replaceRange := replaceRange
  }
  savePanelWidgetInfo stx panel (pure <| toJson props)

/-- Define a tactic with the given name that create a panels with the given title
and help text that allow to select some locations in the tactic state and then call
the given callback function to generate a String that will replace the tactic when
users click on a link.  The callback function must have type
`(pos : Array Lean.SubExpr.GoalsLocation) → (goalType : Expr) → MetaM String`. -/
macro "mkSelectInsertTactic" name:str title:str help:str callback:ident : command =>
  let cmdName : Name := .str `mkSelectInsertCmd (name.getString ++ "_widget")
  `(
    @[server_rpc_method]
    def $(mkIdent cmdName) := mkSelectInsertCommand $callback
    elab stx:$name:str : $(mkIdent `tactic) =>
      mkInsertTactic ``mkInsertWidget $title:str $help:str $(quote cmdName) stx)
