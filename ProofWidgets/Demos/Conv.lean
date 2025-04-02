/-
Copyright (c) 2022 Robin Böhne. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Robin Böhne, Wojciech Nawrocki
-/

import Lean.Meta.ExprLens
import ProofWidgets.Data.Html
import ProofWidgets.Component.Panel.Basic
import ProofWidgets.Component.OfRpcMethod
import ProofWidgets.Component.MakeEditLink

open Lean Server

/-! # Metaprogramming utilites for generating `conv` zoom-in commands from expression locations -/

structure NewCursorPos where
  newCursorPos? : Option Lsp.Position
  uri? : Option String
  deriving ToJson, FromJson

private structure SolveReturn where
  expr : Expr
  val? : Option String
  listRest : List Nat

private def solveLevel (expr : Expr) (path : List Nat) : MetaM SolveReturn := match expr with
  | Expr.app _ _ => do
    let mut descExp := expr
    let mut count := 0
    let mut explicitList := []

    -- we go through the application until we reach the end, counting how many explicit arguments it has and noting whether
    -- they are explicit or implicit
    while descExp.isApp do
      if (←Lean.Meta.inferType descExp.appFn!).bindingInfo!.isExplicit then
        explicitList := true::explicitList
        count := count + 1
      else
        explicitList := false::explicitList
      descExp := descExp.appFn!

    -- we get the correct `enter` command by subtracting the number of `true`s in our list
    let mut mutablePath := path
    let mut length := count
    explicitList := List.reverse explicitList
    while !mutablePath.isEmpty && mutablePath.head! == 0 do
      if explicitList.head! == true then
        count := count - 1
      explicitList := explicitList.tail!
      mutablePath := mutablePath.tail!

    let mut nextExp := expr
    while length > count do
      nextExp := nextExp.appFn!
      length := length - 1
    nextExp := nextExp.appArg!

    let pathRest := if mutablePath.isEmpty then [] else mutablePath.tail!

    return { expr := nextExp, val? := toString count , listRest := pathRest }

  | Expr.lam n _ b _ => do
    let name := match n with
      | Name.str _ s => s
      | _ => panic! "no name found"
    return { expr := b, val? := name, listRest := path.tail! }

  | Expr.forallE n _ b _ => do
    let name := match n with
      | Name.str _ s => s
      | _ => panic! "no name found"
    return { expr := b, val? := name, listRest := path.tail! }

  | Expr.mdata _ b => do
    match b with
      | Expr.mdata _ _ => return { expr := b, val? := none, listRest := path }
      | _ => return { expr := b.appFn!.appArg!, val? := none, listRest := path.tail!.tail! }

  | _ => do
    return { expr := ←(Lean.Core.viewSubexpr path.head! expr), val? := toString (path.head! + 1), listRest := path.tail! }


def reprint! (stx : Syntax) : String :=
  match stx.reprint with
    | some x => x
    | none =>  panic! "Could not reprint syntax"

structure LocateReturn where
  pathBeforeConv : List Nat
  pathAfterConv : List Nat
  deriving Inhabited

private def locate (stx : Syntax) (pos : String.Pos) : LocateReturn := Id.run do
  let mut t := Syntax.Traverser.fromSyntax stx
  let mut path := []
  let mut rangeList := []

  -- first, we roughly locate `pos` in the syntax
  while !t.cur.getArgs.isEmpty do
    let mut args := t.cur.getArgs
    let mut i := 0
    let mut newT := t
    let mut found := false
    rangeList := []
    for arg in args do
      let mut range := match arg.getRange? with
        | some x => x
        | none => { start := 0, stop := 0 }
      rangeList := range::rangeList
      if range.start < pos && pos <= range.stop then do
        newT := t.down i
        path := i::path
        found := true
      i := i + 1
    if !found then break
    t := newT

  -- go back up from found location to the first `conv` we find
  let mut pathAfterConv := []
  while !(t.cur.getKind == `Lean.Parser.Tactic.Conv.conv
      || t.cur.getKind == `Lean.Parser.Tactic.Conv.convConvSeq
      || t.cur.getKind == `Lean.Widget.conv!)
    && path.length > 0 do
    t := t.up
    pathAfterConv := path.head!::pathAfterConv
    path := path.tail!

  -- the cursor is in front of the first tactic or at the conv atom, we need to do some extra work
  if pathAfterConv == [] || pathAfterConv.length == 1 then
    pathAfterConv := [t.cur.getArgs.size - 2]

  -- the cursor is in front of another tactic, we need to do some extra work
  else if pathAfterConv.length == 3 then
    let mut rangeList' := rangeList.reverse
    let mut ctr := 0
    while rangeList'.head!.stop < pos do
      ctr := ctr + 1
      rangeList' := rangeList'.tail!
    pathAfterConv := List.append pathAfterConv ((ctr-2)::[0])

  return {pathBeforeConv := path.reverse, pathAfterConv := pathAfterConv }

private partial def extractIndentation (input : String) : String := match "  ".isPrefixOf input with
  | true => "  " ++ extractIndentation (input.drop 2)
  | false => ""

private structure InsertReturn where
  stx : Syntax
  newPath : List Nat
  newCursorPos : Lsp.Position

private def insertAfterArrow (stx : Syntax) (pathBeforeConvParam : List Nat) (pathAfterConvParam : List Nat) (value : String) (text : FileMap) : InsertReturn := Id.run do
  let mut t := Syntax.Traverser.fromSyntax stx
  let mut pathBeforeConv := pathBeforeConvParam
  while pathBeforeConv.length > 0 do
    t := t.down pathBeforeConv.head!
    pathBeforeConv := pathBeforeConv.tail!
  let mut pathAfterConv := pathAfterConvParam

--move up to find previous whitespace
  let mut pathBeforeConv' := pathBeforeConvParam.reverse
  let mut returnPath := []
  for _ in [:4] do
    t := t.up
    returnPath := pathBeforeConv'.head!::returnPath
    pathBeforeConv' := pathBeforeConv'.tail!
  t := t.up

  --get previous whitespace
  let argNr := pathBeforeConv'.head! - 1
  let prevArg := reprint! t.cur.getArgs[argNr]!
  let mut previousIndentation :=  extractIndentation (prevArg.splitOn "\n").reverse.head!

  -- move back down to `conv`
  t := t.down pathBeforeConv'.head!
  while returnPath.length > 0 do
    t := t.down returnPath.head!
    returnPath := returnPath.tail!

  -- we also need the whitespace fron the `=>` node
  let arrow := reprint! t.cur.getArgs[pathAfterConv.head!]!
  let mut arrowIndentation := extractIndentation (arrow.splitOn "\n").reverse.head!
  let mut newCursorPos := match t.cur.getArgs[pathAfterConv.head!]!.getRange? with
    | some range => text.utf8PosToLspPos range.stop
    | none => text.utf8PosToLspPos 0

  let mut newNode := Syntax.missing
  --if there is an empty conv body, we need to remove the newlines from the `=>`
  if reprint! t.cur.getArgs[pathAfterConv.head! + 1]! == "" || previousIndentation == arrowIndentation then
    let newArrow := Syntax.atom (SourceInfo.original "".toSubstring 0 "".toSubstring 0) "=>\n"
    t := t.setCur (t.cur.setArgs (List.append (t.cur.getArgs.toList.take pathAfterConv.head!) (newArrow::t.cur.getArgs.toList.drop (pathAfterConv.head! + 1))).toArray)
    newNode := Syntax.atom (SourceInfo.original "".toSubstring 0 "".toSubstring 0) ("  " ++ previousIndentation ++ value ++ "\n" ++ arrowIndentation)
    newCursorPos := { line := newCursorPos.line + 1, character := 2 + previousIndentation.length + value.length }
  else
    newNode := Syntax.atom (SourceInfo.original "".toSubstring 0 "".toSubstring 0) (value ++ "\n" ++ arrowIndentation)
    newCursorPos := { line := newCursorPos.line + 1, character := arrowIndentation.length + value.length }

  -- move down to args of `conv`
  t := t.down (pathAfterConv.head! + 1)
  t := t.down 0
  t := t.down 0

  -- add new node to Syntax and move to the very top
  let newArgList := newNode::t.cur.getArgs.toList
  t := t.setCur (t.cur.setArgs newArgList.toArray)
  while t.parents.size > 0 do
    t := t.up

  let newPath := pathBeforeConvParam ++ (pathAfterConv.head! + 1)::0::0::[0]

  return { stx := t.cur, newPath := newPath, newCursorPos }

private def insertAnywhereElse (stx : Syntax) (pathBeforeConvParam : List Nat) (pathAfterConvParam : List Nat) (value : String) (text : FileMap): InsertReturn := Id.run do
  let mut t := Syntax.Traverser.fromSyntax stx
  let mut pathBeforeConv := pathBeforeConvParam
  while pathBeforeConv.length > 0 do
    t := t.down pathBeforeConv.head!
    pathBeforeConv := pathBeforeConv.tail!
  let mut pathAfterConv := pathAfterConvParam

--check if other tactics follow after the `conv` block
  let nothingAfterConv := t.up.cur.getArgs.size - 1 == pathBeforeConvParam.reverse.head!

  -- move down to args of `conv`
  for _ in [:3] do
    t := t.down pathAfterConv.head!
    pathAfterConv := pathAfterConv.tail!

  -- check if it's an enter and if yes, merge them
  let argAsString := reprint! t.cur.getArgs[pathAfterConv.head!]!
  let mut newval := value
  let mut entersMerged := false
  if toString t.cur.getArgs[pathAfterConv.head!]!.getKind == "Lean.Parser.Tactic.Conv.«convEnter[__]»" then
    let mut additionalArgs := (argAsString.splitOn "\n").head!
    additionalArgs := (additionalArgs.drop "enter [".length).dropRight 1

    let left := value.take "enter [".length
    let right := value.drop "enter [".length
    newval := left ++ additionalArgs ++ ", " ++ right
    entersMerged := true

  --get end position of previous node
  let mut newCursorPos := match t.cur.getArgs[pathAfterConv.head!]!.getRange? with
    | some range => text.utf8PosToLspPos range.stop
    | none => text.utf8PosToLspPos 0

  --get whitespace from previous tactic and make new node
  let mut argNr := pathAfterConv.head!
  let mut indentation := ""
  if argNr == 0 then
    -- in this case, we need to grab the whitespace from `=>`
    indentation := extractIndentation ((reprint! t.up.up.up.cur).splitOn "\n").tail!.head!
  else
    argNr := argNr - 2
    let mut prevArg := reprint! t.cur.getArgs[argNr]!
    let mut split := prevArg.splitOn "\n"
    -- if there is no `\n`, we take the whitespace from the following node instead
    while split.length == 1 do
      argNr := argNr + 2
      prevArg := reprint! t.cur.getArgs[argNr]!
      split := prevArg.splitOn "\n"

    let mut indentationLine := split.reverse.head!
    indentation := extractIndentation indentationLine

  newCursorPos := match entersMerged with
    | true => { line := newCursorPos.line, character := indentation.length + newval.length }
    | false => { line := newCursorPos.line + 1, character := indentation.length + newval.length }

  -- if we are inserting after the last element of the conv block, we need to add additional indentation in front of our tactic,
  -- and remove some at the end.
  let mut frontIndentation := ""
  if pathAfterConv.head! == t.cur.getArgs.size - 1 then
    let lastArg := reprint! t.cur.getArgs[ t.cur.getArgs.size - 1]!
    let mut lastArgIndentation := extractIndentation (lastArg.splitOn "\n").reverse.head!
    let numOfWhitespace := indentation.length - lastArgIndentation.length
    for _ in [:numOfWhitespace] do
      frontIndentation := frontIndentation ++ " "
    indentation := indentation.drop numOfWhitespace

    -- add new node to syntax and move to the very top
  let mut argList := []
  --if there are no tactics after the conv block, we need to remove all but one `\n` from the last tactic
  if nothingAfterConv then
    let mut adjustedLastLine := reprint! t.cur.getArgs[t.cur.getArgs.size - 1]!
    while adjustedLastLine.takeRight 1 == "\n" do
      adjustedLastLine := adjustedLastLine.dropRight 1
    adjustedLastLine := adjustedLastLine ++ "\n"
    let adjustedLastNode := Syntax.atom (SourceInfo.original "".toSubstring 0 "".toSubstring 0) adjustedLastLine
    argList := (adjustedLastNode::t.cur.getArgs.toList.reverse.tail!).reverse
  else
    argList := t.cur.getArgs.toList

  let newNode := match entersMerged with
    | true => Syntax.atom (SourceInfo.original "".toSubstring 0 "".toSubstring 0) (newval ++ "\n" ++ indentation)
    | false => Syntax.atom (SourceInfo.original "".toSubstring 0 "".toSubstring 0) (frontIndentation ++ newval ++ "\n" ++ indentation)
  let newArgList := match entersMerged with
    | true => List.append (argList.take (pathAfterConv.head!) ) (newNode::(argList.drop (pathAfterConv.head! + 1)))
    | false => List.append (argList.take (pathAfterConv.head! + 1) ) (newNode::(argList.drop (pathAfterConv.head! + 1)))
  let newPath := match entersMerged with
    | true => pathBeforeConvParam ++ (pathAfterConvParam.take 4)
    | false => pathBeforeConvParam ++ (pathAfterConvParam.take 3) ++ [pathAfterConvParam[3]! + 2]

  t := t.setCur (t.cur.setArgs newArgList.toArray)
  while t.parents.size > 0 do
    t := t.up

  return { stx := t.cur, newPath := newPath, newCursorPos := newCursorPos }

private def syntaxInsert (stx : Syntax) (pathBeforeConvParam : List Nat) (pathAfterConvParam : List Nat) (value : String) (text : FileMap): InsertReturn := Id.run do
  if value == "" then return { stx := stx, newPath := pathBeforeConvParam ++ pathAfterConvParam, newCursorPos := {line := 0, character := 0} }
  if pathAfterConvParam.length == 1 then
    return insertAfterArrow stx pathBeforeConvParam pathAfterConvParam value text
  else
    return insertAnywhereElse stx pathBeforeConvParam pathAfterConvParam value text

structure InsertEnterResponse where
  /-- The description of the action to display in the UI. -/
  label : String
  edit : Lsp.TextDocumentEdit
  /-- Where to place the cursor after the edit. We assume that it is always in the same file. -/
  newCursorPos : Lsp.Position
  deriving FromJson, ToJson

def insertEnter (subexprPos : SubExpr.Pos) (goalType : Expr) (cmdStx : Syntax)
    (cursorPos : String.Pos) (doc : Lean.Server.FileWorker.EditableDocument) : MetaM InsertEnterResponse := do
  let mut list := (SubExpr.Pos.toArray subexprPos).toList
  let mut expr := goalType
  let mut retList := []
  -- generate list of commands for `enter`
  while !list.isEmpty do
    let res ← solveLevel expr list
    expr := res.expr
    retList := match res.val? with
      | none => retList
      | some val => val::retList
    list := res.listRest

  -- build `enter [...]` string
  retList := List.reverse retList
  let mut enterval := "enter " ++ toString retList
  if enterval.contains '0' then enterval := "Error: Not a valid conv target"
  if retList.isEmpty then enterval := ""

  let range := match cmdStx.getRange? with
    | some x => x
    | none => panic! "could not get range"

  -- insert `enter [...]` string into syntax
  let text := doc.meta.text

  let located := locate cmdStx { byteIdx := (min cursorPos.byteIdx range.stop.byteIdx) }
  let inserted := syntaxInsert cmdStx located.pathBeforeConv located.pathAfterConv enterval text
  let mut newSyntax := reprint! inserted.stx

  --drop newlines and whitespace at the end
  let mut syntaxAsList := newSyntax.data.reverse
  while syntaxAsList.head! == '\n' || syntaxAsList.head! == ' ' do
    newSyntax := newSyntax.dropRight 1
    syntaxAsList := syntaxAsList.tail!

  -- insert new syntax into document
  let textEdit : Lsp.TextEdit := { range := { start := text.utf8PosToLspPos range.start, «end» := text.utf8PosToLspPos { byteIdx := range.stop.byteIdx } }, newText := newSyntax }
  let edit : Lsp.TextDocumentEdit := { textDocument := { uri := doc.meta.uri, version? := doc.meta.version }, edits := [textEdit].toArray }

  return { label := enterval, edit, newCursorPos := inserted.newCursorPos }

/-! # Conv widget -/

open ProofWidgets

structure ConvButtonProps where
  pos : Lsp.Position
  goal : Widget.InteractiveGoal
  loc : SubExpr.GoalLocation
  deriving RpcEncodable

open scoped Jsx in
@[server_rpc_method]
def ConvButton.rpc (props : ConvButtonProps) : RequestM (RequestTask Html) :=
  RequestM.withWaitFindSnapAtPos props.pos fun snap => do
    if props.goal.goalPrefix != "| " then
      throw $ .invalidParams s!"The current goal is not a conv goal."
    let .target subexprPos := props.loc
      | throw $ .invalidParams s!"Select something in the target type."
    let doc ← RequestM.readDoc
    let cursorPos := doc.meta.text.lspPosToUtf8Pos props.pos
    props.goal.ctx.val.runMetaM {} do
      let md ← props.goal.mvarId.getDecl
      let lctx := md.lctx |>.sanitizeNames.run' {options := (← getOptions)}
      Meta.withLCtx lctx md.localInstances do
        let resp ← insertEnter subexprPos md.type snap.stx cursorPos doc
        return <div><MakeEditLink
            edit={resp.edit}
            newSelection?={some ⟨resp.newCursorPos, resp.newCursorPos⟩}
            title?={resp.label}
          >
            {.text resp.label}
          </MakeEditLink></div>

@[widget_module]
def ConvButton : Component ConvButtonProps :=
  mk_rpc_widget% ConvButton.rpc

def findGoalForLocation (goals : Array Widget.InteractiveGoal) (loc : SubExpr.GoalsLocation) :
    Option Widget.InteractiveGoal :=
  goals.find? (·.mvarId == loc.mvarId)

open scoped Jsx in
@[server_rpc_method]
def ConvPanel.rpc (props : PanelWidgetProps) : RequestM (RequestTask Html) :=
  RequestM.asTask do
    let inner : Html ← (do
      if props.selectedLocations.isEmpty then
        return <span>No actions available. You can use shift-click to select an expression in the goal state.</span>
      let buttons : Array Html ← props.selectedLocations.mapM fun loc => do
        let some g := findGoalForLocation props.goals loc
          | throw $ .invalidParams s!"could not find goal for location {toJson loc}"
        return <ConvButton pos={props.pos} goal={g} loc={loc.loc} />
      return Html.element "div" #[] buttons)
    return <details «open»={true}>
        <summary className="mv2 pointer">Conv 🔍</summary>
        <div className="ml1">{inner}</div>
      </details>

@[widget_module]
def ConvPanel : Component PanelWidgetProps :=
  mk_rpc_widget% ConvPanel.rpc

/-! # Example usage -/

-- Like `sorry` but avoids a warning for demonstration purposes.
axiom test_sorry {α} : α

example [Add α] [Neg α] [OfNat α (nat_lit 0)]
    (h₁ : ∀ (a : α), a + 0 = a)
    (h₂ : ∀ (a b c : α), (a + b) + c = a + (b + c))
    (h₃ : ∀ (a : α), a + (-a) = 0) :
    ∀ (a : α), (-a) + a = 0 :=
by with_panel_widgets [ConvPanel]
  intro a
  have : ∀ (a : α), a + a = a → a = 0 := by
    intro a h
    rw [← h₁ a, ← h₃ a, ← h₂, h]
  apply this
  rw [← h₂]
  conv =>
    -- Place your cursor in the `conv` block
    skip
  exact test_sorry
