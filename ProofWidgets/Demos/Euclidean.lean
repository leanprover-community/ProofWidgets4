/-
 Copyright (c) 2023 Vladimir Sedlacek. All rights reserved.
 Released under Apache 2.0 license as described in the file LICENSE.
 Authors: Vladimir Sedlacek, Wojciech Nawrocki
 -/

import Lean.Data.HashMap
import Lean.Elab.Tactic
import ProofWidgets.Component.PenroseDiagram
import ProofWidgets.Component.HtmlDisplay
import ProofWidgets.Component.Panel.Basic
import ProofWidgets.Component.OfRpcMethod
import ProofWidgets.Component.MakeEditLink

/-! A widget to display Euclidean geometry diagrams,
and another one to make geometric constructions in the UI. -/

open Lean Meta Server
open ProofWidgets

/-! # Minimal definitions of synthetic geometric primitives

Inspired by https://github.com/ah1112/synthetic_euclid_4. -/

class IncidenceGeometry where
  Point : Type u₁
  Line : Type u₂
  between : Point → Point → Point → Prop -- implies colinearity
  onLine : Point → Line → Prop
  ne_23_of_between : ∀ {a b c : Point}, between a b c → b ≠ c
  line_unique_of_pts : ∀ {a b : Point}, ∀ {L M : Line},
    a ≠ b → onLine a L → onLine b L → onLine a M → onLine b M → L = M
  onLine_2_of_between : ∀ {a b c : Point}, ∀ {L : Line},
    between a b c → onLine a L → onLine c L → onLine b L
  line_of_pts : ∀ a b, ∃ L, onLine a L ∧ onLine b L

variable [i : IncidenceGeometry]
open IncidenceGeometry

/-! # Metaprogramming utilities to break down expressions -/

/-- If `e == onLine a L` return `some (a, L)`, otherwise `none`. -/
def isOnLinePred? (e : Expr) : Option (Expr × Expr) := do
  let some (_, a, L) := e.app3? ``onLine | none
  return (a, L)

/-- If `e == between a b c` return `some (a, b, c)`, otherwise `none`. -/
def isBetweenPred? (e : Expr) : Option (Expr × Expr × Expr) := do
  let some (_, a, b, c) := e.app4? ``between | none
  return (a, b, c)

def isPoint? (e : Expr) : Bool :=
  e.isAppOf ``Point

def isLine? (e : Expr) : Bool :=
  e.isAppOf ``Line

/-! # Utilities for constructing diagrams -/

structure DiagramState where
  /-- The Penrose substance program.
  Note that `embeds` are added lazily at the end. -/
  sub : String := ""
  /-- Components to display as labels in the diagram,
  mapped as name ↦ (type, html). -/
  embeds : HashMap String (String × Html) := .empty

abbrev DiagramBuilderM := StateT DiagramState MetaM

open scoped Jsx in
def buildDiagram : DiagramBuilderM (Option Html) := do
  let st ← get
  if st.sub == "" && st.embeds.isEmpty then
    return none
  let mut sub := "AutoLabel All\n"
  let mut embedHtmls := #[]
  for (n, (tp, h)) in st.embeds.toArray do
    sub := sub ++ s!"{tp} {n}\n"
    embedHtmls := embedHtmls.push (n, h)
  sub := sub ++ st.sub
  return <PenroseDiagram
    embeds={embedHtmls}
    dsl={include_str ".."/".."/"widget"/"penrose"/"euclidean.dsl"}
    sty={include_str ".."/".."/"widget"/"penrose"/"euclidean.sty"}
    sub={sub} />

/-- Add a substance `nm` of Penrose type `tp`,
labelled by `h` to the substance program. -/
def addEmbed (nm : String) (tp : String) (h : Html) : DiagramBuilderM Unit := do
  modify fun st => { st with embeds := st.embeds.insert nm (tp, h )}

open scoped Jsx in
/-- Add a substance of Penrose type `tp`,
corresponding to (and labelled by) the expression `e`,
to the substance program.
Return its Penrose name. -/
def addExpr (tp : String) (e : Expr) : DiagramBuilderM String := do
  let nm ← toString <$> Lean.Meta.ppExpr e
  let h := <InteractiveCode fmt={← Widget.ppExprTagged e} />
  addEmbed nm tp h
  return nm

def addExpr' (tp : String) (e : Expr) : DiagramBuilderM Unit := do
  let _ ← addExpr tp e

/-- Add instruction `i` to the substance program. -/
def addInstruction (i : String) : DiagramBuilderM Unit := do
  modify fun st => { st with sub := st.sub ++ s!"{i}\n" }

def addHypotheses (hyps : Array LocalDecl) : DiagramBuilderM Unit := do
  for h in hyps do
    let tp ← instantiateMVars h.type
    if isPoint? tp then
      addExpr' "Point" h.toExpr
    if isLine? tp then
      addExpr' "Line" h.toExpr
    if let some (a, L) := isOnLinePred? tp then
      let sa ← addExpr "Point" a
      let sL ← addExpr "Line" L
      addInstruction s!"On({sa}, {sL})"
    if let some (a, b, c) := isBetweenPred? tp then
      let sa ← addExpr "Point" a
      let sb ← addExpr "Point" b
      let sc ← addExpr "Point" c
      addInstruction s!"Between({sa}, {sb}, {sc})"

def DiagramBuilderM.run (x : DiagramBuilderM α) : MetaM α :=
  x.run' {}

/-! # Implementation of the widget -/

open scoped Jsx in
@[server_rpc_method]
def EuclideanDisplay.rpc (props : PanelWidgetProps) : RequestM (RequestTask Html) :=
  RequestM.asTask do
    let inner : Html ← (do
      -- Are there any goals unsolved? If so, pick the first one.
      if props.goals.isEmpty then
        return <span>No goals.</span>
      let some g := props.goals[0]? | unreachable!

      -- Execute the next part using the metavariable context and local context of the goal.
      g.ctx.val.runMetaM {} do
        let md ← g.mvarId.getDecl
        let lctx := md.lctx |>.sanitizeNames.run' {options := (← getOptions)}
        Meta.withLCtx lctx md.localInstances do
          -- Which hypotheses have been selected in the UI,
          -- meaning they should *not* be shown in the display.
          let mut hiddenLocs : HashSet FVarId := mkHashSet props.selectedLocations.size
          for l in props.selectedLocations do
            match l with
            | ⟨mv, .hyp fv⟩ | ⟨mv, .hypType fv _⟩ =>
              if mv == g.mvarId then
                hiddenLocs := hiddenLocs.insert fv
            | _ => continue
          -- Filter local declarations by whether they are not in `hiddenLocs`.
          let locs := (← getLCtx).decls.toArray.filterMap (fun d? =>
            if let some d := d? then
              if !hiddenLocs.contains d.fvarId then some d else none
            else
              none)
          -- Produce the diagram.
          DiagramBuilderM.run do
            addHypotheses locs
            match ← buildDiagram with
            | some html => return html
            | none => return <span>No Euclidean goal.</span>)

    return <details «open»={true}>
        <summary className="mv2 pointer">Euclidean diagram</summary>
        <div className="ml1">{inner}</div>
      </details>

@[widget_module]
def EuclideanDisplay : Component PanelWidgetProps :=
  mk_rpc_widget% EuclideanDisplay.rpc

/-! # Example usage -/

example {a b c : Point} {L M : Line} (Babc : between a b c) (aL : onLine a L) (bM : onLine b M)
    (cL : onLine c L) (cM : onLine c M) : L = M := by
  with_panel_widgets [EuclideanDisplay]
      -- Place your cursor here.
    have bc := ne_23_of_between Babc
    have bL := onLine_2_of_between Babc aL cL
    exact line_unique_of_pts bc bL cL bM cM

/-! # Euclidean constructions -/

open scoped Jsx in
/-- Add every possible line between any two points in `hyps`
to the diagram.
Lines are labelled with links to insert them into the proof script. -/
def constructLines (hyps : Array LocalDecl) (meta : Server.DocumentMeta) (cursorPos : Lsp.Position)
    : DiagramBuilderM Unit := do
  -- Identify all the points.
  let mut points : Array LocalDecl := {}
  for h in hyps do
    let tp ← instantiateMVars h.type
    if isPoint? tp then
      points := points.push h

  -- Construct every possible line.
  for hi : i in [0:points.size] do
    for hj : j in [i+1:points.size] do
      let p := points[i]'hi.upper
      let q := points[j]'hj.upper
      let sp ← addExpr "Point" p.toExpr
      let sq ← addExpr "Point" q.toExpr
      let nm := s!"{sp}{sq}"
      let ctr := s!"let ⟨{nm}, _, _⟩ := line_of_pts {sp} {sq}"
      -- Add the line, labelled with a link that makes the text edit.
      addEmbed nm "Line" (
        <span>
          <b>{.text nm}</b> ({
            .ofComponent MakeEditLink
              (MakeEditLinkProps.ofReplaceRange meta ⟨cursorPos, cursorPos⟩ ctr)
              #[.text "insert"]
          })
        </span>)
      addInstruction s!"On({sp}, {nm})"
      addInstruction s!"On({sq}, {nm})"
      addInstruction s!"Emphasize({nm})"

open scoped Jsx in
@[server_rpc_method]
def EuclideanConstructions.rpc (props : PanelWidgetProps) : RequestM (RequestTask Html) :=
  RequestM.asTask do
    let doc ← RequestM.readDoc
    let inner : Html ← (do
      -- Are there any goals unsolved? If so, pick the first one.
      if props.goals.isEmpty then
        return <span>No goals.</span>
      let some g := props.goals[0]? | unreachable!

      -- Execute the next part using the metavariable context and local context of the goal.
      g.ctx.val.runMetaM {} do
        let md ← g.mvarId.getDecl
        let lctx := md.lctx |>.sanitizeNames.run' {options := (← getOptions)}
        Meta.withLCtx lctx md.localInstances do

          -- Grab all hypotheses from the local context.
          let allHyps := (← getLCtx).decls.toArray.filterMap id

          -- Find which hypotheses are selected.
          let selectedHyps ← props.selectedLocations.filterMapM fun
            | ⟨mv, .hyp fv⟩ | ⟨mv, .hypType fv _⟩ =>
              return if mv == g.mvarId then some (← fv.getDecl) else none
            | _ =>
              return none

          -- Produce the diagram.
          DiagramBuilderM.run do
            addHypotheses allHyps
            constructLines selectedHyps doc.meta props.pos
            match ← buildDiagram with
            | some html => return html
            | none => return <span>No Euclidean goal.</span>)

    -- Return a collapsible `<details>` block around our widget.
    return <details «open»={true}>
        <summary className="mv2 pointer">Euclidean constructions</summary>
        <div className="ml1">{inner}</div>
      </details>

@[widget_module]
def EuclideanConstructions : Component PanelWidgetProps :=
  mk_rpc_widget% EuclideanConstructions.rpc

axiom test_sorry {α} : α

example {a b _c _d : Point} : ∃ L, onLine a L ∧ onLine b L := by
  with_panel_widgets [EuclideanConstructions]
    -- Place your cursor below.

    exact test_sorry
