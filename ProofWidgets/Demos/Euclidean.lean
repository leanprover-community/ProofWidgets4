/-
 Copyright (c) 2023 Vladimir Sedlacek. All rights reserved.
 Released under Apache 2.0 license as described in the file LICENSE.
 Authors: Vladimir Sedlacek, Wojciech Nawrocki
 -/

import Lean.Elab.Tactic
import ProofWidgets.Component.PenroseDiagram
import ProofWidgets.Component.HtmlDisplay
import ProofWidgets.Component.Panel.Basic
import ProofWidgets.Component.OfRpcMethod
import ProofWidgets.Component.MakeEditLink

/-! A widget to display Euclidean geometry diagrams,
and another one to make geometric constructions in the UI. -/

open Lean Meta Server
open ProofWidgets Penrose

/-! # Minimal definitions of synthetic geometric primitives

Inspired by https://github.com/ah1112/synthetic_euclid_4. -/

class IncidenceGeometry where
  Point : Type u₁
  Line : Type u₂
  Circle : Type u₃

  between : Point → Point → Point → Prop -- implies colinearity
  onLine : Point → Line → Prop
  onCircle : Point → Circle → Prop
  inCircle : Point → Circle → Prop
  centerCircle : Point → Circle → Prop
  circlesInter : Circle → Circle → Prop

  ne_23_of_between : ∀ {a b c : Point}, between a b c → b ≠ c
  line_unique_of_pts : ∀ {a b : Point}, ∀ {L M : Line},
    a ≠ b → onLine a L → onLine b L → onLine a M → onLine b M → L = M
  onLine_2_of_between : ∀ {a b c : Point}, ∀ {L : Line},
    between a b c → onLine a L → onLine c L → onLine b L
  line_of_pts : ∀ a b, ∃ L, onLine a L ∧ onLine b L
  circle_of_ne : ∀ a b, a ≠ b → ∃ C, centerCircle a C ∧ onCircle b C
  circlesInter_of_onCircle_inCircle :
    ∀ {a b α β}, onCircle b α → onCircle a β → inCircle a α →
    inCircle b β → circlesInter α β
  pts_of_circlesInter : ∀ {α β}, circlesInter α β →
    ∃ a b, a ≠ b ∧ onCircle a α ∧ onCircle a β ∧ onCircle b α ∧ onCircle b β
  inCircle_of_centerCircle : ∀ {a α}, centerCircle a α → inCircle a α

open IncidenceGeometry

/-! # Metaprogramming utilities to break down expressions -/

/-- If `e == between a b c` return `some (a, b, c)`, otherwise `none`. -/
def isBetweenPred? (e : Expr) : Option (Expr × Expr × Expr) := do
  let some (_, a, b, c) := e.app4? ``between | none
  return (a, b, c)

/-- If `e == onLine a L` return `some (a, L)`, otherwise `none`. -/
def isOnLinePred? (e : Expr) : Option (Expr × Expr) := do
  let some (_, a, L) := e.app3? ``onLine | none
  return (a, L)

/-- If `e == onCircle a C` return `some (a, C)`, otherwise `none`. -/
def isOnCirclePred? (e : Expr) : Option (Expr × Expr) := do
  let some (_, a, C) := e.app3? ``onCircle | none
  return (a, C)

/-- If `e == inCircle a C` return `some (a, C)`, otherwise `none`. -/
def isInCirclePred? (e : Expr) : Option (Expr × Expr) := do
  let some (_, a, C) := e.app3? ``inCircle | none
  return (a, C)

/-- If `e == centerCircle a C` return `some (a, C)`, otherwise `none`. -/
def isCenterCirclePred? (e : Expr) : Option (Expr × Expr) := do
  let some (_, a, C) := e.app3? ``centerCircle | none
  return (a, C)

/-- If `e == circlesInter a C` return `some (a, C)`, otherwise `none`. -/
def isCirclesInterPred? (e : Expr) : Option (Expr × Expr) := do
  let some (_, a, C) := e.app3? ``circlesInter | none
  return (a, C)

def isPoint? (e : Expr) : Bool :=
  e.isAppOf ``Point

def isLine? (e : Expr) : Bool :=
  e.isAppOf ``Line

/-! # Utilities for constructing diagrams -/

open DiagramBuilderM in
def addHypotheses (hyps : Array LocalDecl) : DiagramBuilderM Unit := do
  for h in hyps do
    let tp ← instantiateMVars h.type
    if isPoint? tp then
      discard $ addExpr "Point" h.toExpr
    if isLine? tp then
      discard $ addExpr "Line" h.toExpr
    if let some (a, b, c) := isBetweenPred? tp then
      let sa ← addExpr "Point" a
      let sb ← addExpr "Point" b
      let sc ← addExpr "Point" c
      addInstruction s!"Between({sa}, {sb}, {sc})"
    if let some (a, L) := isOnLinePred? tp then
      let sa ← addExpr "Point" a
      let sL ← addExpr "Line" L
      addInstruction s!"OnLine({sa}, {sL})"
    if let some (a, C) := isOnCirclePred? tp then
      let sa ← addExpr "Point" a
      let sC ← addExpr "Circle" C
      addInstruction s!"OnCircle({sa}, {sC})"
    if let some (a, C) := isInCirclePred? tp then
      let sa ← addExpr "Point" a
      let sC ← addExpr "Circle" C
      addInstruction s!"InCircle({sa}, {sC})"
    if let some (a, C) := isCenterCirclePred? tp then
      let sa ← addExpr "Point" a
      let sC ← addExpr "Circle" C
      addInstruction s!"CenterCircle({sa}, {sC})"
    if let some (C, D) := isCirclesInterPred? tp then
      let sC ← addExpr "Circle" C
      let sD ← addExpr "Circle" D
      addInstruction s!"CirclesInter({sC}, {sD})"

/-! # Implementation of the widget -/

def EuclideanDisplay.dsl :=
  include_str ".."/".."/"widget"/"penrose"/"euclidean.dsl"

def EuclideanDisplay.sty :=
  include_str ".."/".."/"widget"/"penrose"/"euclidean.sty"

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
          let mut hiddenLocs : Std.HashSet FVarId := .emptyWithCapacity props.selectedLocations.size
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
            match ← DiagramBuilderM.buildDiagram dsl sty with
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

variable [i : IncidenceGeometry]

example {a b c : Point} {L M : Line} (Babc : between a b c)
    (aL : onLine a L) (bM : onLine b M) (cL : onLine c L) (cM : onLine c M) :
    L = M := by
  with_panel_widgets [EuclideanDisplay]
    -- Place your cursor here.
    have bc := ne_23_of_between Babc
    have bL := onLine_2_of_between Babc aL cL
    exact line_unique_of_pts bc bL cL bM cM

/-! # Euclidean constructions -/

open DiagramBuilderM Jsx in
/-- Add every possible line between any two points in `hyps`
to the diagram.
Lines are labelled with links to insert them into the proof script. -/
def constructLines (hyps : Array LocalDecl) (meta : Server.DocumentMeta) (cursorPos : Lsp.Position)
    : DiagramBuilderM Unit := do
  -- Identify objects and hypotheses from which constructions can be made.
  let mut points : Array LocalDecl := {}
  let mut circleInters : Array (LocalDecl × LocalDecl × LocalDecl) := {}
  for h in hyps do
    let tp ← instantiateMVars h.type
    if isPoint? tp then
      points := points.push h
    if let some (.fvar C, .fvar D) := isCirclesInterPred? tp then
      circleInters := circleInters.push (h, ← C.getDecl, ← D.getDecl)

  -- Add a plausible construction, labelled with a link that makes the text edit.
  let addConstruction (nm tp ctr : String) : DiagramBuilderM Unit := do
    addEmbed nm tp (
      <span>
        <b>{.text nm}</b> ({
          .ofComponent MakeEditLink
            (MakeEditLinkProps.ofReplaceRange meta ⟨cursorPos, cursorPos⟩ ctr)
            #[.text "insert"]
        })
      </span>)
    addInstruction s!"Emphasize({nm})"

  -- Construct every possible line and circle.
  for hi : i in [0:points.size] do
    let p := points[i]
    let sp ← addExpr "Point" p.toExpr
    for hj : j in [i+1:points.size] do
      let q := points[j]
      let sq ← addExpr "Point" q.toExpr

      -- Add the line.
      let nm := s!"{sp}{sq}"
      addConstruction nm "Line" s!"let ⟨{nm}, _, _⟩ := line_of_pts {sp} {sq}"
      addInstruction s!"OnLine({sp}, {nm})"
      addInstruction s!"OnLine({sq}, {nm})"

      -- Add two possible circles.
      let nm := s!"C{sp}{sq}"
      addConstruction nm "Circle" s!"let ⟨{nm}, _, _⟩ := circle_of_ne {sp} {sq} (by assumption)"
      addInstruction s!"CenterCircle({sp}, {nm})"
      addInstruction s!"OnCircle({sq}, {nm})"

      let nm := s!"C{sq}{sp}"
      addConstruction nm "Circle" s!"let ⟨{nm}, _, _⟩ := circle_of_ne {sq} {sp} (by assumption)"
      addInstruction s!"CenterCircle({sq}, {nm})"
      addInstruction s!"OnCircle({sp}, {nm})"

  -- Construct every possible circle intersection.
  for hi : i in [0:circleInters.size] do
    let (h, C, D) := circleInters[i]
    let sC ← addExpr "Circle" C.toExpr
    let sD ← addExpr "Circle" D.toExpr
    let nm := s!"{sC}{sD}"
    let nm' := s!"{sD}{sC}"
    addConstruction nm "Point" s!"let ⟨{nm}, {nm'}, _, _, _, _, _⟩ := pts_of_circlesInter {h.userName}"
    addEmbed nm' "Point" <b>{.text nm'}</b>
    addInstruction s!"OnCircle({nm'}, {sC})"
    addInstruction s!"OnCircle({nm}, {sD})"
    addInstruction s!"OnCircle({nm}, {sC})"
    addInstruction s!"OnCircle({nm'}, {sD})"

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
              if mv == g.mvarId then return some (← fv.getDecl) else return none
            | _ =>
              return none

          -- Produce the diagram.
          DiagramBuilderM.run do
            addHypotheses allHyps
            constructLines selectedHyps doc.meta props.pos
            match ← DiagramBuilderM.buildDiagram EuclideanDisplay.dsl EuclideanDisplay.sty 1500 with
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

show_panel_widgets [local EuclideanConstructions]

/-! # Example construction -/

/- Try constructing an equilateral triangle abc
with line segment ab as the base.

Place your cursor in the proof below.
To make a construction involving objects `x, y, z`,
shift-click their names in 'Tactic state'
in order to select them.
Due to a limitation of the widget,
you can only click-to-select in 'Tactic state',
and not in the diagram.

- To construct a line on two points,
  select `a` and `b` in `a b : Point`.
- To construct a circle with center `a` passing through `b`,
  select `a` and `b` in `a b : Point`.
  You may have to prove that `a ≠ b`.
- To construct an intersection point of two circles `C` and `D`,
  you first have to provide a local proof
  of `have h : circlesInter C D := ...`
  (i.e., show that the circles intersect)
  using the axioms of `IncidenceGeometry` above.
  Then select `h` in `h : circlesInter C D`. -/
example {a b : Point} (_hab : a ≠ b) :
    ∃ L M c, onLine a L ∧ onLine b M ∧ onLine c M ∧ onLine c L := by
  -- Place your cursor here.

  exact test_sorry
