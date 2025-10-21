module

public meta import ProofWidgets.Component.GraphDisplay
public meta import ProofWidgets.Component.Panel.Basic
public meta import ProofWidgets.Component.OfRpcMethod

public meta section

/-! This demo visualizes the graph of metavariable assignments in tactic proofs. -/

universe u v

open Lean Meta

/-- Adjacency list representation of a directed graph with labelled edges. -/
abbrev Graph (α β : Type u) [BEq α] [Hashable α] := Std.HashMap α (List (α × β))

variable {α β : Type u} [BEq α] [Hashable α]

/-- Filter out vertices not reachable from a vertex in `vs`. -/
def Graph.filterReachable (g : Graph α β) (vs : List α) : Graph α β := Id.run do
  let mut work := vs
  let mut reached := Std.HashSet.ofList vs
  -- FIXME: `do` notation freaks out if `a` and `β` live in different universes
  while h : !work.isEmpty do
    let v := work.head (by simpa using h)
    work := work.tail
    for (u, _) in g.get? v |>.getD [] do
      if !reached.contains u then
        work := u :: work
        reached := reached.insert u
  return g.filter fun v _ => reached.contains v

/-- Return the user name if it exists, otherwise the prettified underlying name. -/
def _root_.Lean.MVarId.getName (m : MVarId) : MetaM Name := do
  let d ← m.getDecl
  if !d.userName.isAnonymous then
    return d.userName
  let .num _ n := m.name | return m.name
  return Name.anonymous.str "m" |>.num n

/-- Return the graph of metavariables,
where a `true` edge `m → n` exists iff `n` is assigned an expression that uses `m`,
and a `false` edge `m → n` exists iff the type of `n` depends on `m`. -/
def buildMVarGraph : MetaM (Graph MVarId Bool) := do
  let mut g : Graph MVarId Bool := {}
  for (m, _) in (← getMCtx).decls do
    g := g.alter m (·.getD [])
    if let some e := (← getMCtx).eAssignment.find? m then
      for n in (e.collectMVars {}).result do
        g := g.alter n ((m, true) :: ·.getD [])
    if let some a := (← getMCtx).dAssignment.find? m then
      let n := a.mvarIdPending
      g := g.alter n ((m, true) :: ·.getD [])
    for n in ((← m.getType).collectMVars {}).result do
      g := g.alter n ((m, false) :: ·.getD [])
  return g

open ProofWidgets Jsx in
def mkLabel (e : MVarId)
    (stroke := "var(--vscode-editor-foreground)")
    (fill := "var(--vscode-editor-background)") :
    MetaM (Nat × Nat × Html) := do
  let fmt := s!"?{← e.getName}"
  let fmtTp ← withOptions (·.setNat `pp.deepTerms.threshold 2)
    (toString <$> ppExpr (← e.getType'))
  let len := fmt.length + fmtTp.length + 3
  let w := min (15 + len * 6) 100
  let h := max 20 (20 * (1 + len / 15))
  let x : Int := -w/2
  let y : Int := -h/2
  return (w, h, <g>
      <rect
        fill={fill}
        stroke={stroke}
        strokeWidth={.num 1.5}
        width={w} height={h} x={x} y={y}
        rx={5}
      />
      <foreignObject width={w} height={h} x={.num (x + 5 : Int)} y={y}>
        <span className="font-code">{.text fmt} : {.text fmtTp}</span>
      </foreignObject>
    </g>
  )

open ProofWidgets Jsx in
def drawMVarGraph (goals : List MVarId) : MetaM Html := do
  let mvars ← buildMVarGraph
  let mg := goals.head!
  let g := mvars.filterReachable goals

  let mut vertices := #[]
  let mut edges := #[]
  let mut maxLabelRadius := 0.0
  for (m, ns) in g do
    -- TODO: mark delayed assigned mvars in yet another colour?
    let vAssigned ← m.isAssignedOrDelayedAssigned
    let (w, h, label) ←  mkLabel m
      (stroke :=
        if m == mg then "var(--vscode-lean4-infoView\\.turnstile)"
        else if vAssigned then "var(--vscode-editor-foreground)"
        else "var(--vscode-lean4-infoView\\.caseLabel)")
      (fill :=
        if vAssigned then "var(--vscode-editorHoverWidget-background)"
        else "var(--vscode-editor-background)")
    maxLabelRadius := max maxLabelRadius (Float.sqrt <| (w.toFloat/2)^2 + (h.toFloat/2)^2)
    let val? ← (← getMCtx).eAssignment.find? m |>.mapM fun v =>
      return <span className="font-code">
        {.text s!"?{← m.getName}"} :=
        <InteractiveCode fmt={← m.withContext <| Widget.ppExprTagged v} />
      </span>
    let delayedVal? ← (← getMCtx).dAssignment.find? m |>.mapM fun n =>
      return <span className="font-code">
        {.text s!"?{← m.getName}"} [delayed] :=
        {.text s!"?{← n.mvarIdPending.getName}"}
      </span>
    vertices := vertices.push {
      id := toString m.name
      label
      boundingShape := .rect w.toFloat h.toFloat
      details? := val? <|> delayedVal?
    }
    for (n, b) in ns do
      if b then
        edges := edges.push {
          source := toString m.name
          target := toString n.name
        }
      else
        edges := edges.push {
          source := toString m.name
          target := toString n.name
          attrs := #[("strokeDasharray", "5,5")]
        }
  return <GraphDisplay
      vertices={vertices}
      edges={edges}
      forces={#[
        .link { distance? := some (maxLabelRadius * 3) },
        .manyBody { strength? := some (-150) },
        .x { strength? := some 0.01 },
        .y { strength? := some 0.01 }
      ]}
      showDetails={true}
    />

open Server ProofWidgets Jsx

@[server_rpc_method]
def MVarGraph.rpc (props : PanelWidgetProps) : RequestM (RequestTask Html) :=
  RequestM.asTask do
    let inner : Html ← (do
      -- Are there any goals unsolved? If so, the first one is the current main goal.
      if props.goals.isEmpty then
        return <span>No goals.</span>
      let some g := props.goals[0]? | unreachable!

      -- Execute the next part using the metavariable context and local context of the main goal.
      g.ctx.val.runMetaM {} do
        let md ← g.mvarId.getDecl
        let lctx := md.lctx |>.sanitizeNames.run' {options := (← getOptions)}
        Meta.withLCtx lctx md.localInstances do
          drawMVarGraph <| props.goals.toList.map (·.mvarId))

    return <details «open»={true}>
        <summary className="mv2 pointer">Metavariable graph</summary>
        <div className="ml1">{inner}</div>
      </details>

@[widget_module]
def MVarGraph : Component ProofWidgets.PanelWidgetProps :=
  mk_rpc_widget% MVarGraph.rpc

show_panel_widgets [local MVarGraph]

-- When printing an assigned metavariable `?m := v`,
-- print out the metavariable name `?m` rather than `v`.
set_option pp.instantiateMVars false
-- Do the same for delayed assigned metavariables.
set_option pp.mvars.delayed true

example {P Q : Prop} (p : P) (q : Q) : P ∧ Q := by
  constructor
  . exact p
  . exact q

example {P Q R : Prop} (p : P) (q : Q) (r : R) : (P ∧ Q) ∧ R := by
  constructor
  constructor
  . exact p
  . exact q
  . exact r

example {P Q R : Prop} (pq : P → Q) (qr : Q → R) : P → R := by
  intro p
  apply qr
  apply pq
  exact p

example {α : Type} {P Q : α → Prop} (pq : ∀ x, P x → Q x)
    (px : ∀ x, P x) : ∀ x, Q x := by
  intro y
  apply pq
  apply px

example {α : Type} {P : α → Prop} {a : α} (pa : P a) :
    ∃ x, P x := by
  refine ⟨?w, ?h⟩
  exact a
  exact pa

example {α : Type} {P : α → Prop} {a b c : α}
    (ab : a = b) (bc : b = c) (pa : P a) : P c := by
  rw [← bc]
  rw [← ab]
  exact pa

example (n : Nat) : n = 0 ∨ ∃ m, n = m + 1 := by
  induction n -- a big mess
  . exact Or.inl rfl
  . exact Or.inr ⟨_, rfl⟩
