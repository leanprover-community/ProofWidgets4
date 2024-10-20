import ProofWidgets.Component.Digraph
import ProofWidgets.Component.HtmlDisplay
import Lean.Util.FoldConsts

open ProofWidgets Jsx

/-- Display the graph of all constants appearing in a given constant. -/
syntax (name := exprGraphCmd) "#expr_graph" ident : command

open Lean Elab Command in
@[command_elab exprGraphCmd]
def elabExprGraphCmd : CommandElab := fun
  | stx@`(#expr_graph $i:ident) => runTermElabM  fun _ => do
    let env ← getEnv
    let c ← realizeGlobalConstNoOverloadWithInfo i
    let some c := env.find? c | throwError "internal error"
    let nodes : NameSet := c.getUsedConstantsAsSet
    let mut edges : Std.HashSet (String × String) := {}
    for a in nodes do for b in nodes do
      let some bb := env.find? b | continue
      if bb.getUsedConstantsAsSet.contains a then
        edges := edges.insert (toString a, toString b)
    let mut nodesWithInfos : Array DigraphDisplay.Vertex := #[]
    let mut maxRadius := 10
    for node in nodes.toArray do
      let some c := env.find? node | continue
      let doc? ← findDocString? env node
      let ee ← Lean.Widget.ppExprTagged c.type
      let us ← Meta.mkFreshLevelMVarsFor c
      let e ← Lean.Widget.ppExprTagged (.const node us)
      let node := toString node
      let rx := node.length * 3
      maxRadius := Nat.max maxRadius rx
      let newNode : DigraphDisplay.Vertex := {
        id := node
        label :=
          <g>
            <ellipse
              fill="var(--vscode-editor-background)"
              stroke="var(--vscode-editorHoverWidget-border)"
              rx={(rx*2 : Nat)}
              ry="10"
            />
            <text x={s!"-{rx}"} y="5" className="font-code">{.text node}</text>
          </g>
        radius := 15
        details? :=
          match doc? with
          | some d =>
            <div>
              <InteractiveCode fmt={e} />{.text " : "}<InteractiveCode fmt={ee} />
              <MarkdownDisplay contents={d} />
            </div>
          | none =>
            <span>
              <InteractiveCode fmt={e} />{.text " : "}<InteractiveCode fmt={ee} />
            </span>
      }
      nodesWithInfos := nodesWithInfos.push newNode
    let html : Html := <DigraphDisplay
      vertices={nodesWithInfos}
      edges={edges.fold (init := #[]) fun acc (a,b) => acc.push {source := a, target := b}}
      forces={#[
        .link { distance? := Float.ofNat (maxRadius * 2) },
        .collide { radius? := Float.ofNat maxRadius },
        .x { strength? := some 0.05 },
        .y { strength? := some 0.05 }
      ]}
    />
    Widget.savePanelWidgetInfo
      (hash HtmlDisplayPanel.javascript)
      (return json% { html : $(← Server.rpcEncode html) })
      stx
  | stx => throwError "Unexpected syntax {stx}"

def a : Nat := 0
def b : Nat := 1
def foo (c : Nat) : Nat × Int := (a + b * c, a / b)

-- Put your cursor here.
#expr_graph foo
