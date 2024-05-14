import ProofWidgets.Component.InfoGraph
import ProofWidgets.Component.MarkdownWithMathjax
import Lean

open ProofWidgets Jsx Lean Server

structure MyProps where
  name : Name
deriving RpcEncodable

syntax (name := exprGraphCmd) "#expr_graph" ident : command

open Elab Command in
@[command_elab exprGraphCmd]
def elabExprGraphCmd : CommandElab := fun
  | stx@`(#expr_graph $i:ident) => Elab.Command.runTermElabM  fun _ => do
    let env ← getEnv
    let some c := env.find? i.getId | throwError "ERROR"
    let nodes : NameSet := c.getUsedConstantsAsSet
    let mut edges : HashSet (Name × Name) := {}
    for a in nodes do for b in nodes do
      let some bb := env.find? b | continue
      if bb.getUsedConstantsAsSet.contains a then
        edges := edges.insert (a, b)
    let mut nodesWithInfos : Array Node := #[]
    for node in nodes.toArray do
      let some c := env.find? node | continue
      let doc? ← findDocString? env node
      let ee ← Lean.Widget.ppExprTagged c.type
      let us ← Meta.mkFreshLevelMVarsFor c
      let e ← Lean.Widget.ppExprTagged (.const node us)
      let newNode : Node := {
        id := s!"{hash node}"
        html := match doc? with
            | some d =>
              <div>
                <InteractiveCode fmt = {e} />{.text " : "}<InteractiveCode fmt = {ee} />
                <MarkdownWithMathjax markdown = {d} />
              </div>
            | none =>
              <div>
                <InteractiveCode fmt = {e} />{.text " : "}<InteractiveCode fmt = {ee} />
              </div>
      }
      nodesWithInfos := nodesWithInfos.push newNode
    let mut dot : String := "digraph{"
    for (a,b) in nodes.toArray.zip nodesWithInfos do
      dot := dot ++ s!" \"{b.id}\" [id=\"{b.id}\", label=\"{a}\"];\n"
    for (a,b) in edges do
      dot := dot ++ s!"  \"{hash a}\" -> \"{hash b}\";\n"
    dot := dot ++ "}"
    let defaultHtml : Html :=
      <MarkdownWithMathjax markdown={"# Click on one of the nodes!"}/>
    let html : Html := <InfoGraph
      nodes = {nodesWithInfos}
      dot = {dot}
      defaultHtml = {defaultHtml}
    />
    Widget.savePanelWidgetInfo
      (hash HtmlDisplayPanel.javascript)
      (return json% { html : $(← Server.rpcEncode html) })
      stx
  | stx => throwError "Unexpected syntax {stx}"

def a : Nat := 0

def b : Nat := 1

def foo (c : Nat) : Nat × Int := (a + b * c, a / b)

#expr_graph foo
