import ProofWidgets.Component.HtmlDisplay

-- The `ProofWidgets.Jsx` namespace provides JSX-like notation for HTML
open scoped ProofWidgets.Jsx

-- Put your cursor over this
#html <b>What, HTML in Lean?!</b>

-- String-valued attributes can be written directly, or interpolated with `{ }`
#html <img src={"https://" ++ "upload.wikimedia.org/wikipedia/commons/a/a5/Parrot_montage.jpg"}
           alt="Six photos of parrots arranged in a grid." />

def htmlLetters : Array ProofWidgets.Html := #[
    <span style={json% {color: "red"}}>H</span>,
    <span style={json% {color: "yellow"}}>T</span>,
    <span style={json% {color: "green"}}>M</span>,
    <span style={json% {color: "blue"}}>L</span>
  ]

-- HTML children can be interpolated with `{ }` (single node) and `{... }` (multiple nodes)
def x := <b>You can use {...htmlLetters} in Lean {.text s!"{1 + 3}"}! <hr/> </b>
#html x

-- Use `MarkdownDisplay` to render Markdown
open ProofWidgets in
#html <MarkdownDisplay contents={"
  ## Hello, Markdown
  We have **bold text**, _italic text_, `example : True := by trivial`,
  and $3×19 = \\int\\limits_0^{57}1~dx$.
"} />

-- HTML trees can also be produced by metaprograms
open ProofWidgets Lean Server Elab in
#html (do
  let e ← Term.elabTerm (← ``(1 + 3)) (mkConst ``Nat)
  Term.synthesizeSyntheticMVarsNoPostponing
  let e ← instantiateMVars e
  return <InteractiveExpr expr={← WithRpcRef.mk (← ExprWithCtx.save e)} />
    : Term.TermElabM Html)
