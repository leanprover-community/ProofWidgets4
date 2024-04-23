import ProofWidgets.Component.HtmlDisplay

open scoped ProofWidgets.Jsx -- ⟵ remember this!

def x := <b «class»={1}>You can use HTML in lean! {.text <| toString <| 4 + 5} <hr/> </b>

-- Put your cursor over this
#html x

theorem ghjk : True := by
  -- Put your cursor over any of the `html!` lines
  html! <b>What, HTML in Lean?! </b>
  html! <i>And another!</i>
  -- attributes and text nodes can be interpolated
  html! <img src={ "https://" ++ "upload.wikimedia.org/wikipedia/commons/a/a5/Parrot_montage.jpg"} alt="parrots" />
  trivial

section delaborator_tests

open ProofWidgets.Html Lean

-- interactive test: check that the hovers in the infoview on subexpressions are correct
#print x


/-- info: <span id={Json.str "greeting"}>Hello world</span> : ProofWidgets.Html -/
#guard_msgs in
#check <span id="greeting">Hello world</span>

/-- info: <span>Hello interpolated world</span> : ProofWidgets.Html -/
#guard_msgs in
#check <span>Hello {.text "interpolated"} world</span>

/-- info: <span>Hello {text "<>"}world</span> : ProofWidgets.Html -/
#guard_msgs in
#check <span>Hello {.text "<>"} world</span>

/-- info: <hr/> : ProofWidgets.Html -/
#guard_msgs in
#check <hr />

structure CustomProps where
  val : Nat
  deriving Server.RpcEncodable

def CustomComponent : ProofWidgets.Component CustomProps where
  javascript := ""

/-- info: <div><CustomComponent val={2}>Content</CustomComponent></div> : ProofWidgets.Html -/
#guard_msgs in
#check <div><CustomComponent val={2}>Content</CustomComponent></div>

def ProdComponent : ProofWidgets.Component (Nat × Nat) where
  javascript := ""

-- TODO: fix this
/-- info: <div>{ofComponent ProdComponent (1, 2) #[]}</div> : ProofWidgets.Html -/
#guard_msgs in
#check <div><ProdComponent fst={1} snd={2} /></div>

end delaborator_tests
