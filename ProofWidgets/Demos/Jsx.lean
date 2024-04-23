import ProofWidgets.Component.HtmlDisplay

open scoped ProofWidgets.Jsx -- ⟵ remember this!

def x := <b>You can use HTML in lean! {.text <| toString <| 4 + 5} <hr/> </b>

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

-- interactive test: check that the hovers in the infoview on subexpressions are correct
#print x

/-- info: <span id={Lean.Json.str "greeting"}>Hello world</span> : ProofWidgets.Html -/
-- #guard_msgs in
#check <span id="greeting">Hello world</span>

/-- info: <span>Hello interpolated world</span> : ProofWidgets.Html -/
#guard_msgs in
#check <span>Hello {.text "interpolated"} world</span>

/-- info: <span>Hello {ProofWidgets.Html.text "<>"}world</span> : ProofWidgets.Html -/
#guard_msgs in
#check <span>Hello {.text "<>"} world</span>

/-- info: <hr/> : ProofWidgets.Html -/
#guard_msgs in
#check <hr />

end delaborator_tests
