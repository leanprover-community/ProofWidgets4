import ProofWidgets.Component.HtmlDisplay

open scoped ProofWidgets.Jsx -- ⟵ remember this!

def x := <b>You can use HTML in lean! {.text <| toString <| 4 + 5} <hr/> </b>

#html x

theorem ghjk : True := by
  html! <b>What, HTML in Lean?! </b>
  html! <i>And another!</i>
  html! <img src="https://upload.wikimedia.org/wikipedia/commons/a/a5/Parrot_montage.jpg"/>
  trivial
