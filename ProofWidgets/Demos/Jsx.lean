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
