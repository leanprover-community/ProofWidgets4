import ProofWidgets.Component.HtmlDisplay

open scoped ProofWidgets.Jsx -- ⟵ remember this!

def htmlLetters : Array ProofWidgets.Html :=
  #[
    <span style={json% {color: "red"}}>H</span>,
    <span style={json% {color: "yellow"}}>T</span>,
    <span style={json% {color: "green"}}>M</span>,
    <span style={json% {color: "blue"}}>L</span>
  ]

def x := <b>You can use {...htmlLetters} in Lean {.text s!"{1 + 3}"}! <hr/> </b>

-- Put your cursor over this
#html x

theorem ghjk : True := by
  -- Put your cursor over any of the `html!` lines
  html! <b>What, HTML in Lean?!</b>
  html! <i>And another!</i>
  -- attributes and text nodes can be interpolated
  html! <img src={"https://" ++ "upload.wikimedia.org/wikipedia/commons/a/a5/Parrot_montage.jpg"} alt="parrots" />
  trivial
