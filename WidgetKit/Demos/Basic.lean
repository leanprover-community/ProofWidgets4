import WidgetKit.HtmlWidget
open Lean.Widget.Jsx -- ⟵ remember this!

def x := <b>You can use HTML in lean! {toString $ 4 + 5} <hr/> </b>

#html x

open scoped Lean.Widget.Jsx in
theorem ghjk : True := by
  html! <b>What, HTML in Lean?! </b>
  html! <i>And another!</i>
  trivial

