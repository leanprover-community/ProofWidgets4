import WidgetKit.HtmlWidget
open Lean.Widget.Jsx -- ⟵ remember this!

#html <b>You can use HTML in lean!</b>

open scoped Lean.Widget.Jsx in
theorem ghjk : True := by
  html! <b>What, HTML in Lean?! </b>
  html! <i>And another!</i>
  trivial

