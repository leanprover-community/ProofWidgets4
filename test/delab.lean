import ProofWidgets.Data.Html

open ProofWidgets.Html Lean


/-- info: <span id="greeting">Hello world</span> : ProofWidgets.Html -/
#guard_msgs in
#check jsx%{<span id="greeting">Hello world</span>}

/-- info: <span>Hello interpolated world</span> : ProofWidgets.Html -/
#guard_msgs in
#check jsx%{<span>Hello {.text "interpolated"} world</span>}

/-- info: <span>Hello {text "<>"} world</span> : ProofWidgets.Html -/
#guard_msgs in
#check jsx%{<span>Hello {.text "<>"} world</span>}

/-- info: <hr/> : ProofWidgets.Html -/
#guard_msgs in
#check jsx%{<hr />}

variable (attrs children)
/-- info: <div {...attrs}>{...children}</div> : ProofWidgets.Html -/
#guard_msgs in
#check element "div" attrs children

/-- info: <div {...attrs}>{...children}</div> : ProofWidgets.Html -/
#guard_msgs in
#check jsx%{<div {...attrs}>{...children}</div>}

/-- error: expected at most one HTML element, found a `{... }` interpolation -/
#guard_msgs in
#check jsx%{{...children}}

structure CustomProps where
  val : Nat
  str : String
  deriving Server.RpcEncodable

def CustomComponent : ProofWidgets.Component CustomProps where
  javascript := ""

-- TODO: spacing between attributes
/-- info: <div><CustomComponent val={2}str={"3"}>Content</CustomComponent></div> : ProofWidgets.Html -/
#guard_msgs in
#check jsx%{<div><CustomComponent val={2} str="3">Content</CustomComponent></div>}

def ProdComponent : ProofWidgets.Component (Nat × Nat) where
  javascript := ""

/-- info: <div><ProdComponent {...(1, 2)}/></div> : ProofWidgets.Html -/
#guard_msgs in
#check jsx%{<div><ProdComponent fst={1} snd={2} /></div>}
/-- info: <div><ProdComponent {...(1, 2)}/></div> : ProofWidgets.Html -/
#guard_msgs in
#check jsx%{<div><ProdComponent {...Prod.mk 1 2}/></div>}
/--
info: <div><ProdComponent {...let __src := (1, 2);
          (__src.fst, 3)}/></div> : ProofWidgets.Html
-/
#guard_msgs in
#check jsx%{<div><ProdComponent {...Prod.mk 1 2} snd={3}/></div>}

-- interactive test: check that the hovers in the infoview on subexpressions are correct
#check jsx%{<span id="test">Hello {.text "<>"} world<CustomComponent val={1} str="3" /></span>}
