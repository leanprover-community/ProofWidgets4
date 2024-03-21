/-
 Copyright (c) 2021-2023 Wojciech Nawrocki. All rights reserved.
 Released under Apache 2.0 license as described in the file LICENSE.
 Authors: Wojciech Nawrocki, Sebastian Ullrich
 -/
import Lean.Data.Json.FromToJson
import Lean.Parser
import Lean.PrettyPrinter.Delaborator.Basic
import Lean.Server.Rpc.Basic

import ProofWidgets.Component.Basic

/-! We define a representation of HTML trees together with a JSX-like DSL for writing them. -/

namespace ProofWidgets
open Lean Server

/-- A HTML tree which may contain widget components. -/
inductive Html where
  /-- An `element "tag" attrs children` represents `<tag {...attrs}>{...children}</tag>`. -/
  | element : String → Array (String × Json) → Array Html → Html
  /-- Raw HTML text. -/
  | text : String → Html
  /-- A `component h e props children` represents `<Foo {...props}>{...children}</Foo>`,
  where `Foo : Component Props` is some component such that `h = hash Foo.javascript`,
  `e = Foo.«export»`, and `props` will produce a JSON-encoded value of type `Props`. -/
  | component : UInt64 → String → LazyEncodable Json → Array Html → Html
  deriving Inhabited, RpcEncodable

def Html.ofComponent [RpcEncodable Props]
    (c : Component Props) (props : Props) (children : Array Html) : Html :=
  .component (hash c.javascript) c.export (rpcEncode props) children

/-- See [MDN docs](https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Flow_Layout/Block_and_Inline_Layout_in_Normal_Flow). -/
inductive LayoutKind where
  | block
  | inline

namespace Jsx
open Parser PrettyPrinter

declare_syntax_cat jsxElement
declare_syntax_cat jsxChild
declare_syntax_cat jsxAttr
declare_syntax_cat jsxAttrVal

-- See https://leanprover.zulipchat.com/#narrow/stream/270676-lean4/topic/expected.20parser.20to.20return.20exactly.20one.20syntax.20object
-- def jsxAttrVal : Parser := strLit <|> group ("{" >> termParser >> "}")
scoped syntax str : jsxAttrVal
scoped syntax group("{" term "}") : jsxAttrVal
scoped syntax ident "=" jsxAttrVal : jsxAttr

-- JSXTextCharacter : SourceCharacter but not one of {, <, > or }
def jsxText : Parser :=
  withAntiquot (mkAntiquot "jsxText" `jsxText) {
    fn := fun c s =>
      let startPos := s.pos
      let s := takeWhile1Fn (not ∘ "{<>}$".contains) "expected JSX text" c s
      mkNodeToken `jsxText startPos c s }

def getJsxText : TSyntax `jsxText → String
  | stx => stx.raw[0].getAtomVal

@[combinator_formatter ProofWidgets.Jsx.jsxText]
def jsxText.formatter : Formatter :=
  Formatter.visitAtom `jsxText
@[combinator_parenthesizer ProofWidgets.Jsx.jsxText]
def jsxText.parenthesizer : Parenthesizer :=
  Parenthesizer.visitToken

scoped syntax "<" ident jsxAttr* "/>" : jsxElement
scoped syntax "<" ident jsxAttr* ">" jsxChild* "</" ident ">" : jsxElement

scoped syntax jsxText      : jsxChild
-- TODO(WN): expand `{... $t}` as list of children
scoped syntax "{" term "}" : jsxChild
scoped syntax jsxElement   : jsxChild

scoped syntax:max jsxElement : term

def transformTag (n m : Ident) (ns : Array Ident) (vs : Array (TSyntax `jsxAttrVal))
    (cs : Array (TSyntax `jsxChild)) : MacroM Term := do
  let nId := n.getId.eraseMacroScopes
  let mId := m.getId.eraseMacroScopes
  if nId != mId then
    Macro.throwErrorAt m s!"expected </{nId}>"
  let cs ← cs.mapM fun
    | `(jsxChild| $t:jsxText)    => `(Html.text $(quote <| getJsxText t))
    | `(jsxChild| { $t })        => return t
    | `(jsxChild| $e:jsxElement) => `(term| $e:jsxElement)
    | stx                        => Macro.throwErrorAt stx "unknown syntax"
  let vs : Array (TSyntax `term) ← vs.mapM fun
    | `(jsxAttrVal| $s:str)      => pure s
    | `(jsxAttrVal| { $t:term }) => pure t
    | stx                        => Macro.throwErrorAt stx "unknown syntax"
  let tag := toString nId
  -- Uppercase tags are parsed as components
  if tag.get? 0 |>.filter (·.isUpper) |>.isSome then
    `(Html.ofComponent $n { $[$ns:ident := $vs],* } #[ $[$cs],* ])
  -- Lowercase tags are parsed as standard HTML
  else
    let ns := ns.map (quote <| toString ·.getId)
    `(Html.element $(quote tag) #[ $[($ns, $vs)],* ] #[ $[$cs],* ])

/-- Support for writing HTML trees directly, using XML-like angle bracket syntax. It works very
similarly to [JSX](https://react.dev/learn/writing-markup-with-jsx) in JavaScript. The syntax is
enabled using `open scoped ProofWidgets.Jsx`.

Lowercase tags are interpreted as standard HTML whereas uppercase ones are expected to be
`ProofWidgets.Component`s. -/
macro_rules
  | `(<$n:ident $[$attrs:ident = $vs:jsxAttrVal]* />) => transformTag n n attrs vs #[]
  | `(<$n:ident $[$attrs:ident = $vs:jsxAttrVal]* >$cs*</$m>) => transformTag n m attrs vs cs

open Lean Delaborator SubExpr

def delabJsxChildren : DelabM (Array (TSyntax `jsxChild)) := do
  let children ← delab
  let `(term| #[ $cs,* ]) := children | failure
  (@id (TSyntaxArray `term) cs).mapM fun c => do
    if let `(term| Html.text $s:str) := c.raw then
      let txt := mkNode `jsxText #[mkAtom s.getString]
      `(jsxChild| $txt:jsxText)
    -- hack.
    else if c.raw[0].getKind == ``ProofWidgets.Jsx.«jsxElement<__>_</_>» ||
            c.raw[0].getKind == ``ProofWidgets.Jsx.«jsxElement<__/>» then
      `(jsxChild| $(⟨c.raw⟩):jsxElement)
    else
      `(jsxChild| { $c })

@[delab app.ProofWidgets.Html.element]
def delabHtmlElement : Delab := do
  let e ← getExpr
  -- `Html.element tag attrs children`
  let #[tag, _, _] := e.getAppArgs | failure

  let .lit (.strVal s) := tag | failure
  let tag := mkIdent (.mkSimple s)

  let attrs ← withAppFn (withAppArg delab)
  let `(term| #[ $[($as:str, $vs)],* ] ) := attrs | failure
  let attrs : Array (TSyntax `jsxAttr) ← as.zip vs |>.mapM fun (a, v) => do
    let attr := mkIdent (.mkSimple a.getString)
    `(jsxAttr| $attr:ident={ $v })

  let children ← withAppArg delabJsxChildren
  if children.isEmpty then
    `(term| < $tag $[$attrs]* />)
  else
    `(term| < $tag $[$attrs]* > $[$children]* </ $tag >)

@[delab app.ProofWidgets.Html.ofComponent]
def delabHtmlOfComponent : Delab := do
  -- `Html.ofComponent Props inst c props children`
  let c ← withNaryArg 2 delab
  unless c.raw.isIdent do failure
  let tag : Ident := ⟨c.raw⟩

  let props ← withNaryArg 3 delab
  let `(term| { $[$ns:ident := $vs],* } ) := props | failure
  let attrs : Array (TSyntax `jsxAttr) ← ns.zip vs |>.mapM fun (n, v) => do
    `(jsxAttr| $n:ident={ $v })

  let children ← withNaryArg 4 delabJsxChildren
  if children.isEmpty then
    `(term| < $tag $[$attrs]* />)
  else
    `(term| < $tag $[$attrs]* > $[$children]* </ $tag >)

end Jsx
end ProofWidgets
