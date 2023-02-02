/-
 Copyright (c) 2021 Wojciech Nawrocki. All rights reserved.
 Released under Apache 2.0 license as described in the file LICENSE.
 Authors: Wojciech Nawrocki, Sebastian Ullrich
 -/
import Lean.Data.Json.FromToJson
import Lean.Parser
import Lean.PrettyPrinter.Delaborator.Basic
import Lean.Server.Rpc.Basic

import WidgetKit.Component.Basic

/-! We define a representation of HTML trees together with a JSX-like DSL for writing them. -/

namespace WidgetKit
open Lean Server

/-- A HTML tree which may contain widget components. -/
inductive Html where
  /-- An `element "tag" attrs children` represents `<tag {...attrs}>{...children}</tag>`. -/
  | element : String → Array (String × Json) → Array Html → Html
  /-- Raw HTML text.-/
  | text : String → Html
  /-- A `component Foo props children` represents `<Foo {...props}>{...children}</>`. -/
  -- TODO: The universe lift is unfortunate.
  | component {Props} [RpcEncodable Props] : Component Props → Props → Array Html → Html

/-- A variant of `Html` which lives in `Type`, at the cost of some type safety.

Unfortunately we cannot build `RpcEncodable Html` because `Html : Type 1` and `RpcEncodable` is not
universe-polymorphic. We can, however, do it for `EncodableHtml`. -/
inductive EncodableHtml where
  | element : String → Array (String × Json) → Array EncodableHtml → EncodableHtml
  | text : String → EncodableHtml
  | component : UInt64 → LazyEncodable Json → Array EncodableHtml → EncodableHtml
  deriving Inhabited

#mkrpcenc EncodableHtml

partial def EncodableHtml.ofHtml : Html → EncodableHtml
  | .element t as cs => element t as (cs.map ofHtml)
  | .text s => text s
  | @Html.component _ _ c ps cs => component (hash c.javascript) (rpcEncode ps) (cs.map ofHtml)

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

@[combinator_formatter WidgetKit.Jsx.jsxText]
def jsxText.formatter : Formatter :=
  Formatter.visitAtom `jsxText
@[combinator_parenthesizer WidgetKit.Jsx.jsxText]
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
  if n.getId != m.getId then
    Macro.throwErrorAt m s!"expected </{n.getId}>"
  let cs ← cs.mapM fun
    | `(jsxChild| $t:jsxText)    => `(Html.text $(quote <| getJsxText t))
    | `(jsxChild| { $t })        => return t
    | `(jsxChild| $e:jsxElement) => `(term| $e:jsxElement)
    | _                          => unreachable!
  let vs : Array (TSyntax `term) := vs.map fun
    | `(jsxAttrVal| $s:str) => s
    | `(jsxAttrVal| { $t:term }) => t
    | _ => unreachable!
  let tag := toString n.getId
  -- Uppercase tags are parsed as components
  if tag.get? 0 |>.filter (·.isUpper) |>.isSome then
    `(Html.component $n { $[$ns:ident := $vs],* } #[ $[$cs],* ])
  -- Lowercase tags are parsed as standard HTML
  else
    let ns := ns.map (quote <| toString ·.getId)
    `(Html.element $(quote tag) #[ $[($ns, $vs)],* ] #[ $[$cs],* ])


/-- Support for writing HTML trees directly, using XML-like angle bracket syntax. It work very
similarly to [JSX](https://reactjs.org/docs/introducing-jsx.html) in JavaScript. The syntax is
enabled using `open scoped WidgetKit.Jsx`.

Lowercase tags are interpreted as standard HTML whereas uppercase ones are expected to be
`WidgetKit.Component`s. -/
macro_rules
  | `(<$n:ident $[$ns:ident = $vs:jsxAttrVal]* />) => transformTag n n ns vs #[]
  | `(<$n:ident $[$ns:ident = $vs:jsxAttrVal]* >$cs*</$m>) => transformTag n m ns vs cs

open Lean Delaborator SubExpr

@[delab app.WidgetKit.Html.text]
def delabHtmlText : Delab := do
  withAppArg delab

@[delab app.WidgetKit.Html.element]
def delabHtmlElement : Delab := do
  let e ← getExpr
  -- `Html.element tag attrs children`
  let #[tag, _, _] := e.getAppArgs | failure

  let .lit (.strVal s) := tag | failure
  let tag := mkIdent s

  let attrs ← withAppFn (withAppArg delab)
  let `(term| #[ $[($as:str, $vs)],* ] ) := attrs | failure
  let attrs : Array (TSyntax `jsxAttr) ← as.zip vs |>.mapM fun (a, v) => do
    let attr := mkIdent a.getString
    `(jsxAttr| $attr:ident={ $v })

  let children ← withAppArg delab
  let `(term| #[ $cs,* ]) := children | failure
  let cs : Array (TSyntax `jsxChild) ← (@id (TSyntaxArray `term) cs).mapM fun c => do
    if let some s := c.raw.isStrLit? then
      let txt := mkNode `jsxText #[mkAtom s]
      `(jsxChild| $txt:jsxText)
    -- hack.
    else if c.raw[0].getKind == ``WidgetKit.Jsx.«jsxElement<__>_</_>» then
      `(jsxChild| $(⟨c.raw⟩):jsxElement)
    else
      `(jsxChild| { $c })
  `(term| < $tag $[$attrs]* > $[$cs]* </ $tag >)

end Jsx
end WidgetKit
