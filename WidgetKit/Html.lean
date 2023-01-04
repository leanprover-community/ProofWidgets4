/-
 Copyright (c) 2021 Wojciech Nawrocki. All rights reserved.
 Released under Apache 2.0 license as described in the file LICENSE.
 Authors: Wojciech Nawrocki, Sebastian Ullrich
 -/
import Lean.Data.Json.FromToJson
import Lean.Parser
import Lean.PrettyPrinter.Delaborator.Basic

/-! We define a representation of HTML trees together with a JSX-like DSL for writing them. -/

namespace WidgetKit
open Lean

inductive Html where
  /-- An `element tag attrs children` represents `<tag {...attrs}>{...children}</tag>`. -/
  | element : String → Array (String × Json) → Array Html → Html
  /-- Raw HTML text.-/
  | text : String → Html
  deriving BEq, Inhabited, FromJson, ToJson

instance : Coe String Html :=
  ⟨Html.text⟩

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

@[combinator_formatter WidgetKit.Jsx.jsxText]
def jsxText.formatter : Formatter :=
  Formatter.visitAtom `jsxText
@[combinator_parenthesizer WidgetKit.Jsx.jsxText]
def jsxText.parenthesizer : Parenthesizer :=
  Parenthesizer.visitToken

scoped syntax "<" ident jsxAttr* "/>" : jsxElement
scoped syntax "<" ident jsxAttr* ">" jsxChild* "</" ident ">" : jsxElement

scoped syntax jsxText      : jsxChild
scoped syntax "{" term "}" : jsxChild
scoped syntax jsxElement   : jsxChild

scoped syntax:max jsxElement : term

macro_rules
  | `(<$n:ident $[$ns:ident = $vs:jsxAttrVal]* />) =>
    let ns := ns.map (quote <| toString ·.getId)
    let vs : Array (TSyntax `term) := vs.map fun
      | `(jsxAttrVal| $s:str) => s
      | `(jsxAttrVal| { $t:term }) => t
      | _ => unreachable!
    `(Html.element $(quote <| toString n.getId) #[ $[($ns, toJson $vs)],* ] #[])
  | `(<$n:ident $[$ns:ident = $vs:jsxAttrVal]* >$cs*</$m>) =>
    if n.getId == m.getId then do
      let ns := ns.map (quote <| toString ·.getId)
      let vs : Array (TSyntax `term) := vs.map fun
        | `(jsxAttrVal| $s:str) => s
        | `(jsxAttrVal| { $t:term }) => t
        | _ => unreachable!
      let cs ← cs.mapM fun
        | `(jsxChild|$t:jsxText)    => `(Html.text $(quote t.raw[0].getAtomVal))
        -- TODO(WN): elab as list of children if type is `t Html` where `Foldable t`
        | `(jsxChild|{$t})          => return t
        | `(jsxChild|$e:jsxElement) => `(term| $e:jsxElement)
        | _                         => unreachable!
      let tag := toString n.getId
      `(Html.element $(quote tag) #[ $[($ns, toJson $vs)],* ] #[ $[$cs],* ])
    else Macro.throwErrorAt m ("expected </" ++ toString n.getId ++ ">")

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