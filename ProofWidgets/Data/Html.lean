/-
 Copyright (c) 2021-2023 Wojciech Nawrocki. All rights reserved.
 Released under Apache 2.0 license as described in the file LICENSE.
 Authors: Wojciech Nawrocki, Sebastian Ullrich, Eric Wieser
 -/
module

public meta import Lean.Data.Json.FromToJson
public meta import Lean.Parser
public meta import Lean.PrettyPrinter.Delaborator.Basic
public meta import Lean.Server.Rpc.Basic

public meta import ProofWidgets.Component.Basic
public meta import ProofWidgets.Util

public meta section

/-! We define a representation of HTML trees together with a JSX-like DSL for writing them. -/

namespace ProofWidgets
open Lean Server ProofWidgets.Util

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

scoped syntax str : jsxAttrVal
/-- Interpolates an expression into a JSX attribute literal. -/
scoped syntax group("{" term "}") : jsxAttrVal
scoped syntax ident "=" jsxAttrVal : jsxAttr
/-- Interpolates a collection into a JSX attribute literal.
For HTML tags, this should have type `Array (String × Json)`.
For `ProofWidgets.Component`s, it can be any structure `$t` whatsoever:
it is interpolated into the component's props using `{ $t with ... }` notation. -/
scoped syntax group(" {..." term "}") : jsxAttr

/-- Characters not allowed inside JSX plain text. -/
def jsxTextForbidden : String := "{<>}$"
/-- A plain text literal for JSX (notation for `Html.text`). -/
def jsxText : Parser :=
  withAntiquot (mkAntiquot "jsxText" `ProofWidgets.Jsx.jsxText) {
    fn := fun c s =>
      let startPos := s.pos
      let s := takeWhile1Fn (not ∘ jsxTextForbidden.contains) "expected JSX text" c s
      mkNodeToken `ProofWidgets.Jsx.jsxText startPos true c s }

def getJsxText : TSyntax ``jsxText → String
  | stx => stx.raw[0].getAtomVal

@[combinator_formatter ProofWidgets.Jsx.jsxText]
def jsxText.formatter : Formatter :=
  Formatter.visitAtom ``jsxText
@[combinator_parenthesizer ProofWidgets.Jsx.jsxText]
def jsxText.parenthesizer : Parenthesizer :=
  Parenthesizer.visitToken

scoped syntax "<" ident jsxAttr* "/>" : jsxElement
scoped syntax "<" ident jsxAttr* ">" jsxChild* "</" ident ">" : jsxElement

scoped syntax jsxText      : jsxChild
/-- Interpolates an array of elements into a JSX literal -/
scoped syntax "{..." term "}" : jsxChild
/-- Interpolates an expression into a JSX literal -/
scoped syntax "{" term "}" : jsxChild
scoped syntax jsxElement   : jsxChild

scoped syntax:max jsxElement : term

def transformTag (tk : Syntax) (n m : Ident) (vs : Array (TSyntax `jsxAttr))
    (cs : Array (TSyntax `jsxChild)) : MacroM Term := do
  let nId := n.getId.eraseMacroScopes
  let mId := m.getId.eraseMacroScopes
  if nId != mId then
    Macro.throwErrorAt m s!"expected </{nId}>"

  let trailingWs (stx : Syntax) :=
    if let .original _ _ trailing _ := stx.getTailInfo then
      trailing.toString
    else ""

  -- Whitespace appearing before the current child.
  let mut wsBefore := trailingWs tk
  -- This loop transforms (for example) `` `(jsxChild*| {a} text {...cs} {d})``
  -- into ``children ← `(term| #[a, Html.text " text "] ++ cs ++ #[d])``.
  let mut csArrs := #[]
  let mut csArr := #[]
  for c in cs do
    match c with
    | `(jsxChild| $t:jsxText) =>
      csArr ← csArr.push <$> `(Html.text $(quote <| wsBefore ++ getJsxText t))
      wsBefore := ""
    | `(jsxChild| { $t }%$tk) =>
      csArr := csArr.push t
      wsBefore := trailingWs tk
    | `(jsxChild| $e:jsxElement) =>
      csArr ← csArr.push <$> `(term| $e:jsxElement)
      wsBefore := trailingWs e
    | `(jsxChild| {... $t }%$tk) =>
      if !csArr.isEmpty then
        csArrs ← csArrs.push <$> `(term| #[$csArr,*])
      csArr := #[]
      csArrs := csArrs.push t
      wsBefore := trailingWs tk
    | stx => Macro.throwErrorAt stx "unknown syntax"
  if !csArr.isEmpty then
    csArrs ← csArrs.push <$> `(term| #[$csArr,*])
  let children ← joinArrays csArrs

  let vs : Array ((Ident × Term) ⊕ Term) ← vs.mapM fun
    | `(jsxAttr| $attr:ident = $s:str)      => Sum.inl <$> pure (attr, s)
    | `(jsxAttr| $attr:ident = { $t:term }) => Sum.inl <$> pure (attr, t)
    | `(jsxAttr| {... $t:term })            => Sum.inr <$> pure t
    | stx                                   => Macro.throwErrorAt stx "unknown syntax"
  let tag := toString nId

  -- Uppercase tags are parsed as components
  if tag.get? 0 |>.filter (·.isUpper) |>.isSome then
    let withs : Array Term ← vs.filterMapM fun
      | .inr e => return some e
      | .inl _ => return none
    let vs ← vs.filterMapM fun
      | .inl (attr, val) => return some <|
        ← `(Term.structInstField| $attr:ident := $val)
      | .inr _ => return none
    let props ← match withs, vs with
      | #[w], #[] => pure w
      | _, _ => `({ $withs,* with $vs:structInstField,* })
    `(Html.ofComponent $n $props $children)
  -- Lowercase tags are parsed as standard HTML
  else
    let vs ← joinArrays <| ← foldInlsM vs (fun vs' => do
      let vs' ← vs'.mapM (fun (k, v) =>
        `(term| ($(quote <| toString k.getId), ($v : Json))))
      `(term| #[$vs',*]))
    `(Html.element $(quote tag) $vs $children)

/-- Support for writing HTML trees directly, using XML-like angle bracket syntax. It works very
similarly to [JSX](https://react.dev/learn/writing-markup-with-jsx) in JavaScript. The syntax is
enabled using `open scoped ProofWidgets.Jsx`.

Lowercase tags are interpreted as standard HTML
whereas uppercase ones are expected to be `ProofWidgets.Component`s. -/
macro_rules
  | `(<$n:ident $[$attrs:jsxAttr]* />%$tk) => transformTag tk n n attrs #[]
  | `(<$n:ident $[$attrs:jsxAttr]* >%$tk $cs*</$m>) => transformTag tk n m attrs cs

section delaborator

open Lean Delaborator SubExpr

/-! First delaborate into our non-term `TSyntax`. Note this means we can't call `delab`,
so we have to add the term annotations ourselves. -/

partial def delabHtmlText : DelabM (TSyntax ``jsxText) := do
  let_expr Html.text e := ← getExpr | failure
  let .lit (.strVal s) := e | failure
  if s.any jsxTextForbidden.contains then
    failure
  annotateTermLikeInfo <| mkNode ``jsxText #[mkAtom s]

mutual

partial def delabHtmlElement' : DelabM (TSyntax `jsxElement) := do
  let_expr Html.element tag _attrs _children := ← getExpr | failure

  let .lit (.strVal s) := tag | failure
  let tag ← withNaryArg 0 <| annotateTermLikeInfo <| mkIdent <| .mkSimple s

  let attrs ← withNaryArg 1 <|
    try
      delabArrayLiteral <| withAnnotateTermLikeInfo do
        let_expr Prod.mk _ _ a _ := ← getExpr | failure
        let .lit (.strVal a) := a | failure
        let attr ← withNaryArg 2 <| annotateTermLikeInfo <| mkIdent <| .mkSimple a
        withNaryArg 3 do
          let v ← getExpr
          -- If the attribute's value is a string literal,
          -- use `attr="val"` syntax.
          -- TODO: also do this for `.ofComponent`.
          -- WN: not sure if matching a string literal is possible with `let_expr`.
          match v with
          | .app (.const ``Json.str _) (.lit (.strVal v)) =>
            -- TODO: this annotation doesn't seem to work in infoview
            let val ← annotateTermLikeInfo <| Syntax.mkStrLit v
            `(jsxAttr| $attr:ident=$val:str)
          | _ =>
            let val ← delab
            `(jsxAttr| $attr:ident={ $val })
    catch _ =>
      let vs ← delab
      return #[← `(jsxAttr| {... $vs })]

  let children ← withAppArg delabJsxChildren
  if children.isEmpty then
    `(jsxElement| < $tag $[$attrs]* />)
  else
    `(jsxElement| < $tag $[$attrs]* > $[$children]* </ $tag >)

partial def delabHtmlOfComponent' : DelabM (TSyntax `jsxElement) := do
  let_expr Html.ofComponent _Props _inst _c _props _children := ← getExpr | failure
  let c ← withNaryArg 2 delab
  unless c.raw.isIdent do failure
  let tag : Ident := ⟨c.raw⟩

  -- TODO: handle `Props` that do not delaborate to `{ }`, such as `Prod`, by parsing the `Expr`
  -- instead.
  let attrDelab ← withNaryArg 3 delab
  let attrs : Array (TSyntax `jsxAttr) ← do
    let `(term| { $[$ns:ident := $vs],* } ) := attrDelab |
      pure #[← `(jsxAttr| {...$attrDelab})]
    ns.zip vs |>.mapM fun (n, v) => do
      `(jsxAttr| $n:ident={ $v })
  let children ← withNaryArg 4 delabJsxChildren
  if children.isEmpty then
    `(jsxElement| < $tag $[$attrs]* />)
  else
    `(jsxElement| < $tag $[$attrs]* > $[$children]* </ $tag >)

partial def delabJsxChildren : DelabM (Array (TSyntax `jsxChild)) := do
  try
    delabArrayLiteral (withAnnotateTermLikeInfo do
      try
        match_expr ← getExpr with
        | Html.text _ =>
          let html ← delabHtmlText
          return ← `(jsxChild| $html:jsxText)
        | Html.element _ _ _ =>
          let html ← delabHtmlElement'
          return ← `(jsxChild| $html:jsxElement)
        | Html.ofComponent _ _ _ _ _ =>
          let comp ← delabHtmlOfComponent'
          return ← `(jsxChild| $comp:jsxElement)
        | _ => failure
      catch _ =>
        let fallback ← delab
        return ← `(jsxChild| { $fallback }))
  catch _ =>
    let vs ← delab
    return #[← `(jsxChild| {... $vs })]

end

/-! Now wrap our `TSyntax _` delaborators into `Term` elaborators. -/

@[delab app.ProofWidgets.Html.element]
def delabHtmlElement : Delab := do
  let t ← delabHtmlElement'
  `(term| $t:jsxElement)

@[delab app.ProofWidgets.Html.ofComponent]
def delabHtmlOfComponent : Delab := do
  let t ← delabHtmlOfComponent'
  `(term| $t:jsxElement)

end delaborator

end Jsx
end ProofWidgets
