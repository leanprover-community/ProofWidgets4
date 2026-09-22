/-
Copyright (c) 2026 Lean FRO, LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Wojciech Nawrocki
-/
module

public meta import Lean.Elab.Term
public meta import Lean.Data.Html.Syntax
public meta import Lean.Data.Html.Elab
public import ProofWidgets.Util
import ProofWidgets.Data.Html.Basic

set_option doc.verso true

meta section

/-! Elaborators for writing {name}`ProofWidgets.Html` trees
using the HTML-like syntax defined in {lit}`Lean.Data.Html.Syntax`. -/

namespace ProofWidgets
open Util
open Lean Parser Html Syntax Elab Term

/-- Elaborates a single attribute into {lit}`.inl pair`,
where {lit}`pair : String × Json`,
and a sequence of attributes into {lit}`.inr pairs`,
where {lit}`pairs : Array (String × Json)`. -/
def elabHtmlAttr (stx : Attr) : TermElabM (Term ⊕ Term) := withRef stx do
  match ← stx.view with
  | .val { name, val, .. } =>
    let name ← name.view
    let val : Term ← match ← val.view with
      | .str s => pure <| quote (← decodeCharacterReferences s)
      | .interp stx => pure (← stx.view).term
    Sum.inl <$> `(($(quote name), ($val : Json)))
  | .bool name =>
    let name ← name.view
    -- The value is `true` rather than the empty string of the HTML standard
    -- because React expects boolean attributes such as `disabled` to store booleans.
    Sum.inl <$> `(($(quote name), Json.bool true))
  | .interp false stx =>
    let i ← stx.view
    Sum.inl <$> `(($(i.term) : String × Json))
  | .interp true stx =>
    let i ← stx.view
    Sum.inr <$> `(($(i.term) : Array (String × Json)))

/-- Elaborates a {lit}`name=val` attribute into {lit}`.inl (name, value)`,
and an interpolation {lit}`{... t}` into {lit}`.inr t`.
Throws on single-element interpolations. -/
def elabComponentAttr (stx : Attr) : TermElabM ((Ident × Term) ⊕ Term) := withRef stx do
  match ← stx.view with
  | .val { name, val, .. } =>
    let field := mkIdentFrom name (← name.view).toName
    let val : Term ← match ← val.view with
      | .str s => pure <| quote (← decodeCharacterReferences s)
      | .interp stx => pure (← stx.view).term
    return .inl (field, val)
  | .bool name =>
    let field := mkIdentFrom name (← name.view).toName
    return .inl (field, ← `(true))
  | .interp true stx =>
    return .inr (← stx.view).term
  | .interp false stx =>
    throwErrorAt stx
      "single attribute interpolation is not supported in ProofWidgets components; \
       use `\{... props}` instead"

mutual

/-- Elaborates an HTML element into a {name}`Term` of type {name}`Html`. -/
partial def elabHtmlElement (stx : Element) : TermElabM Term := withRef stx do
  stx.checkNamesMatch
  let v ← stx.view
  let tagName ← v.startTag.name.view
  let children ← match v.children? with
    | some cs => elabHtmlContent cs
    | none => pure #[]
  let children ← joinArrays <| ← foldInlsM children fun hs => `(#[$hs,*])

  -- Uppercase tags are parsed as ProofWidgets components.
  if String.Pos.Raw.get! tagName 0 |>.isUpper then
    let cId := mkIdentFrom v.startTag.name tagName.toName (canonical := true)
    let attrs ← v.startTag.attrs.mapM elabComponentAttr
    let withs := attrs.filterMap fun
      | .inr props => some props
      | .inl _ => none
    let fields ← attrs.filterMapM fun
      | .inl (field, val) => some <$> `(Term.structInstField| $field:ident := $val)
      | .inr _ => pure none
    let props ← match withs, fields with
      | #[w], #[] => pure w
      | _, _ => `({ $withs,* with $fields:structInstField,* })
    `(Html.ofComponent $cId $props $children)
  -- Lowercase tags are parsed as HTML elements.
  else
    stx.checkNoVoidChildren
    let attrs ← v.startTag.attrs.mapM elabHtmlAttr
    let attrs ← joinArrays <| ← foldInlsM attrs fun pairs => `(#[$pairs,*])
    `(Html.element $(quote tagName) $attrs $children)

/-- Elaborates a sequence of HTML content items.
- Many-item interpolations {lit}`{... $hs }` become {lit}`.inr hs`,
  with expected type {lit}`hs : Array Html`.
- Other items become {lit}`.inl h`, with expected type {lit}`h : Html`. -/
partial def elabHtmlContent (stx : Content) : TermElabM (Array (Term ⊕ Term)) := do
  let mut out := #[]
  for item in ← stx.view do
    match item with
    | .element stx =>
      out := out.push (.inl (← elabHtmlElement stx))
    | .textComments tcs => withRef tcs.getSyntax do←
      let s ← tcs.getText
      unless s.isEmpty do
        out := out.push (.inl (← ``(Html.text $(quote s))))
    | .interp false stx =>
      let i ← stx.view
      out := out.push (.inl i.term)
    | .interp true stx => withRef stx do←
      let i ← stx.view
      out := out.push (.inr (← `(($(i.term) : Array Html))))
  return out

end

/--
A {name}`ProofWidgets.Html` literal written using HTML-like syntax.

The HTML-like syntax is defined in core Lean ({lit}`Lean.Data.Html.Syntax`).
The following behavior is specific to ProofWidgets:
- Lowercase tags are interpreted as standard HTML elements,
  whereas uppercase tags are expected to be {name}`ProofWidgets.Component`s.
- Attributes on an HTML element have type {lean}`String × Json` (name and value)
  rather than {lean}`String × String`.
- Attributes on a widget component of type
  {given -show}`Props : Type` {lean}`ProofWidgets.Component Props`
  are the fields of {lean}`Props`.
  They can be specified as interpolated values {lit}`<Widget name={val}/>`
  and whole structures {lit}`<Widget {... { name := val }}/>`.
  Interpolation of single attributes with {lit}`<Widget {("name", val)}/>`
  is not supported on components.
- At most one node is allowed between the braces, and this must be known statically,
  so e.g. `jsx%{<br/><br/>}` and `jsx%{{... hs}}` are forbidden.
-/
@[term_parser]
public meta def «jsx%» : Parser :=
  leading_parser "jsx%" >> rawSymbol "{" >> Lean.Html.Syntax.content >> "}"

elab_rules : term
  | `(term| jsx%{$c:content}) => do
    match ← elabHtmlContent c with
    | #[] => elabTermEnsuringType (← ``(Html.text "")) (some (.const ``Html []))
    | #[.inl h] => elabTermEnsuringType h (some (.const ``Html []))
    | #[.inr _] =>
      throwErrorAt c "expected at most one HTML element, found a `\{... }` interpolation"
    | out => throwErrorAt c m!"expected at most one HTML element, found {out.size}"

-- TODO: delaborators

end ProofWidgets
