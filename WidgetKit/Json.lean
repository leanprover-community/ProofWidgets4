/-
 Copyright (c) 2022 E.W.Ayers. All rights reserved.
 Released under Apache 2.0 license as described in the file LICENSE.
 Authors: E.W.Ayers
-/
import Lean.Data.Json
import Lean.Meta.Basic
import Lean.Elab.Term
import Lean.Elab.Eval

/-!
# Json-like syntax for Lean.

Now you can write

```lean
#eval json% {
  hello : "world",
  cheese : ["edam", "cheddar", {kind : "spicy", rank : 100.2}],
  lemonCount : 100e30,
  isCool : true,
  isBug : null,
  lookACalc: $(23 + 54 * 2)
}
```
 -/

open Lean Parser
declare_syntax_cat jso
declare_syntax_cat jso_field
declare_syntax_cat jso_ident

instance : OfScientific JsonNumber where
  ofScientific mantissa exponentSign decimalExponent :=
    if exponentSign then
      {mantissa := mantissa, exponent := decimalExponent}
    else
      {mantissa := (mantissa * 10 ^ decimalExponent : Nat), exponent := 0}

instance : Neg JsonNumber where
  neg jn := ⟨- jn.mantissa, jn.exponent⟩

instance : ToJson Float where
  toJson x :=
    let s :=  toString (if x < 0.0 then - x else x)
    match Lean.Syntax.decodeScientificLitVal? <| s with
    | none =>
      match s with
      | "inf" => "inf" -- [todo] emit a warning
      | "-inf" => "-inf"
      | "nan" => "nan"
      | _ => panic! s!"unhandled float string {s}"
    | some (m, e, de) =>
      let j : JsonNumber := OfScientific.ofScientific m e de
      let j := if x < 0.0 then -j else j
      Json.num j

syntax "[" jso,* "]" : jso
syntax "-"? scientific : jso
syntax "-"? num : jso
syntax str : jso
syntax "true" : jso
syntax "false" : jso
syntax "null" : jso
syntax ident : jso_ident
syntax "$(" term ")" : jso_ident
syntax str : jso_ident
syntax jso_ident ": " jso : jso_field
syntax "{" jso_field,* "}" : jso
syntax "$(" term ")" : jso
syntax "json% " jso  : term

macro_rules
  | `(json% $($t))          => `(Lean.toJson $t)
  | `(json% null)           => `(Lean.Json.null)
  | `(json% true)           => `(Lean.Json.bool Bool.true)
  | `(json% false)          => `(Lean.Json.bool Bool.false)
  | `(json% $n:str)         => `(Lean.Json.str $n)
  | `(json% $n:num)         => `(Lean.Json.num $n)
  | `(json% $n:scientific)  => `(Lean.Json.num $n)
  | `(json% -$n:num)        => `(Lean.Json.num (-$n))
  | `(json% -$n:scientific) => `(Lean.Json.num (-$n))
  | `(json% [$[$xs],*])     => `(Lean.Json.arr #[$[json% $xs],*])
  | `(json% {$[$ks:jso_ident : $vs:jso],*}) =>
    let ks : Array (TSyntax `term) := ks.map fun
      | `(jso_ident| $k:ident)   => (k.getId |> toString |> quote)
      | `(jso_ident| $k:str)     => k
      | `(jso_ident| $($k:term)) => k
      | stx                      => panic! s!"unrecognized ident syntax {stx}"
    `(Lean.Json.mkObj [$[($ks, json% $vs)],*])

open Lean Elab Meta Term in
unsafe def Lean.evalJsonUnsafe (stx : Syntax) : TermElabM Json := do
  let JsonT := mkConst ``Json
  let jsonStx : TSyntax `jso := TSyntax.mk stx
  Elab.Term.evalTerm Json JsonT (← `(json% $jsonStx))

open Lean Elab in
@[implemented_by evalJsonUnsafe]
opaque Lean.evalJson : Syntax → TermElabM Json
