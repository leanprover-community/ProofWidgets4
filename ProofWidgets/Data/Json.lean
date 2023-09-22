/-
 Copyright (c) 2022 E.W.Ayers. All rights reserved.
 Released under Apache 2.0 license as described in the file LICENSE.
 Authors: E.W.Ayers, Wojciech Nawrocki
-/
import Lean.Data.Json
import Lean.Syntax

/-!
# JSON-like syntax for Lean.

Now you can write

```lean
open scoped ProofWidgets.Json

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

namespace ProofWidgets.Json
open Lean

declare_syntax_cat jso (behavior := symbol)

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
    if x == 0.0 then 0 else
    let s := toString (if x < 0.0 then - x else x)
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

scoped syntax "null" : jso
scoped syntax "true" : jso
scoped syntax "false" : jso
scoped syntax str : jso
scoped syntax "-"? num : jso
scoped syntax "-"? scientific : jso
scoped syntax "[" jso,* "]" : jso
syntax jsoIdent := ident <|> str
syntax jsoField := jsoIdent ": " jso
scoped syntax "{" jsoField,* "}" : jso
scoped syntax "json% " jso  : term

macro_rules
  | `(json% null)           => `(Lean.Json.null)
  | `(json% true)           => `(Lean.Json.bool Bool.true)
  | `(json% false)          => `(Lean.Json.bool Bool.false)
  | `(json% $n:str)         => `(Lean.Json.str $n)
  | `(json% $n:num)         => `(Lean.Json.num $n)
  | `(json% $n:scientific)  => `(Lean.Json.num $n)
  | `(json% -$n:num)        => `(Lean.Json.num (-$n))
  | `(json% -$n:scientific) => `(Lean.Json.num (-$n))
  | `(json% [$[$xs],*])     => `(Lean.Json.arr #[$[json% $xs],*])
  | `(json% {$[$ks:jsoIdent : $vs:jso],*}) => do
    let ks : Array (TSyntax `term) ← ks.mapM fun
      | `(jsoIdent| $k:ident) => pure (k.getId |> toString |> quote)
      | `(jsoIdent| $k:str)   => pure k
      | _                     => Macro.throwUnsupported
    `(Lean.Json.mkObj [$[($ks, json% $vs)],*])
  | `(json% $stx)           =>
    if stx.raw.isAntiquot then
      let stx := ⟨stx.raw.getAntiquotTerm⟩
      `(Lean.toJson $stx)
    else
      Macro.throwUnsupported

end ProofWidgets.Json
