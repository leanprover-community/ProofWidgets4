/-
 Copyright (c) 2022 E.W.Ayers. All rights reserved.
 Released under Apache 2.0 license as described in the file LICENSE.
 Authors: E.W.Ayers
-/
import Lean.Data.Json

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

scoped syntax "[" jso,* "]" : jso
scoped syntax "-"? scientific : jso
scoped syntax "-"? num : jso
scoped syntax str : jso
scoped syntax "true" : jso
scoped syntax "false" : jso
scoped syntax "null" : jso
scoped syntax ident : jso_ident
scoped syntax "$(" term ")" : jso_ident
scoped syntax str : jso_ident
scoped syntax jso_ident ": " jso : jso_field
scoped syntax "{" jso_field,* "}" : jso
scoped syntax "$(" term ")" : jso
scoped syntax "json% " jso  : term

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

end ProofWidgets.Json
