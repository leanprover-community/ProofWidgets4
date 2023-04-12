import ProofWidgets.Presentation.Expr
import ProofWidgets.Component.SelectionPanel

/-! ## References:
- Chris Okasaki. "Functional Pearls: Red-Black Trees in a Functional Setting". 1993 -/

inductive RBColour where
  | red | black

inductive RBTree (α : Type u) where
  | empty : RBTree α
  | node (color : RBColour) (l : RBTree α) (a : α) (r : RBTree α) : RBTree α

namespace RBTree

def contains [Ord α] (a : α) : RBTree α → Bool
  | empty => false
  | node _ l b r => match compare a b with
    | .lt => l.contains a
    | .eq => true
    | .gt => r.contains a

def balance : RBColour → RBTree α → α → RBTree α → RBTree α
  | .black, (node .red (node .red a x b) y c), z, d
  | .black, (node .red a x (node .red b y c)), z, d
  | .black, a, x, (node .red (node .red b y c) z d)
  | .black, a, x, (node .red b y (node .red c z d)) =>
    node .red (node .black a x b) y (node .black c z d)
  | color, a, x, b => node color a x b

def insert [Ord α] (a : α) (s : RBTree α) : RBTree α :=
  makeBlack (ins s)
where
  ins : RBTree α → RBTree α
    | empty => node .red empty a empty
    | node c l b r => match compare a b with
      | .lt => balance c (ins l) b r
      | .eq => node c l b r
      | .gt => balance c l b (ins r)
  makeBlack : RBTree α → RBTree α
    | empty => empty
    | node _ l b r => node .black l b r

end RBTree

/-! # Metaprogramming utilities for red-black trees -/

open Lean

def empty? (e : Expr) : Bool :=
  e.app1? ``RBTree.empty matches some _

@[inline] def Lean.Expr.app5? (e : Expr) (fName : Name) : Option (Expr × Expr × Expr × Expr × Expr) :=
  if e.isAppOfArity fName 5 then
    some (
      e.appFn!.appFn!.appFn!.appFn!.appArg!,
      e.appFn!.appFn!.appFn!.appArg!,
      e.appFn!.appFn!.appArg!,
      e.appFn!.appArg!,
      e.appArg!)
  else
    none

def node? (e : Expr) : Option (Expr × Expr × Expr × Expr) := do
  let some (_, color, l, a, r) := e.app5? ``RBTree.node | none
  return (color, l, a, r)

unsafe def evalColourUnsafe (e : Expr) : MetaM RBColour :=
  Lean.Meta.evalExpr' RBColour ``RBColour e

@[implemented_by evalColourUnsafe]
opaque evalColour (e : Expr) : MetaM RBColour

/-- Like `RBTreeColour`, but with `blue` standing in for unknown, symbolic `c : RBTreeColour`. -/
inductive RBTreeVarsColour where
  | red | black | blue
  deriving FromJson, ToJson

open Widget in
/-- Like `RBTree` but with concrete node contents replaced by quoted, pretty-printed code,
and an extra constructor for similarly pretty-printed symbolic subtrees.

Tangent: what is the transformation of polynomial functors from the original type to
one with this kind of symbolic data? -/
inductive RBTreeVars where
  | empty : RBTreeVars
  | var : CodeWithInfos → RBTreeVars
  | node (color : RBTreeVarsColour) (l : RBTreeVars) (a : CodeWithInfos) (r : RBTreeVars) : RBTreeVars

#mkrpcenc RBTreeVars

/-! # `Expr` presenter to display red-black trees -/

structure RBDisplayProps where
  tree : RBTreeVars

#mkrpcenc RBDisplayProps

open ProofWidgets

@[widget_module]
def RBDisplay : Component RBDisplayProps where
  javascript := include_str ".." / ".." / "build" / "js" / "rbTree.js"

open scoped Jsx in
partial def drawTree? (e : Expr) : MetaM (Option Html) := do
  if let some _ := node? e then
    return some <| .ofTHtml <RBDisplay tree={← go e}/>
  else if empty? e then
    return some <| .ofTHtml <RBDisplay tree={← go e}/>
  else
    return none
where go (e : Expr) : MetaM RBTreeVars := do
  if let some (color, l, a, r) := node? e then
    let color ← try
        match ← evalColour color with
        | .red => pure .red
        | .black => pure .black
      catch _ => pure .blue
    return .node color (← go l) (← Widget.ppExprTagged a) (← go r)
  else if empty? e then
    return .empty
  else
    return .var (← Widget.ppExprTagged e)

@[expr_presenter]
def RBTree.presenter : ExprPresenter where
  userName := "Red-black tree"
  present e := do
    let some t ← drawTree? e
      | throwError "not a tree :("
    return t

/-! # Example -/

open RBTree RBColour in
example {α : Type} (x y z : α) (a b c d : RBTree α)
    (h : ¬ ∃ e w f, a = node red e w f) :
    balance black (node red a x (node red b y c)) z d =
    node red (node black a x b) y (node black c z d) := by
  withPanelWidgets [SelectionPanel]
    match a with
    | .empty => simp [balance]
    | node black .. => simp [balance]
    | node red .. =>
      conv => unfold balance; simp_match
      exact False.elim <| h ⟨_, _, _, rfl⟩
