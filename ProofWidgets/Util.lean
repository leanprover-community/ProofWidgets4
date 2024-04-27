open Lean

namespace ProofWidgets.Util

/-- Sends `#[a, b, c]` to `` `(term| $a ++ $b ++ $c)``-/
def joinArrays {m} [Monad m] [MonadRef m] [MonadQuotation m] (arr : Array Term) : m Term := do
  if h : 0 < arr.size then
    arr.foldlM (fun x xs => `($x ++ $xs)) arr[0] (start := 1)
  else
    `(#[])

/-- Collapse adjacent `inl (_ : α)`s into a `β` using `f`. -/
def foldInlsM {m} [Monad m] (arr : Array (α ⊕ β)) (f : Array α → m β) : m (Array β) := do
  let mut ret : Array β := #[]
  let mut pending_inls : Array α := #[]
  for c in arr do
    match c with
    | .inl ci =>
      pending_inls := pending_inls.push ci
    | .inr cis =>
      if pending_inls.size ≠ 0 then
        ret := ret.push <| ← f pending_inls
      pending_inls := #[]
      ret := ret.push cis
  if pending_inls.size ≠ 0 then
    ret := ret.push <| ← f pending_inls
  return ret

open Lean Delaborator

/-- Delaborate the elements of a list literal separately, calling `elem` on each. -/
partial def delabListLiteral {α} (elem : DelabM α) : DelabM (Array α) :=
  go #[]
where
  go (acc : Array α) : DelabM (Array α) := do
    match_expr ← getExpr with
    | List.nil _ => return acc
    | List.cons _ _ _ =>
      let hd ← withNaryArg 1 elem
      withNaryArg 2 $ go (acc.push hd)
    | _ => failure

/-- Delaborate the elements of an array literal separately, calling `elem` on each. -/
partial def delabArrayLiteral {α} (elem : DelabM α) : DelabM (Array α) := do
  match_expr ← getExpr with
  | List.toArray _ _ => withNaryArg 1 <| delabListLiteral elem
  | _ => failure

/-- A copy of `Delaborator.annotateTermInfo` for other syntactic categories. -/
def annotateTermLikeInfo (stx : TSyntax n) : DelabM (TSyntax n) := do
  let stx ← annotateCurPos ⟨stx⟩
  addTermInfo (← getPos) stx (← getExpr)
  pure ⟨stx⟩

/-- A copy of `Delaborator.withAnnotateTermInfo` for other syntactic categories. -/
def withAnnotateTermLikeInfo (d : DelabM (TSyntax n)) : DelabM (TSyntax n) := do
  let stx ← d
  annotateTermLikeInfo stx


end ProofWidgets.Util
