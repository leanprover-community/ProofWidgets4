open Lean

namespace ProofWidgets.Util

/-- Sends `#[a, b, c]` to `` `(term| $a ++ $b ++ $c)``-/
def joinArrays {m} [Monad m] [MonadRef m] [MonadQuotation m] (arr : Array Term) : m Term := do
  if h : 0 < arr.size then
    arr.foldlM (fun x xs => `($x ++ $xs)) arr[0] (start := 1)
  else
    `(#[])

/-- Collapse adjacent `inl (_ : α)`s into a `β` using `f`.
For example, `#[.inl a₁, .inl a₂, .inr b, .inl a₃] ↦ #[← f #[a₁, a₂], b, ← f #[a₃]]`. -/
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

end ProofWidgets.Util
