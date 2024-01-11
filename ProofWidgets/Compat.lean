import Lean.Elab.InfoTree.Main

namespace ProofWidgets
open Lean Server Elab

abbrev LazyEncodable α := StateM RpcObjectStore α

-- back from exile
structure ExprWithCtx where
  ci : Elab.ContextInfo
  lctx : LocalContext
  expr : Expr
  deriving TypeName

def ExprWithCtx.runMetaM (e : ExprWithCtx) (x : Expr → MetaM α) : IO α :=
  e.ci.runMetaM e.lctx (x e.expr)

def ExprWithCtx.save (e : Expr) : MetaM ExprWithCtx :=
  return {
    ci := ← ContextInfo.save
    lctx := ← getLCtx
    expr := e
  }

end ProofWidgets
