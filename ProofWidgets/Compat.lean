import Lean.Elab.InfoTree.Main

namespace ProofWidgets
open Lean Server Elab

abbrev LazyEncodable α := StateM RpcObjectStore α

-- back from exile
structure ExprWithCtx where
  ci : Elab.ContextInfo
  lctx : LocalContext
  linsts : LocalInstances
  expr : Expr
  deriving TypeName

def ExprWithCtx.runMetaM (e : ExprWithCtx) (x : Expr → MetaM α) : IO α :=
  e.ci.runMetaM {} $
    Meta.withLCtx e.lctx e.linsts (x e.expr)

def ExprWithCtx.save (e : Expr) : MetaM ExprWithCtx :=
  return {
    ci := { ← CommandContextInfo.save with }
    lctx := ← getLCtx
    linsts := ← Meta.getLocalInstances
    expr := e
  }

end ProofWidgets
