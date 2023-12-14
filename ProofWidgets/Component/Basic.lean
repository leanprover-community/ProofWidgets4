import Lean.Widget.UserWidget
import ProofWidgets.Compat

namespace ProofWidgets
open Lean

structure DocumentPosition extends Lsp.Position where
  uri : Lsp.DocumentUri
  deriving ToJson, FromJson

/-- Every widget instance receives the position in the Lean source file
at which the cursor is currently located. -/
structure PositionProps where
  pos : DocumentPosition
  deriving ToJson, FromJson

/-- The props of a component that expects none. -/
structure NoProps where
  deriving ToJson, FromJson

/-- A component is a widget module
that exports a [React component](https://react.dev/learn/your-first-component).

Every component definition must either be annotated with `@[widget_module]`,
or use a value of `javascript` identical to that of another definition
annotated with `@[widget_module]`.
This makes it possible for the infoview to load the component.

## Lean encoding of props

In general, the [React props](https://react.dev/learn/passing-props-to-a-component)
received by a widget component are split into two parts:
data passed to the component by the infoview (`IProps`),
and data passed to the component from Lean (`LProps`).

The types `LProps` and `IProps` are both expected
to have `Lean.Server.RpcEncodable` instances.

Write `TS(α)` for the TypeScript type describing
the JSON encoding of `α` as per its `RpcEncodable` instance.
The export of the module specified in `«export»`
should then be a function `(props: TS(LProps) & TS(IProps)) => React.ReactNode`.
Note that by defining a `Component L I`,
you are asserting that this is indeed the type
of its JS implementation.

Note that it is possible to set `IProps` from Lean
by also putting the fields in `LProps`: they will take precedence. -/
structure Component (LProps : Type) (IProps : Type := PositionProps) extends Widget.Module where
  /-- Which export of the module to use as the component function. -/
  «export» : String := "default"

instance : Widget.ToModule (Component LProps IProps) := ⟨Component.toModule⟩

structure InteractiveCodeProps where
  fmt : Widget.CodeWithInfos
  deriving Server.RpcEncodable

/-- Present pretty-printed code as interactive text.

The most common use case is to instantiate this component from a `Lean.Expr`. To do so, you must
eagerly pretty-print the `Expr` using `Widget.ppExprTagged`. See also `InteractiveExpr`. -/
@[widget_module]
def InteractiveCode : Component InteractiveCodeProps where
  javascript := "
    import { InteractiveCode } from '@leanprover/infoview'
    import * as React from 'react'
    export default function(props) {
      return React.createElement(InteractiveCode, props)
    }"

structure InteractiveExprProps where
  expr : Server.WithRpcRef ExprWithCtx
  deriving Server.RpcEncodable

@[server_rpc_method]
def ppExprTagged : InteractiveExprProps → Server.RequestM (Server.RequestTask Widget.CodeWithInfos)
  | ⟨⟨expr⟩⟩ => Server.RequestM.asTask <| expr.runMetaM Widget.ppExprTagged

/-- Lazily pretty-print and present a `Lean.Expr` as interactive text.

This component is preferrable over `InteractiveCode` when the `Expr` will not necessarily be
displayed in the UI (e.g. it may be hidden by default), in which case laziness saves some work.
On the other hand if the `Expr` will likely be shown and you are in a `MetaM` context, it is
preferrable to use the eager `InteractiveCode` in order to avoid the extra client-server roundtrip
needed for the pretty-printing RPC call. -/
@[widget_module]
def InteractiveExpr : Component InteractiveExprProps where
  javascript := include_str ".." / ".." / ".lake" / "build" / "js" / "interactiveExpr.js"

end ProofWidgets
