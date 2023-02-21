import Lean.Widget.InteractiveCode
import ProofWidgets.Compat

namespace ProofWidgets

/-- A component is a widget module whose `default` export is a
[React component](https://reactjs.org/docs/components-and-props.html). The definition must be
annotated with `@[widget_module]` to be accessible from the infoview.

## Execution environment

The JS environment in which components execute provides a fixed set of libraries accessible via
direct `import`, notably `@leanprover/infoview`. All React contexts exported from
`@leanprover/infoview` are usable from components.

## Lean encoding of props

`Props` is expected to have a `Lean.Server.RpcEncodable` instance. The JS export should then have
type `function(props: JsProps & { pos : DocumentPosition }): React.ReactNode` where `JsProps` is the
JSON encoding of `Props` and `DocumentPosition` is defined in `@leanprover/infoview`. -/
structure Component (Props : Type) extends Module

open Lean

structure InteractiveCodeProps where
  fmt : Widget.CodeWithInfos

#mkrpcenc InteractiveCodeProps

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

#mkrpcenc InteractiveExprProps

@[server_rpc_method]
def ppExprTagged : InteractiveExprProps → Server.RequestM (Server.RequestTask Widget.CodeWithInfos)
  | ⟨⟨expr⟩⟩ => Server.RequestM.asTask <| expr.runMetaM Widget.ppExprTagged

/-- Pretty-print and present a `Lean.Expr` as interactive text.

If you are producing a `Component`in a `MetaM` context where `Widget.ppExprTagged` can be used
directly, it is preferrable to use that and the `InteractiveCode` component instead. This avoids
one extra roundtrip to make the RPC call. -/
@[widget_module]
def InteractiveExpr : Component InteractiveExprProps where
  javascript := include_str ".." / ".." / "build" / "js" / "interactiveExpr.js"

/-- These are the props passed to a panel widget, i.e. a component which can appear as a top-level
panel in the infoview. For example, a goal state display. See `savePanelWidgetInfo`. -/
-- TODO: This contains the fields described in `userWidget.tsx`
structure PanelWidgetProps where

end ProofWidgets
