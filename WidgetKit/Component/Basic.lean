import Lean.Widget.InteractiveCode
import WidgetKit.Compat

namespace WidgetKit

/-- A component is a widget module whose `default` export is

a [React component](https://reactjs.org/docs/components-and-props.html). The definition must be
annotated with `@[widget_module]` to be accessible from the infoview.

## Execution environment

The JS environment in which components execute provides a fixed set of libraries accessible via
direct `import`, notably `@leanprover/infoview`. All React contexts exported from
`@leanprover/infoview` should be usable from components.

## Lean encoding of props

`Props` is expected to have a `Lean.Server.RpcEncodable` instance. The export should then have type
`function(props: JsProps & { pos : DocumentPosition }): React.ReactNode` where `JsProps` is the JSON
encoding of `Props` and `DocumentPosition` is defined in `@leanprover/infoview`. -/
structure Component (Props : Type) extends Module

open Lean

structure InteractiveCodeProps where
  fmt : Widget.CodeWithInfos
  deriving Server.RpcEncodable

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

/-- Pretty-print and present a `Lean.Expr` as interactive text.

If you are producing a `Component`in a `MetaM` context where `Widget.ppExprTagged` can be used
directly, it is preferrable to use that and the `InteractiveCode` component instead. This avoids
one extra roundtrip to make the RPC call. -/
@[widget_module]
def InteractiveExpr : Component InteractiveExprProps where
  javascript := include_str ".." / ".." / "widget" / "dist" / "interactiveExpr.js"

/-- A panel widget is a component which can appear as a top-level panel in the infoview. A goal
state display is one example. -/
-- TODO: This is in `userWidget.tsx`
structure PanelWidgetProps where

end WidgetKit
