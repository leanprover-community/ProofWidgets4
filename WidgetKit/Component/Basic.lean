import Lean.Widget.InteractiveCode
import WidgetKit.Component.Compat

namespace WidgetKit

/-- A widget module whose `default` export is a
[React component](https://reactjs.org/docs/components-and-props.html). It must be annotated with
`@[widget_module]` to work.

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
  javascript := "
    import { mapRpcError, useAsync, InteractiveCode, RpcContext } from '@leanprover/infoview'
    import * as React from 'react'
    const e = React.createElement
    export default function(props) {
      const rs = React.useContext(RpcContext)
      const st = useAsync(() => rs.call('WidgetKit.ppExprTagged', props), [props.expr])
      if (st.state === 'resolved')
        return e(InteractiveCode, {fmt: st.value})
      else if (st.state === 'rejected')
        return `Error: ${mapRpcError(st.error).message}`
      else
        return 'Loading..'
    }"

-- TODO: This is in `userWidget.tsx`
structure PanelWidgetProps where

end WidgetKit
