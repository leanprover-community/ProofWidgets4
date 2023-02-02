import { mapRpcError, useAsync, InteractiveCode, RpcContext, RpcPtr } from '@leanprover/infoview'
import * as React from 'react'

type ExprWithCtx = RpcPtr<'WidgetKit.ExprWithCtx'>

export default function({expr}: {expr: ExprWithCtx}): JSX.Element {
  const rs = React.useContext(RpcContext)
  const st = useAsync(() => rs.call('WidgetKit.ppExprTagged', {expr}), [expr])
  if (st.state === 'resolved')
    return <InteractiveCode fmt={st.value as any} />
  else if (st.state === 'rejected')
    return <>Error: ${mapRpcError(st.error).message}</>
  else
    return <>Loading..</>
}
