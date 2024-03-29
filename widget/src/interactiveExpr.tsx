import { mapRpcError, InteractiveCode, RpcContext, RpcPtr, useAsyncPersistent }
  from '@leanprover/infoview'
import * as React from 'react'

type ExprWithCtx = RpcPtr<'ProofWidgets.ExprWithCtx'>

export default function ({ expr }: { expr: ExprWithCtx }): JSX.Element {
  const rs = React.useContext(RpcContext)
  const st = useAsyncPersistent(() => rs.call('ProofWidgets.ppExprTagged', { expr }), [expr])
  if (st.state === 'resolved')
    return <InteractiveCode fmt={st.value as any} />
  else if (st.state === 'rejected')
    return <>Error: ${mapRpcError(st.error).message}</>
  else
    return <>Loading..</>
}
