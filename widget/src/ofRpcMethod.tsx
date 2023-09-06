import * as React from 'react'
import { DocumentPosition, RpcContext, mapRpcError, useAsyncPersistent } from '@leanprover/infoview'
import { Html, default as HtmlDisplay } from './htmlDisplay'

declare type Props = any & { pos : DocumentPosition };
// The string template is replaced in OfRpcMethod.lean
const RPC_METHOD = '$RPC_METHOD'

export default function(props: Props) {
  const rs = React.useContext(RpcContext)
  const st  = useAsyncPersistent<Html>(async () => {
    return await rs.call(RPC_METHOD, props)
  }, [rs, props])

  return st.state === 'rejected' ?
      <p style={{color: 'red'}}>{mapRpcError(st.error).message}</p>
    : st.state === 'loading' ?
      <>Loading..</>
    :
      <HtmlDisplay pos={props.pos} html={st.value} />
}
