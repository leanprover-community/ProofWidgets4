import * as React from 'react'
import { DocumentPosition, RpcContext, mapRpcError, useAsyncPersistent } from '@leanprover/infoview'
import { Html, default as HtmlDisplay } from './htmlDisplay'
import { Fn, callCancellable } from './cancellable'

declare type Props = any & { pos : DocumentPosition };
// The string template is replaced in OfRpcMethod.lean.
const RPC_METHOD = '$RPC_METHOD'
// HACK: TS runs dead-code elimination
// and eliminates the `callCancellable` branch if we use a string literal,
// so use a runtime value.
const RPC_CANCELLABLE = window.toString()

export default function(props: Props) {
  const rs = React.useContext(RpcContext)
  const cancelRef = React.useRef<Fn>({ fn: () => {} })
  const st = useAsyncPersistent<Html>(async () => {
    if (RPC_CANCELLABLE === 'true') {
      cancelRef.current.fn()
      const [res, cancel] = callCancellable<Props, Html>(rs, RPC_METHOD, props)
      cancelRef.current = cancel
      return res
    } else
      return rs.call<Props, Html>(RPC_METHOD, props)
  }, [rs, props])

  // This effect runs once on startup, does nothing,
  // and sets up a cleanup function that runs when the component is unmounted.
  React.useEffect(() => {
    return () => { cancelRef.current.fn() }
  }, [])

  return st.state === 'rejected' ?
      <p style={{color: 'red'}}>{mapRpcError(st.error).message}</p>
    : st.state === 'loading' ?
      <>Loading..</>
    :
      <HtmlDisplay pos={props.pos} html={st.value} />
}
