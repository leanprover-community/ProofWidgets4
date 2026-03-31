import * as React from 'react'
import isEqual from 'react-fast-compare'
import { DocumentPosition, mapRpcError, useAsyncPersistent, useRpcSession } from '@leanprover/infoview'
import { Html, default as HtmlDisplay } from './htmlDisplay'
import { Fn, callCancellable } from './cancellable'

declare type Props = any & { pos : DocumentPosition };
// The string template is replaced in OfRpcMethod.lean.
const RPC_METHOD = '$RPC_METHOD'
// HACK: TS runs dead-code elimination
// and eliminates the `callCancellable` branch if we use a string literal,
// so use a runtime value.
const RPC_CANCELLABLE = window.toString()

export default React.memo((props: Props) => {
  const rs = useRpcSession()
  const cancelRef = React.useRef<Fn>({ fn: () => {} })
  const st = useAsyncPersistent<Html>(async () => {
    cancelRef.current.fn()
    if (RPC_CANCELLABLE === 'true') {
      // TODO: Remove this branch when removing `ProofWidgets.Cancellable`
      const [res, cancel] = callCancellable<Props, Html>(rs, RPC_METHOD, props)
      cancelRef.current = cancel
      return res
    } else {
      const ac = new AbortController()
      const res = rs.call<Props, Html>(RPC_METHOD, props,
        { abortSignal: ac.signal })
      cancelRef.current = { fn: () => ac.abort() }
      return res
    }
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
      <HtmlDisplay html={st.value} />
},
/*
 * HACK:
 * The Lean-side API for writing components is quite impoverished:
 * an RPC method used with `mk_rpc_widget%` cannot register React effects
 * (or anything that behaves similarly; see issue #9).
 *
 * There is thus no way to react to changes in props
 * without manually managing component state.
 * Many components written by users thus directly launch effects
 * in the body of the RPC method.
 * This used to happen on every render,
 * because the identity of the `props` object would change essentially every time.
 * We try to mitigate this excessive re-running to some extent
 * by deeply comparing the props here.
 * Note that RPC refs are still compared shallowly,
 * in the sense that their data (stored server-side) is not inspected.
 */
isEqual)
