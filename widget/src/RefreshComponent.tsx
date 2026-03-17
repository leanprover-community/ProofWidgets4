/*
Copyright (c) 2025 Jovan Gerbscheid. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jovan Gerbscheid
*/
import React from 'react'
import { useRpcSession, RpcPtr } from '@leanprover/infoview'
import HtmlDisplay, { Html } from './htmlDisplay'

interface RefreshComponentProps {
  state : RpcPtr<'UpdatableHtml'>
  cancelTk : RpcPtr<'IO.CancelToken'>
}

export default function RefreshComponent(props: RefreshComponentProps): JSX.Element {
  const rs = useRpcSession()
  const [html, setHtml] = React.useState<Html | null>(null)
  const st = React.useRef<RpcPtr<'UpdatableHtml'>>(props.state)

  React.useEffect(() => {
    let cancelled = false
    async function loop() {
      const result = await rs.call<RpcPtr<'UpdatableHtml'>, [Html, RpcPtr<'UpdatableHtml'>] | null>(
        'ProofWidgets.RefreshComponent.awaitNextHtml', st.current)
      if (cancelled || !result) return
      st.current = result[1]
      setHtml(result[0])
      return loop()
    }
    const ac = new AbortController()
    rs.call<RefreshComponentProps, null>('ProofWidgets.RefreshComponent.monitor', props,
      { autoCancel: true, abortSignal: ac.signal });
    (async () => {
      // Display the HTML tree provided in the initial props
      const result = await rs.call<RpcPtr<'UpdatableHtml'>, Html>(
        'ProofWidgets.RefreshComponent.getCurrHtml', props.state)
      if (cancelled) return
      st.current = props.state
      setHtml(result)
      // Then repeatedly await updates to the display
      return loop()
    })()
    return () => {
      cancelled = true
      ac.abort()
    }
  // Use the RPC reference ID as the only dep.
  // This gives Lean code control over when this component is reset.
  }, [RpcPtr.toKey(props.state)])
  // For the short time that `html` doesn't have a value yet, we return the empty HTML.
  return html ? <HtmlDisplay html={html}/> : <></>
}
