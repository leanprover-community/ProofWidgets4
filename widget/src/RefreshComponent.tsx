/*
Copyright (c) 2025 Jovan Gerbscheid. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jovan Gerbscheid
*/
import React from 'react'
import { useRpcSession, RpcPtr } from '@leanprover/infoview'
import HtmlDisplay, { Html } from './htmlDisplay'

interface VersionedHtml {
  html : Html
  idx : number
}

interface AwaitRefreshParams {
  state : RpcPtr<'RefreshRef'>
  oldIdx : number
}

interface RefreshComponentProps {
  state : RpcPtr<'RefreshRef'>
  cancelTk : RpcPtr<'IO.CancelToken'>
}

export default function RefreshComponent(props: RefreshComponentProps): JSX.Element {
  const rs = useRpcSession()
  const [html, setHtml] = React.useState<Html | null>(null)

  React.useEffect(() => {
    let cancelled = false
    async function loop(idx: number) {
      const result = await rs.call<AwaitRefreshParams, VersionedHtml | null>(
        'ProofWidgets.RefreshComponent.awaitRefresh', { oldIdx: idx, state: props.state })
      if (cancelled || !result) return
      setHtml(result.html)
      return loop(result.idx)
    }
    const ac = new AbortController()
    rs.call<RefreshComponentProps, null>('ProofWidgets.RefreshComponent.monitor', props,
      { autoCancel: true, abortSignal: ac.signal });
    (async () => {
      // Display the HTML tree provided in the initial props
      const result = await rs.call<RpcPtr<'RefreshRef'>, VersionedHtml>(
        'ProofWidgets.RefreshComponent.getCurrHtml', props.state)
      if (cancelled) return
      setHtml(result.html)
      // Then repeatedly await updates to the display
      return loop(result.idx)
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
