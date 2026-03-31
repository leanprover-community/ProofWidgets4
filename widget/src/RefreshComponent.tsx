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
  const [html, setHtml] = React.useState<Html>({ text: '' })

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
      { abortSignal: ac.signal })
    // Repeatedly await updates to the display
    loop(0)
    return () => {
      cancelled = true
      ac.abort()
    }
  // Use the RPC reference ID as the only dep.
  // This gives Lean code control over when this component is reset.
  }, [RpcPtr.toKey(props.state)])
  return <HtmlDisplay html={html}/>
}
