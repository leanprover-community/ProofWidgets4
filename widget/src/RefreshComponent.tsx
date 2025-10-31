/*
Copyright (c) 2025 Jovan Gerbscheid. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Jovan Gerbscheid
*/
import React from "react";
import { useRpcSession, RpcPtr } from "@leanprover/infoview";
import HtmlDisplay, { Html } from './htmlDisplay';

/** The arguments passed to a `RefreshComponent` */
interface RefreshComponentProps {
  /** The initial HTML to display */
  initial: Html
  /** A Lean task for iteratively refreshing the HTML display */
  refresh: RpcPtr<'RefreshTask'>
  /** A cancel token that will cancel the `refresh` computation */
  cancelTk? : RpcPtr<'IO.CancelToken'>
}

interface RefreshResultProps {
  /** The new HTML to display */
  html? : Html
  /** A Lean task for continuing to refreshing the HTML display */
  refresh? : RpcPtr<'RefreshTask'>
}

export default function RefreshComponent(props: RefreshComponentProps): JSX.Element {
  const rs = useRpcSession()
  const [html, setHtml] = React.useState<Html>(props.initial)

  // Repeatedly call Lean to update
  React.useEffect(() => {
    // Set the html to the initial value at the start of each re-render
    setHtml(props.initial)
    let cancelled = false
    async function loop(refresh: RpcPtr<'RefreshTask'>) {
        const result = await rs.call<RpcPtr<'RefreshTask'>, RefreshResultProps> ("ProofWidgets.awaitRefresh", refresh)
        if (cancelled) return
        if (result.html) {
            setHtml(result.html)
            if (result.refresh) loop(result.refresh)
        }
    }
    loop(props.refresh)
    return () => {
        cancelled = true
        if (props.cancelTk) {
            rs.call<RpcPtr<'IO.CancelToken'>, void> ("ProofWidgets.cancelRefresh", props.cancelTk)
        }
    }
  }, [props])
  return <HtmlDisplay html={html}/>
}
