import * as React from 'react'
import { mapRpcError, useAsync, DynamicComponent, PanelWidgetProps, RpcContext }
  from '@leanprover/infoview'
import { Range } from 'vscode-languageserver-protocol'

interface MetaWidgetProps extends PanelWidgetProps {
  infoId: string
}

interface PanelWidgetInstance {
  id: string
  srcHash: string
  props: any
  infoId: string
  range?: Range
}

function MetaWidget (props_: MetaWidgetProps): JSX.Element {
  const { pos, infoId, ...props } = props_

  const rs = React.useContext(RpcContext)
  const st = useAsync(async () => {
    const ws: { widgets: PanelWidgetInstance[] } = await rs.call('ProofWidgets.getPanelWidgets', { pos })
    const ret = []
    for (const w of ws.widgets) {
      if (w.infoId === infoId) {
        ret.push(w)
      }
    }
    return ret
  }, [rs, infoId, pos])

  // Hide the '▶ Widget Name' element
  const ref = React.useRef<HTMLDivElement>(null)
  React.useLayoutEffect(() => {
    if (ref.current === null) return
    const parent = ref.current.parentElement
    if (parent === null) return
    for (const c of parent.children) {
      if (c.tagName !== 'SUMMARY') continue
      if (!(c instanceof HTMLElement)) continue
      c.style.display = 'none'
      break
    }
  }, [])

  const [st0, setSt0] = React.useState<PanelWidgetInstance[] | undefined>(undefined)
  React.useEffect(() => {
    if (st.state === 'resolved')
      setSt0(st.value)
  }, [st.state])

  let inner = <></>
  if (st0 !== undefined) {
    // If every widget panel uses a unique hash, use the hash as the key so that state persists
    // when moving the cursor.
    const seenHashes = new Set<string>()
    const hashKeysOkay = st0.every(w => {
      if (seenHashes.has(w.srcHash)) return false
      seenHashes.add(w.srcHash)
      return true
    })
    inner = <>
      {st0.map(w =>
        <DynamicComponent
          pos={pos}
          key={hashKeysOkay ? w.srcHash : w.infoId}
          hash={w.srcHash}
          props={{ ...w.props, ...props, pos }} />)}
    </>
  } else if (st.state === 'rejected')
    inner = <>Error: {mapRpcError(st.error).message}</>
  else
    inner = <>Loading..</>

  return <div ref={ref}>{inner}</div>
}

export default MetaWidget
