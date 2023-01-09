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

export default function(props_: MetaWidgetProps): React.ReactNode {
  const {pos, infoId, ...props} = props_

  const rs = React.useContext(RpcContext)
  const st = useAsync(async () => {
    const ws: { widgets: PanelWidgetInstance[] } = await rs.call('WidgetKit.getPanelWidgets', {pos})
    const ret = []
    for (const w of ws.widgets) {
      if (w.infoId === infoId) {
        ret.push(w)
      }
    }
    return ret
  }, [rs, infoId, pos])

  const [st0, setSt0] = React.useState<JSX.Element | undefined>(undefined)
  React.useEffect(() => {
    if (st.state === 'resolved')
      setSt0(<>{
        st.value.map(w =>
          <DynamicComponent
            pos={pos}
            key={w.infoId}
            hash={w.srcHash}
            props={{ ...w.props, ...props, pos }} />)
      }</>)
  }, [st.state])

  if (st0 !== undefined)
    return st0
  else if (st.state === 'rejected')
    return `Error: ${mapRpcError(st.error).message}`
  else
    return 'Loading..'
}
