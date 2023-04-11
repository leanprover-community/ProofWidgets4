import * as React from 'react'
import { RpcContext, RpcSessionAtPos, RpcPtr, Name, DocumentPosition, mapRpcError, useAsyncPersistent }
  from '@leanprover/infoview'
import HtmlDisplay, { Html } from './htmlDisplay'
import InteractiveExpr from './interactiveExpr'

type ExprWithCtx = RpcPtr<'ProofWidgets.ExprWithCtx'>

interface PresenterId {
  name: Name
  userName: string
}

async function applicableExprPresenters(rs: RpcSessionAtPos, expr: ExprWithCtx):
    Promise<PresenterId[]> {
  const ret: any = await rs.call('ProofWidgets.applicableExprPresenters', { expr })
  return ret.presenters
}

async function getExprPresentation(rs: RpcSessionAtPos, expr: ExprWithCtx, name: Name):
    Promise<Html> {
  return await rs.call('ProofWidgets.getExprPresentation', { expr, name })
}

interface ExprPresentationUsingProps {
  pos: DocumentPosition
  expr: ExprWithCtx
  name: Name
}

/** Display the given expression using the `ExprPresenter` registered at `name`. */
function ExprPresentationUsing({ pos, expr, name }: ExprPresentationUsingProps): JSX.Element {
  const rs = React.useContext(RpcContext)
  const st = useAsyncPersistent(() => getExprPresentation(rs, expr, name), [rs, expr, name])
  return st.state === 'resolved' ? <HtmlDisplay pos={pos} html={st.value} />
    : st.state === 'loading' ? <>Loading..</>
      : <>Error: {mapRpcError(st.error).message}</>
}

/** Display the given expression using an `ExprPresenter`. The server is queried for registered
 * `ExprPresenter`s. A dropdown is shown allowing the user to select which of these should be used
 * to display the expression. */
export default function ({ pos, expr }: { pos: DocumentPosition, expr: ExprWithCtx }): JSX.Element {
  const rs = React.useContext(RpcContext)
  const st = useAsyncPersistent(() => applicableExprPresenters(rs, expr), [rs, expr])
  const [selection, setSelection] = React.useState<Name | undefined>(undefined)

  if (st.state === 'rejected')
    return <>Error: {mapRpcError(st.error).message}</>
  else if (st.state === 'resolved' && 0 < st.value.length)
    // For explanation of flow-root see https://stackoverflow.com/a/32301823
    return <div style={{ display: 'flow-root' }}>
      {selection && selection !== 'none' ?
        <ExprPresentationUsing pos={pos} expr={expr} name={selection} /> :
        <InteractiveExpr expr={expr} />}
      <select className='fr' onChange={ev => setSelection(ev.target.value)}>
        <option key='none' value='none'>Default</option>
        {st.value.map(pid =>
          <option key={pid.name} value={pid.name}>{pid.userName}</option>)}
      </select>
    </div>
  else
    return <InteractiveExpr expr={expr} />
}
