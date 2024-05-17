import * as React from 'react'
import { RpcContext, RpcSessionAtPos, RpcPtr, Name, DocumentPosition, mapRpcError,
  useAsyncPersistent } from '@leanprover/infoview'
import HtmlDisplay, { Html } from './htmlDisplay'
import InteractiveExpr from './interactiveExpr'

import { mathjax } from 'mathjax-full/js/mathjax'
import { TeX } from 'mathjax-full/js/input/tex'
import { CHTML } from 'mathjax-full/js/output/chtml'
import { SVG } from 'mathjax-full/js/output/svg'
import { AllPackages } from 'mathjax-full/js/input/tex/AllPackages'
import { liteAdaptor } from 'mathjax-full/js/adaptors/liteAdaptor'
import { RegisterHTMLHandler } from 'mathjax-full/js/handlers/html'

const adaptor = liteAdaptor()
RegisterHTMLHandler(adaptor)

const mathjax_document = mathjax.document('', {
  InputJax: new TeX({ packages: AllPackages }),
  OutputJax: new SVG({ fontCache: 'local' })
})

const mathjax_options = {
  em: 16,
  ex: 8,
  containerWidth: 1280
}

function get_mathjax_svg(math: string): string {
  const node = mathjax_document.convert(math, mathjax_options)
  return adaptor.outerHTML(node)
}

export function RenderLatex({content}: {content: string}): JSX.Element {
  // For explanation of flow-root see https://stackoverflow.com/a/32301823
  return <div dangerouslySetInnerHTML={{ __html: get_mathjax_svg(content) }} />}</div>
}

type ExprWithCtx = RpcPtr<'ProofWidgets.ExprWithCtx'>

interface ExprPresentationData {
  name: Name
  userName: string
  html: Html
}

async function getExprPresentations(rs: RpcSessionAtPos, expr: ExprWithCtx):
    Promise<ExprPresentationData[]> {
  const ret: any = await rs.call('ProofWidgets.getExprPresentations', { expr })
  return ret.presentations
}

/** Display the given expression using an `ExprPresenter`. The server is queried for registered
 * `ExprPresenter`s. A dropdown is shown allowing the user to select which of these should be used
 * to display the expression. */
export default function ({ pos, expr }: { pos: DocumentPosition, expr: ExprWithCtx }): JSX.Element {
  const rs = React.useContext(RpcContext)
  type Selection =
    { tag: 'auto' } |
    // Here `none` means use the default, that is `InteractiveExpr`.
    // We assume no presenter is registered under this name.
    { tag: 'manual', name: Name | 'none' }
  const [selection, setSelection] = React.useState<Selection>({ tag: 'auto' })
  const st = useAsyncPersistent<Map<Name, ExprPresentationData>>(async () => {
      const ret = await getExprPresentations(rs, expr)
      setSelection(s => {
        // If there was a manually selected presenter which no longer applies, reset to auto.
        if (s.tag === 'manual' && s.name !== undefined &&
            !ret.some(d => d.name === s.name))
          return { tag: 'auto' }
        return s
      })
      return new Map(ret.map(v => [v.name, v]))
    }, [rs, expr])

  if (st.state === 'rejected')
    return <>Error: {mapRpcError(st.error).message}</>
  else if (st.state === 'resolved') {
    console.log(selection, st.value)
    let selectionName: Name | 'none' = 'none'
    if (selection.tag === 'auto' && 0 < st.value.size)
      selectionName = Array.from(st.value.values())[0].name
    else if (selection.tag === 'manual' &&
        (selection.name === 'none' || st.value.has(selection.name)))
      selectionName = selection.name

    // For explanation of flow-root see https://stackoverflow.com/a/32301823
    return <div style={{ display: 'flow-root' }}>
      {selectionName !== 'none' &&
        <HtmlDisplay pos={pos} html={st.value.get(selectionName)!.html} />}
      {selectionName === 'none' &&
        <InteractiveExpr expr={expr} />}
      <select
          className='fr'
          value={selectionName}
          onChange={ev => {
            setSelection({ tag: 'manual', name: ev.target.value })
          }}
      >
        {Array.from(st.value.values(), pid =>
          <option key={pid.name} value={pid.name}>{pid.userName}</option>)}
        <option key='none' value='none'>Default</option>
      </select>
    </div>
  } else
    return <InteractiveExpr expr={expr} />
}
