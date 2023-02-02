import { DocumentPosition } from "@leanprover/infoview"
import HtmlDisplay, { Html } from "./htmlDisplay"

export default function HtmlDisplayPanel({pos, html} : {pos: DocumentPosition, html: Html}):
    JSX.Element {
  return <details open>
    <summary className='mv2 pointer'>HTML Display</summary>
      <HtmlDisplay pos={pos} html={html} />
    </details>
}
