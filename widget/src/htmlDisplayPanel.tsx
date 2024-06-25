import { DocumentPosition } from "@leanprover/infoview"
import HtmlDisplay, { Html } from "./htmlDisplay"

export default function HtmlDisplayPanel({html} : {pos: DocumentPosition, html: Html}):
    JSX.Element {
  return <details open>
    <summary className='mv2 pointer'>HTML Display</summary>
      <HtmlDisplay html={html} />
    </details>
}
