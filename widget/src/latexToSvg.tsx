import { RenderLatexSvg } from "./exprPresentation";

export default function ({content}: {content: string}) {
  return <RenderLatexSvg content={content} />
}
