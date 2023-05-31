import { PanelWidgetProps } from "@leanprover/infoview";
import { GoalsLocationPresentation } from "./presentSelection";

export default function (props: PanelWidgetProps) {
  if (props.goals.length === 0) return <></>
  const g = props.goals[0]
  if (!g.mvarId)
    throw new Error(`Lean server 1.1.2 or newer is required.`)
  return <details open>
    <summary className='mv2 pointer'>Main goal type</summary>
    <GoalsLocationPresentation
      pos={props.pos}
      goals={props.goals}
      loc={{ mvarId: g.mvarId, loc: { target: '/' }}} />
  </details>
}
